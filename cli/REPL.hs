module REPL where

import Control.Monad.Except
import Control.Monad.Trans
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.Terminal (renderIO)
import qualified Specstrom.Analysis as Analysis
import qualified Specstrom.Evaluator as Evaluator
import qualified Specstrom.Parser as Parser
import Specstrom.PrettyPrinter
import Specstrom.Syntax as Syntax
import qualified Specstrom.TypeInf as TypeInf
import System.Console.Haskeline
import System.Console.Haskeline.MonadException
import System.IO (hPutStrLn, stderr, stdout)
import Util

repl :: Text -> IO ()
repl fp = do
  (tls, tbl, g) <- load' False fp
  (e, e', ae) <- liftIO $ checkTopLevels Evaluator.basicEnv mempty Analysis.builtIns tls
  runInputT defaultSettings (loop defaultOpts tbl g e e' mempty ae)

defaultOpts :: Options
defaultOpts = Opts False False

data Options = Opts {showTypes :: Bool, showAnalysis :: Bool}

loop ::
  Options ->
  Parser.Table ->
  TypeInf.Context ->
  Evaluator.Env ->
  Evaluator.Env -> -- action bodies
  Evaluator.State ->
  Analysis.AnalysisEnv ->
  InputT IO ()
loop opts tbl g e e' s ae = flip catch (\(Evaluator.Error er) -> outputStrLn er >> loop opts tbl g e e' s ae) $ do
  str <- getInputLine "> "
  case str of
    Nothing -> pure ()
    Just (':' : rest)
      | rest == "set types" -> loop (opts {showTypes = True}) tbl g e e' s ae
      | rest == "set analysis" -> loop (opts {showAnalysis = True}) tbl g e e' s ae
      | rest == "unset analysis" -> loop (opts {showAnalysis = False}) tbl g e e' s ae
      | rest == "unset types" -> loop (opts {showTypes = False}) tbl g e e' s ae
      | rest == "quit" -> pure ()
      | rest == "debug" -> liftIO (print g) >> loop opts tbl g e e' s ae
      | rest == "state" -> pure () -- for now
      | otherwise -> liftIO $ hPutStrLn stderr "Invalid meta-command"
    Just x -> do
      r <- liftIO $ runExceptT (Parser.loadImmediate searchPaths tbl $ pack x)
      case r of
        Left _ -> case Parser.immediateExpr tbl (pack x) of
          Left err -> do
            liftIO $ renderIO stderr (layoutPretty defaultLayoutOptions (prettyParseError err <> line))
            loop opts tbl g e e' s ae
          Right expr -> case TypeInf.inferExpImmediate g expr of
            Left err -> do
              liftIO $ renderIO stderr (layoutPretty defaultLayoutOptions (prettyTypeError err <> line))
              loop opts tbl g e e' s ae
            Right typ -> do
              if showTypes opts
                then liftIO $ renderIO stdout (layoutPretty defaultLayoutOptions (prettyType typ <> line))
                else return ()
              if showAnalysis opts
                then liftIO $ LBS.putStrLn (JSON.encode (Analysis.depOf (Analysis.analyseExpr ae expr)))
                else return ()
              val <- liftIO $ (Evaluator.force s =<< Evaluator.evaluate s e expr)
              liftIO $ renderIO stdout (layoutPretty defaultLayoutOptions (prettyValue val <> line))
              loop opts tbl g e e' s ae
        Right (tbl2, tls) -> do
          case TypeInf.inferTopLevels g tls of
            Left err -> do
              liftIO $ renderIO stderr (layoutPretty defaultLayoutOptions (prettyTypeError err <> line))
              loop opts tbl g e e' s ae
            Right g2 -> do
              (e2, e'2, ae2) <- liftIO $ checkTopLevels e e' ae tls
              loop opts tbl2 g2 e2 e'2 s ae2

checkTopLevel ::
  Evaluator.Env ->
  Evaluator.Env -> -- action bodies
  Analysis.AnalysisEnv ->
  TopLevel ->
  IO (Evaluator.Env, Evaluator.Env, Analysis.AnalysisEnv)
checkTopLevel evalEnv actionEnv analysisEnv tl = case tl of
  Binding b@(Syntax.Bind pat _) -> do
    evalEnv' <- Evaluator.evaluateBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (evalEnv', actionEnv, analysisEnv')
  ActionDecl b@(Syntax.Bind pat _) -> do
    actionEnv' <- Evaluator.evaluateBind' actionEnv evalEnv b
    evalEnv' <- Evaluator.evaluateActionBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (evalEnv', actionEnv', analysisEnv')
  Imported _ ts' -> do
    (e', acte', ae') <- checkTopLevels evalEnv actionEnv analysisEnv ts'
    pure (e', acte', ae')
  Properties {} -> pure (evalEnv, actionEnv, analysisEnv)

checkTopLevels ee ae ane [] = pure (ee, ae, ane)
checkTopLevels ee ae ane (x : xs) = do
  (ee', ae', ane') <- checkTopLevel ee ae ane x
  checkTopLevels ee' ae' ane' xs
