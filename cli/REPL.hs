{-# LANGUAGE LambdaCase #-}

module REPL where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Foldable (foldl')
import Data.Text (Text, pack)
import Prettyprinter (defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.Terminal (renderIO)
import qualified Specstrom.Analysis as Analysis
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Load
import qualified Specstrom.Parser as Parser
import Specstrom.PrettyPrinter
import Specstrom.Syntax as Syntax
import qualified Specstrom.TypeInf as TypeInf
import System.Console.Haskeline
import System.IO (hPutStrLn, stderr, stdout)

repl :: [FilePath] -> Text -> IO ()
repl searchPaths' fp = do
  load' searchPaths' fp >>= \case
    Left err -> renderIO stderr (layoutPretty defaultLayoutOptions (prettyLoadError err))
    Right (tls, tbl, g) -> do
      (e, e', ae) <- liftIO $ checkTopLevels Evaluator.basicEnv mempty Analysis.builtIns tls
      let opts = foldl' addSearchPath defaultOpts searchPaths'
      runInputT defaultSettings (loop 1 opts tbl g e e' Evaluator.dummyState ae)

defaultOpts :: Options
defaultOpts = Opts False False []

data Options = Opts {showTypes :: Bool, showAnalysis :: Bool, searchPaths :: [FilePath]}

addSearchPath :: Options -> FilePath -> Options
addSearchPath opts path = opts {searchPaths = path : searchPaths opts}

loop ::
  Int ->
  Options ->
  Parser.Table ->
  TypeInf.Context ->
  Evaluator.Env ->
  Evaluator.Env -> -- action bodies
  Evaluator.State ->
  Analysis.AnalysisEnv ->
  InputT IO ()
loop lno opts tbl g e e' s ae = flip catch (\err -> liftIO (renderIO stderr (layoutPretty defaultLayoutOptions (prettyEvalError [] err <> line))) >> loop lno opts tbl g e e' s ae) $ do
  str <- getInputLine "> "
  case str of
    Nothing -> pure ()
    Just (':' : rest)
      | rest == "set types" -> loop lno (opts {showTypes = True}) tbl g e e' s ae
      | rest == "set analysis" -> loop lno (opts {showAnalysis = True}) tbl g e e' s ae
      | rest == "unset analysis" -> loop lno (opts {showAnalysis = False}) tbl g e e' s ae
      | rest == "unset types" -> loop lno (opts {showTypes = False}) tbl g e e' s ae
      | rest == "quit" -> pure ()
      | rest == "debug" -> liftIO (print (e, e')) >> loop lno opts tbl g e e' s ae
      | rest == "state" -> pure () -- for now
      | otherwise -> liftIO $ hPutStrLn stderr "Invalid meta-command"
    Just x -> do
      r <- liftIO $ runExceptT (Parser.loadImmediate lno (searchPaths opts) tbl $ pack x)
      case r of
        Left err -> do
          liftIO $ renderIO stderr (layoutPretty defaultLayoutOptions (prettyParseError err <> line))
          loop lno opts tbl g e e' s ae
        Right (tbl2, Right expr) -> case TypeInf.inferExpImmediate g expr of
          Left err -> do
            liftIO $ renderIO stderr (layoutPretty defaultLayoutOptions (prettyTypeError err <> line))
            loop lno opts tbl g e e' s ae
          Right typ -> do
            if showTypes opts
              then liftIO $ renderIO stdout (layoutPretty defaultLayoutOptions (prettyType typ <> line))
              else return ()
            if showAnalysis opts
              then liftIO $ LBS.putStrLn (JSON.encode (Analysis.depOf (Analysis.analyseExpr ae expr)))
              else return ()
            val <- liftIO $ (Evaluator.deepForce s =<< Evaluator.evaluate s e expr)
            liftIO $ renderIO stdout (layoutPretty defaultLayoutOptions (prettyValue val <> line))
            loop lno opts tbl2 g e e' s ae
        Right (tbl2, Left tls) -> do
          case TypeInf.inferTopLevels g tls of
            Left err -> do
              liftIO $ renderIO stderr (layoutPretty defaultLayoutOptions (prettyTypeError err <> line))
              loop lno opts tbl g e e' s ae
            Right g2 -> do
              (e2, e'2, ae2) <- liftIO $ checkTopLevels e e' ae tls
              loop (succ lno) opts tbl2 g2 e2 e'2 s ae2

checkTopLevel ::
  Evaluator.Env ->
  Evaluator.Env -> -- action bodies
  Analysis.AnalysisEnv ->
  TopLevel ->
  IO (Evaluator.Env, Evaluator.Env, Analysis.AnalysisEnv)
checkTopLevel evalEnv actionEnv analysisEnv tl = case tl of
  Binding _ b@(Syntax.Bind pat _) -> do
    evalEnv' <- Evaluator.evaluateBind Evaluator.dummyState evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (evalEnv', actionEnv, analysisEnv')
  ActionDecl _ b@(Syntax.Bind pat _) -> do
    actionEnv' <- Evaluator.evaluateBind' Evaluator.dummyState actionEnv evalEnv b
    evalEnv' <- Evaluator.evaluateActionBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (evalEnv', actionEnv', analysisEnv')
  Imported _ _ ts' -> do
    (e', acte', ae') <- checkTopLevels evalEnv actionEnv analysisEnv ts'
    pure (e', acte', ae')
  Properties {} -> pure (evalEnv, actionEnv, analysisEnv)
  DocBlock {} -> pure (evalEnv, actionEnv, analysisEnv)
  SyntaxDecl {} -> pure (evalEnv, actionEnv, analysisEnv)
  MacroDecl {} -> pure (evalEnv, actionEnv, analysisEnv)

checkTopLevels ::
  Evaluator.Env ->
  Evaluator.Env -> -- action bodies
  Analysis.AnalysisEnv ->
  [TopLevel] ->
  IO (Evaluator.Env, Evaluator.Env, Analysis.AnalysisEnv)
checkTopLevels ee ae ane [] = pure (ee, ae, ane)
checkTopLevels ee ae ane (x : xs) = do
  (ee', ae', ane') <- checkTopLevel ee ae ane x
  checkTopLevels ee' ae' ane' xs
