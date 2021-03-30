module Util where


import Control.Monad.Except (runExceptT)
import Specstrom.Parser
import Specstrom.Syntax
import Specstrom.PrettyPrinter
import System.Exit
import Data.Text (Text)
import Data.Bifunctor (first)
import System.IO (stderr)
import Prettyprinter.Render.Terminal (renderIO)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Specstrom.TypeInf

searchPaths :: [FilePath]
searchPaths = ["ulib", "."]

load :: Text -> IO [TopLevel]
load = fmap (\(a,_,_) -> a) . load' True


load' :: Bool -> Text -> IO ([TopLevel], Table, Context)
load' exit f = do
  result <- first prettyParseError <$> runExceptT (loadModule searchPaths ("Command line", 0, 0) f builtIns)
  case result of
    Left err -> do
      renderIO stderr (layoutPretty defaultLayoutOptions (err <> line))
      if exit then exitFailure else pure ([], builtIns, mempty)
    Right (tbl, ts) ->
      case inferTopLevels builtInTypes ts of
        Left te -> renderIO stderr (layoutPretty defaultLayoutOptions (prettyTypeError te <> line)) 
                >> if exit then exitFailure else pure ([],builtIns, mempty)
        Right g -> pure (ts,tbl,g)