module Util where

import Control.Monad.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.Terminal (renderIO)
import Specstrom.Parser
import Specstrom.PrettyPrinter
import Specstrom.Syntax
import Specstrom.TypeInf
import System.Exit
import System.IO (stderr)

searchPaths :: [FilePath]
searchPaths = ["ulib", "."]

load :: Text -> IO [TopLevel]
load = fmap (\(a, _, _) -> a) . load' True

load' :: Bool -> Text -> IO ([TopLevel], Table, Context)
load' exit f = do
  result <- first prettyParseError <$> runExceptT (loadModule searchPaths ("Command line", 0, 0) f builtIns)
  case result of
    Left err -> do
      renderIO stderr (layoutPretty defaultLayoutOptions (err <> line))
      if exit then exitFailure else pure ([], builtIns, mempty)
    Right (tbl, ts) ->
      case inferTopLevels builtInTypes ts of
        Left te ->
          renderIO stderr (layoutPretty defaultLayoutOptions (prettyTypeError te <> line))
            >> if exit then exitFailure else pure ([], builtIns, mempty)
        Right g -> pure (ts, tbl, g)
