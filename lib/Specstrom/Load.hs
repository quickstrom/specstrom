module Specstrom.Load where

import Control.Monad.Except (runExceptT, withExceptT, liftEither)
import Data.Text (Text)
import Specstrom.Parser
import Specstrom.Syntax
import Specstrom.TypeInf

data LoadError
  = LoadParseError ParseError
  | LoadTypeInfError TypeInfError

load :: [FilePath] -> Text -> IO (Either LoadError [TopLevel])
load searchPaths f = fmap (\(a, _, _) -> a) <$> load' searchPaths f

load' :: [FilePath] -> Text -> IO (Either LoadError ([TopLevel], Table, Context))
load' searchPaths f = runExceptT $ do
  (tbl, ts) <- withExceptT LoadParseError (loadModule searchPaths ("Command line", 0, 0) f builtIns)
  g <- withExceptT LoadTypeInfError (liftEither (inferTopLevels builtInTypes ts))
  pure (ts, tbl, g)
