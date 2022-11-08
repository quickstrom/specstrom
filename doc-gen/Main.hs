{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Trans
import Data.Aeson (object, (.=))
import Data.FileEmbed
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import qualified Prettyprinter as P
import Prettyprinter.Render.Terminal (renderIO)
import qualified Prettyprinter.Render.Text as P
import Specstrom.Load
import Specstrom.PrettyPrinter
import Specstrom.Syntax
import Specstrom.TypeInf
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import System.IO (stderr)
import Text.DocLayout (render)
import Text.DocTemplates hiding (Context)
import Text.Pandoc

data CliOptions = CliOptions
  { searchPaths :: [FilePath],
    outputDirectory :: FilePath,
    docFormat :: Text,
    baseUrl :: Text,
    moduleName :: FilePath
  }
  deriving (Show)

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> many (strOption (long "include" <> short 'I' <> help "include the given path in module search paths" <> metavar "DIR"))
    <*> (fromMaybe "." <$> optional (strOption (long "output" <> short 'o' <> help "export HTML documentation to the specified directory" <> metavar "DIR")))
    <*> (fromMaybe "markdown" <$> optional (strOption (long "format" <> short 'f' <> help "Format for documentation (default:markdown)" <> metavar "FORMAT")))
    <*> (fromMaybe "/" <$> optional (strOption (long "base-url" <> short 'u' <> help "Base url for links (default: /)" <> metavar "URL")))
    <*> argument str (metavar "MODULE")

parserInfo :: ParserInfo CliOptions
parserInfo =
  info
    (cliOptionsParser <**> helper)
    ( fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative"
    )

template :: Text
template = $(embedStringFile "doc-gen/template.html")

main :: IO ()
main = do
  cliOpts <- execParser parserInfo
  let allSearchPaths = searchPaths cliOpts <> ["."]
  let f = moduleName cliOpts
  load' allSearchPaths (T.pack f) >>= \case
    Left err -> do
      renderIO stderr (P.layoutPretty P.defaultLayoutOptions (prettyLoadError err))
      exitFailure
    Right (ts, tbl, ctx) ->
      runIOorExplode $ generateDocs (baseUrl cliOpts) f (outputDirectory cliOpts) (docFormat cliOpts) ts ctx

generateDocs :: Text -> FilePath -> FilePath -> Text -> [TopLevel] -> Context -> PandocIO ()
generateDocs base path out reader ls ctx = do
  (rd, exts) <- (\(TextReader rd, exts) -> (rd, exts)) <$> getReader reader
  let ros = def {readerExtensions = exts}
      wos = def
      go [] = pure []
      go (DocBlock docs : rest) = do
        Pandoc _ doc <- rd ros (T.unlines docs)
        (Div ("", ["doc-block"], []) doc :) <$> go rest
      go (tl@(Binding docs (Bind bp _)) : rest) = do
        Pandoc _ doc <- rd ros (T.unlines docs)
        let docdiv = if null doc then Div ("", ["bottom"], []) [] else Div ("", ["docs"], []) doc
        let header' = P.renderStrict (P.layoutCompact (prettyToplevelHeader tl))
        let vars = bindPatternBoundVars bp
        (Div ("", ["binding"], []) [Div ("", ["header"], []) [Plain [Strong [Str "let"], Space, Span ("", ["term"], []) [Str header']]], Plain (map (\v -> Span (v, [], []) []) vars), docdiv] :) <$> go rest
      go (tl@(ActionDecl docs (Bind bp _)) : rest) = do
        Pandoc _ doc <- rd ros (T.unlines docs)
        let docdiv = if null doc then Div ("", ["bottom"], []) [] else Div ("", ["docs"], []) doc
        let header' = P.renderStrict (P.layoutCompact (prettyToplevelHeader tl))
        let vars = bindPatternBoundVars bp
        (Div ("", ["action-binding"], []) [Div ("", ["header"], []) [Plain [Strong [Str "action"], Space, Span ("", ["term"], []) [Str header']]], Plain (map (\v -> Span (v, [], []) []) vars), docdiv] :) <$> go rest
      go (tl@(MacroDecl docs mac _ bod) : rest) = do
        let (name', _) = peelAps mac []
        let name = case name' of Var _ n -> [n]; _ -> []
        Pandoc _ doc <- rd ros (T.unlines docs)
        let docdiv = if null doc then Div ("", ["bottom"], []) [] else Div ("", ["docs"], []) doc
        let header' = P.renderStrict (P.layoutCompact (prettyToplevelHeader tl))
        (Div ("", ["macro-binding"], []) [Div ("", ["header"], []) [Plain [Strong [Str "macro"], Space, Span ("", ["term"], []) [Str header']]], Plain (map (\n -> Span (n, [], []) []) name), docdiv] :) <$> go rest
      go (tl@(SyntaxDecl docs _ _ _ _) : rest) = do
        Pandoc _ doc <- rd ros (T.unlines docs)
        let docdiv = if null doc then Div ("", ["bottom"], []) [] else Div ("", ["docs"], []) doc
        let header' = P.renderStrict (P.layoutCompact (prettyToplevelHeader tl))
        (Div ("", ["syntax-decl"], []) [Div ("", ["header"], []) [Plain [Strong [Str "syntax"], Space, Span ("", ["term"], []) [Str header']]], docdiv] :) <$> go rest
      go (Imported _ f tls : rest) = do
        generateDocs base (T.unpack f) out reader tls ctx
        (Div ("", ["import"], []) [Div ("", ["header"], []) [Plain [Strong [Str "import"], Space, Link ("", [], []) [Str f] (T.pack (T.unpack base </> T.unpack f <.> "html"), "")]]] :) <$> go rest
      go (tl : rest) = do
        let header' = P.renderStrict (P.layoutCompact (prettyToplevelHeader tl))
        (Div ("", ["misc"], []) [Div ("", ["header"], []) [Plain [Str header']]] :) <$> go rest
  blocks <- go ls
  text <- writeHtml5String wos (Pandoc mempty blocks)
  merr <- liftIO (compileTemplate "" template)
  case merr of
    Left e -> error e
    Right t -> do
      time <- getZonedTime
      let output = render Nothing $ renderTemplate t $ object ["body" .= text, "document-css" .= True, "displaymath-css" .= True, "highlighting-css" .= True, "pagetitle" .= path, "title" .= path, "date" .= show time]
      liftIO $ createDirectoryIfMissing True (takeDirectory (out </> path <.> "html"))
      liftIO $ T.writeFile (out </> path <.> "html") output
