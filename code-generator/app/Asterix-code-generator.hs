-- | Asterix source code (lib) generator.

module Main where

import           Main.Utf8 (withUtf8)
import           Options.Applicative as Opt
import           Paths_generator (version)
import           Data.Version (showVersion)

import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Asterix.Specs as A
import           Asterix.Specs.Validation (validate)
import           Asterix.Struct (Asterix, deriveAsterix)

import qualified Asterix.Language.Python.Generator as Python

languages :: [(Text, Bool -> Maybe Text -> Set Asterix -> TL.Text)]
languages =
    [ ("python", Python.mkCode)
    ]

data Options = Options
    { optLanguage :: Text
    , optComments :: Bool
    , optReference :: Maybe Text
    , optPaths :: [FilePath]
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption (long "language" <> metavar "LANG"
        <> help ("Target format: " ++ show (fmap fst languages)))
    <*> switch (long "comments" <> short 'c'
       <> help "Include verbose comments in output" )
    <*> optional (strOption (long "reference" <> metavar "REF"
        <> help ("Put reference string to the source code")))
    <*> some (Opt.argument str (metavar "PATH..."
       <> help ("Spec input file(s), supported formats: " ++ show syntaxList)))
  where
    syntaxList = do
        (shortName, _, _) <- A.availableDecoders
        pure shortName

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> parseOptions)
    ( fullDesc <> Opt.header "aspecs" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

loadSpec :: FilePath -> IO BS.ByteString -> IO A.Asterix
loadSpec path getS = do
    syntax <- maybe (die "unknown syntax") pure (lookup fmt A.syntaxes)
    decoder <- maybe (die "no decoder") pure (A.syntaxDecoder syntax)
    s <- getS
    ast <- either die pure (decoder path s)
    case validate False ast of
        [] -> pure ast
        _lst -> die "validation errors"
  where
    die :: String -> IO a
    die msg = fail $ path <> ": " <> msg
    fmt = reverse $ fst $ break (== '.') $ reverse path

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- fmap deriveAsterix <$> mapM
        (\path -> loadSpec path (BS.readFile path))
        (optPaths cmdOptions)
    mkCode <- maybe (fail "Unsupported language") pure $
        lookup (optLanguage cmdOptions) languages
    BSL.putStr $ TL.encodeUtf8 $ mkCode
        (optComments cmdOptions)
        (optReference cmdOptions)
        (Set.fromList specs)

