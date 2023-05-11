-- | Asterix source code (lib) generator.

module Main where

import           Main.Utf8 (withUtf8)
import           Options.Applicative as Opt
import           Paths_generator (version)
import           Data.Version (showVersion)

import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar
import           Formatting as F

import qualified Asterix.Specs as A
import           Asterix.Specs.Validation (validate)
import           Asterix.Struct (Asterix, deriveAsterix)

import qualified Asterix.Language.Python.Generator as Python

languages :: [(Text, Bool -> Text -> Text -> Set Asterix -> TL.Text)]
languages =
    [ ("python", Python.mkCode)
    ]

data Options = Options
    { optLanguage :: Text
    , optComments :: Bool
    , optTimestamp :: Integer
    , optReference :: Text
    , optPaths :: [FilePath]
    , optShowVersion :: Bool
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption (long "language" <> metavar "LANG"
        <> help ("Target format: " ++ show (fmap fst languages)))
    <*> switch (long "comments" <> short 'c'
       <> help "Include verbose comments in output" )
    <*> option auto
        ( long "timestamp" <> metavar "TS"
       <> help "Version unix timestamp"
       <> showDefault
       <> value 0
        )
    <*> strOption
        ( long "reference" <> metavar "REF"
       <> help "Version control reference"
       <> showDefault
       <> value "unknown"
        )
    <*> some (Opt.argument str (metavar "PATH..."
       <> help ("Spec input file(s), supported formats: " ++ show syntaxList)))
    <*> switch (long "show-version"
       <> help "Show version text and exit" )
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

toVersionText :: Integer -> Text
toVersionText unixtime = sformat
    (int % left 2 '0' % left 2 '0' % "." % int)
    year month day (seconds :: Integer)
  where
    Clock.UTCTime uDay uTime = Time.posixSecondsToUTCTime $ fromIntegral unixtime
    (year, month, day) = Data.Time.Calendar.toGregorian uDay
    seconds = round uTime

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    let versionText = toVersionText $ optTimestamp cmdOptions
    case optShowVersion cmdOptions of
        True -> do
            T.putStr versionText
        False -> do
            specs <- fmap deriveAsterix <$> mapM
                (\path -> loadSpec path (BS.readFile path))
                (optPaths cmdOptions)
            mkCode <- maybe (fail "Unsupported language") pure $
                lookup (optLanguage cmdOptions) languages
            BSL.putStr $ TL.encodeUtf8 $ mkCode
                (optComments cmdOptions)
                versionText
                (optReference cmdOptions)
                (Set.fromList specs)

