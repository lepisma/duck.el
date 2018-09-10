{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad.IO.Class
import System.IO
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import Prelude
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)


data CliOption = CliOption { text:: Text,
                             lang:: Text,
                             tz:: Text,
                             refTime:: Text
                           } deriving Show

deriveJSON defaultOptions ''CliOption

main :: IO ()
main = do
  tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
  hSetBuffering stdout NoBuffering
  input <- getLine
  case input of
    ":dims" -> do C8.putStrLn allDims; main
    ":quit" -> C8.putStrLn "bye!"
    _ -> do
      case decode (C8.pack input) :: Maybe CliOption of
        Just option -> parseOption option tzs >>= C8.putStrLn
        Nothing -> C8.putStrLn "ERR"
      main

parseOption :: CliOption -> HashMap Text TimeZoneSeries -> IO C8.ByteString
parseOption opt tzs =
  parseDuck tzs (text opt) (lang opt) Nothing (tz opt) Nothing (refTime opt) Nothing

allDims :: C8.ByteString
allDims =
  encode $ HashMap.fromList . map dimText $ HashMap.toList supportedDimensions
  where
    dimText :: (Lang, [Some Dimension]) -> (Text, [Text])
    dimText = (Text.toLower . showt) *** map (\(This d) -> toName d)

parseDuck :: HashMap Text TimeZoneSeries
          -> Text -> Text -> Maybe Text -> Text -> Maybe Text -> Text -> Maybe Text
          -> IO C8.ByteString
parseDuck tzs text lang dims tz loc reftime latent = do
  now <- liftIO $ currentReftime tzs tz
  let
    context = Context
      { referenceTime = maybe now (parseRefTime tz) reftime
      , locale = maybe (makeLocale (parseLang lang) Nothing) parseLocale loc
      }
    options = Options {withLatent = parseLatent latent}

    dimParse = fromMaybe [] $ decode "" -- TODO $ fromMaybe Text.empty dims
    dims = mapMaybe fromName dimParse

    parsedResult = parse text context options dims

  return $ encode parsedResult
  where
    defaultLang = EN
    defaultLocale = makeLocale defaultLang Nothing
    defaultLatent = False

    parseLocale :: Text -> Locale
    parseLocale x = maybe defaultLocale (`makeLocale` mregion) mlang
      where
        (mlang, mregion) = case chunks of
          [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
          _      -> (Nothing, Nothing)
        chunks = map Text.unpack . Text.split (== '_') . Text.toUpper $ x

    parseLang :: Text -> Lang
    parseLang l = fromMaybe defaultLang $ (readMaybe . Text.unpack . Text.toUpper) l

    parseRefTime :: Text -> Text -> DucklingTime
    parseRefTime timezone refTime = makeReftime tzs timezone utcTime
      where
        msec = read $ Text.unpack refTime
        utcTime = posixSecondsToUTCTime $ fromInteger msec / 1000

    parseLatent :: Maybe Text -> Bool
    parseLatent x = fromMaybe defaultLatent
      (readMaybe (Text.unpack $ Text.toTitle $ fromMaybe Text.empty x)::Maybe Bool)
