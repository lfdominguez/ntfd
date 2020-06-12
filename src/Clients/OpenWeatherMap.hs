module Clients.OpenWeatherMap
    ( fetchOwm
    , isDegradedConditions
    , toSymbolicName
    , toWeatherIcon
    , Error(..)
    , OwmResponse(..)
    , QueryType(..)
    )
where

import Control.Exception (try)
import Data.Aeson ((.:), eitherDecode, withObject, Value(..), Object)
import Data.Aeson.Types (parseEither, Parser)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Vector ((!?))
import Network.HTTP.Simple

import Config (WeatherConfig(..))

-- OpenWeatherMap response to API calls
data OwmResponse = OwmResponse
    { owmStatus :: Int
    , owmTemperature :: Float
    , owmDescription :: Text
    , owmIcon :: Text
    }
    deriving (Show)

-- Fetch weather data from OpenWeatherMap - Results are always in Celcius
fetchOwm :: WeatherConfig -> QueryType -> IO (Either Error OwmResponse)
fetchOwm cfg queryType = do
    baseRequest <- parseRequest "GET https://api.openweathermap.org/"
    let request = setRequestPath (endpoint queryType) $ setRequestQueryString params baseRequest
    res <- try $ httpLbs request
    pure $ case res of
        Left  e -> Left $ Client e
        Right r -> parseResponse (getResponseBody r) queryType
  where
    endpoint Current  = "/data/2.5/weather"
    endpoint Forecast = "/data/2.5/forecast"
    params =
        [ ("appid", Just $ weatherApiKey cfg)
        , ("id"   , Just $ weatherCityId cfg)
        , ("units", Just "metric")
        , ("cnt"  , Just "1")
        ]

-- More details here: https://openweathermap.org/weather-conditions
-- brittany-disable-next-binding
toWeatherIcon :: Text -> Char
toWeatherIcon i
    | i == "01d" = '\61453' -- Clear sky - day
    | i == "01n" = '\61486' -- Clear sky - night
    | i == "02d" = '\61442' -- Few clouds (11-25%) - day
    | i == "02n" = '\61574' -- Few clouds (11-25%) - night
    -- Scattered clouds (25-50%) - day/night
    | i == "03d" || i == "03n" = '\61505'
    -- Broken / Overcast clouds (51-84% / 85-100%) - day/night
    | i == "04d" || i == "04n" = '\61459'
    -- Shower rain - day/night
    | i == "09d" || i == "09n" = '\61464'
    | i == "10d" = '\61448' -- Moderate / heavy rain - day
    | i == "10n" = '\61494' -- Moderate / heavy rain - night
    | i == "11d" = '\61445' -- Thunderstorm - day
    | i == "11n" = '\61477' -- Thunderstorm - night
    | i == "13d" = '\61450' -- Snow - day
    | i == "13n" = '\61482' -- Snow - night
    | i == "50d" = '\61443' -- Fog - day
    | i == "50n" = '\61514' -- Fog - night
    | otherwise = '\61453'  -- ??

-- Status codes can be found here: https://openweathermap.org/weather-conditions
-- Icon names info: https://specifications.freedesktop.org/icon-naming-spec/icon-naming-spec-latest.html
toSymbolicName :: Int -> Text
toSymbolicName status
    | status >= 200 && status < 300 = "weather-storm-symbolic"
    | status >= 300 && status < 400 = "weather-showers-scattered-symbolic"
    | status >= 500 && status < 600 = "weather-showers-symbolic"
    | status >= 600 && status < 700 = "weather-snow-symbolic"
    | status >= 700 && status < 800 = "weather-fog-symbolic"
    | otherwise                     = "weather-overcast-symbolic"

-- We need to know when we're transitioning into degraded weather conditions
isDegradedConditions :: Int -> Int -> Bool
isDegradedConditions old new
    | new >= 800 = False
    | old == new = False
    | otherwise  = True

parseResponse :: ByteString -> QueryType -> Either Error OwmResponse
parseResponse bytes queryType = do
    value <- first InvalidJson $ eitherDecode bytes
    first Parse $ parseEither (parser queryType) value
  where
    parser Current  = currentParser
    parser Forecast = forecastParser

{- HLINT ignore "Reduce duplication" -}
-- The two parsers look similar but I don't want to split this too much
currentParser :: Value -> Parser OwmResponse
currentParser = withObject "weather" $ \o -> do
    main           <- o .: "main"
    weather        <- weatherParser $ Object o
    owmStatus      <- weather .: "id"
    owmTemperature <- main .: "temp"
    owmDescription <- weather .: "description"
    owmIcon        <- weather .: "icon"
    return OwmResponse { .. }

forecastParser :: Value -> Parser OwmResponse
forecastParser = withObject "forecast" $ \o -> do
    -- The forecast call returns a list, like above,
    -- we only care about the first entry (a forecast for the next3 hours)
    items <- o .: "list"
    root  <- case items !? 0 of
        Just w  -> pure w
        Nothing -> fail "no forecast received"
    main           <- root .: "main"
    weather        <- weatherParser $ Object root
    owmStatus      <- weather .: "id"
    owmTemperature <- main .: "temp"
    owmDescription <- weather .: "description"
    owmIcon        <- weather .: "icon"
    return OwmResponse { .. }

weatherParser :: Value -> Parser Object
weatherParser = withObject "weather array" $ \o -> do
    weatherArray <- o .: "weather"
    case weatherArray !? 0 of
        Just w  -> pure w
        Nothing -> fail "weather array was empty"

data QueryType
    = Current
    | Forecast
    deriving (Show)

-- | Error types the client might return.
data Error
    = Parse String -- ^ OpenWeatherMap responded with an unexpected JSON object.
    | InvalidJson String -- ^ OpenWeatherMap responded with invalid JSON.
    | Client HttpException -- ^ Other type of errors returned by the underlying client.
    deriving (Show)
