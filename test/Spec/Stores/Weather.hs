module Spec.Stores.Weather where

import Test.Hspec
import Data.Either (isRight)
import qualified Data.Text.IO as Txt

import Spec.Helpers (defaultWeatherCfg)
import Stores.Weather (Store(..), WeatherClient)

spec :: IO ()
spec = hspec $ describe "Weather store" $ it "should synchronize with OpenWeatherMap" $ do
    weatherConfig <- defaultWeatherCfg

    (client :: WeatherClient) <- initWeatherStore weatherConfig
    synced                  <- syncForecast client
    synced `shouldSatisfy` isRight

    rendered <- getRenderedTemplate client
    let Right r = rendered
    Txt.putStrLn $ "Rendered template: " <> r
