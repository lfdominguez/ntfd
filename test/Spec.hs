import qualified Spec.Config as Config
import qualified Spec.Weather as Weather
import qualified Spec.Clients.OpenWeatherMap as OpenWeatherMap

main :: IO ()
main = do
    Config.spec
    Weather.spec
    OpenWeatherMap.spec
