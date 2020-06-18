import qualified Spec.Config as Config
import qualified Spec.Weather as WeatherTypes
import qualified Spec.Stores.Weather as WeatherStores
import qualified Spec.Clients.OpenWeatherMap as OpenWeatherMap
import qualified Spec.Clients.Github as GithubApi

main :: IO ()
main = do
    Config.spec
    WeatherTypes.spec
    WeatherStores.spec
    OpenWeatherMap.spec
    GithubApi.spec
