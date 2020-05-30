module Types.Weather
    ( convert
    , Temperature(..)
    , Unit(..)
    )
where

-- | A temperature and its associated unit
data Temperature = Temperature Float Unit deriving (Show)

-- | Supported temperature unit
data Unit
    = Celcius
    | Kelvin
    | Fahrenheit
    deriving (Show)

-- brittany-disable-next-binding
instance Eq Temperature where
    left == (Temperature rightVal rightUnit) =
        let Temperature leftVal _ = convert left rightUnit
        in leftVal == rightVal

-- brittany-disable-next-binding
instance Ord Temperature where
    left <= (Temperature rightVal rightUnit) =
        let Temperature leftVal _ = convert left rightUnit
        in leftVal <= rightVal

-- | Convert a temperature from one unit to another
convert :: Temperature -> Unit -> Temperature
convert (Temperature v Kelvin    ) Celcius    = Temperature (v - 273.15) Celcius
convert (Temperature v Kelvin    ) Fahrenheit = Temperature (v * 9 / 5 - 459.67) Fahrenheit
convert (Temperature v Celcius   ) Kelvin     = Temperature (v + 273.15) Kelvin
convert (Temperature v Celcius   ) Fahrenheit = Temperature (v * 9 / 5 + 32) Fahrenheit
convert (Temperature v Fahrenheit) Kelvin     = Temperature ((v + 459.67) * 5 / 9) Kelvin
convert (Temperature v Fahrenheit) Celcius    = Temperature ((v - 32) * 5 / 9) Celcius
convert original                   _          = original

