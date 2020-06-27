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
    = Celsius
    | Kelvin
    | Fahrenheit
    deriving (Show)

{- HLINT ignore "Reduce duplication" -}
instance Eq Temperature where
    left == (Temperature rightVal rightUnit) =
        let
            Temperature leftVal _ = convert left rightUnit
            leftTemp              = round leftVal :: Integer
            rightTemp             = round rightVal :: Integer
        in leftTemp == rightTemp

instance Ord Temperature where
    left <= (Temperature rightVal rightUnit) =
        let
            Temperature leftVal _ = convert left rightUnit
            leftTemp              = round leftVal :: Integer
            rightTemp             = round rightVal :: Integer
        in leftTemp <= rightTemp

-- | Convert a temperature from one unit to another
convert :: Temperature -> Unit -> Temperature
convert (Temperature v Kelvin    ) Celsius    = Temperature (v - 273.15) Celsius
convert (Temperature v Kelvin    ) Fahrenheit = Temperature (v * 9 / 5 - 459.67) Fahrenheit
convert (Temperature v Celsius   ) Kelvin     = Temperature (v + 273.15) Kelvin
convert (Temperature v Celsius   ) Fahrenheit = Temperature (v * 9 / 5 + 32) Fahrenheit
convert (Temperature v Fahrenheit) Kelvin     = Temperature ((v + 459.67) * 5 / 9) Kelvin
convert (Temperature v Fahrenheit) Celsius    = Temperature ((v - 32) * 5 / 9) Celsius
convert original                   _          = original
