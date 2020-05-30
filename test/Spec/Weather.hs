module Spec.Weather where

import Test.Hspec

import Types.Weather (convert, Temperature(..), Unit(..))

spec :: IO ()
spec = hspec $ describe "Weather helpers" $ do
    it "should convert Celcius to Fahrenheit" $ do
        let left  = convert (Temperature 0 Celcius) Fahrenheit
        let right = Temperature 32 Fahrenheit
        left `shouldBe` right

    it "should convert Celcius to Kelvin" $ do
        let left  = convert (Temperature (-50) Celcius) Kelvin
        let right = Temperature 223.15 Kelvin
        left `shouldBe` right

    it "should convert Fahrenheit to Celcius" $ do
        let left  = convert (Temperature 32 Fahrenheit) Celcius
        let right = Temperature 0 Celcius
        left `shouldBe` right

    it "should convert Fahrenheit to Kelvin" $ do
        let left  = convert (Temperature (-459.67) Fahrenheit) Kelvin
        let right = Temperature 0 Kelvin
        left `shouldBe` right

    it "should convert Kelvin to Celcius" $ do
        let left  = convert (Temperature 223.15 Kelvin) Celcius
        let right = Temperature (-50) Celcius
        left `shouldBe` right

    it "should convert Kelvin to Fahrenheit" $ do
        let left  = convert (Temperature 0 Kelvin) Fahrenheit
        let right = Temperature (-459.67) Fahrenheit
        left `shouldBe` right
