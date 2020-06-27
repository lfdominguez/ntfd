module Spec.Weather where

import Test.Hspec

import Types.Weather (convert, Temperature(..), Unit(..))

spec :: IO ()
spec = hspec $ describe "Weather helpers" $ do
    it "should convert Celsius to Fahrenheit" $ do
        let left  = convert (Temperature 0 Celsius) Fahrenheit
        let right = Temperature 32 Fahrenheit
        left `shouldBe` right

    it "should convert Celsius to Kelvin" $ do
        let left  = convert (Temperature (-50) Celsius) Kelvin
        let right = Temperature 223.15 Kelvin
        left `shouldBe` right

    it "should convert Fahrenheit to Celsius" $ do
        let left  = convert (Temperature 32 Fahrenheit) Celsius
        let right = Temperature 0 Celsius
        left `shouldBe` right

    it "should convert Fahrenheit to Kelvin" $ do
        let left  = convert (Temperature (-459.67) Fahrenheit) Kelvin
        let right = Temperature 0 Kelvin
        left `shouldBe` right

    it "should convert Kelvin to Celsius" $ do
        let left  = convert (Temperature 223.15 Kelvin) Celsius
        let right = Temperature (-50) Celsius
        left `shouldBe` right

    it "should convert Kelvin to Fahrenheit" $ do
        let left  = convert (Temperature 0 Kelvin) Fahrenheit
        let right = Temperature (-459.67) Fahrenheit
        left `shouldBe` right

    it "should perform equality checks on rounded values" $ do
        let left  = Temperature 23.3 Celsius
        let right = Temperature 23.4 Celsius
        left `shouldBe` right
