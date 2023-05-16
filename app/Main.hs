{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import GHC.Generics
import Data.String (fromString)
import Data.Yaml
import Data.List (isPrefixOf)
import System.Environment (getArgs)

data OscSpec = OscSpec {
        route :: String,
        typeTags :: String
    } deriving (Show, Generic)

data ControlValue = ControlValue {
        min :: Int,
        max :: Int,
        defaultValue :: Int,
        steps :: Int,
        origin :: Int
    } deriving (Show, Generic)

data Spec = Spec {
        osc :: OscSpec,
        value :: ControlValue
    } deriving (Show, Generic)

data Component = Component {
        kind :: String,
        spec :: Spec
    } deriving (Show, Generic)

instance FromJSON OscSpec
instance FromJSON ControlValue
instance FromJSON Spec
instance FromJSON Component

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc

readSpec :: String -> IO Component
readSpec yaml_str = do
    either (error . show) (return) $
        decodeEither' (fromString yaml_str)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ unwords args
    let components_str = splitStr "---" content
    components <- mapM readSpec components_str
    print $ show components
