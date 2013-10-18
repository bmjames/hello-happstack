{-# LANGUAGE OverloadedStrings #-}
module Model (Dog(..), dogs, findDog) where

import Data.List        (find)
import Data.Text        (Text, toLower, pack)
import Data.Aeson       (ToJSON(..), object, (.=), Value)
import Data.Aeson.Types (Pair)


data Dog = Dog { name  :: Text
               , age   :: Int
               , likes :: Text
               } deriving Show

-- Database of dogs
dogs :: [Dog]
dogs = [ Dog "Fluffles" 3 "treats"
       , Dog "Snuffles" 5 "digging"
       ]

-- Simulate a real database lookup by putting the result into IO
findDog :: String -> IO (Maybe Dog)
findDog n = return $
  find ((== pack n) . toLower . name) dogs

instance ToJSON Dog where
  toJSON = asObject [ name  `as` "name"
                    , age   `as` "age"
                    , likes `as` "likes"
                    ]
 
as :: ToJSON b => (a -> b) -> Text -> a -> Pair
as getter = (. getter) . (.=)
 
asObject :: [a -> Pair] -> a -> Value
asObject = (object .) . sequence
