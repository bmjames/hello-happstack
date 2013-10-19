{-# LANGUAGE OverloadedStrings #-}
module Model (Dog(..), allDogs, findDog) where

import Data.List        (find)
import Data.Text        (Text, toLower, pack)
import Data.Aeson       (ToJSON(..), object, (.=), Value)
import Data.Aeson.Types (Pair)
import Data.Functor     ((<$>))


data Dog = Dog { name  :: Text
               , age   :: Int
               , likes :: Text
               } deriving Show

-- | Database of dogs.
allDogs :: IO [Dog]
        -- simulate a real database by putting the results in IO
allDogs = return [ Dog "Fluffles" 3 "treats"
                 , Dog "Snuffles" 5 "digging"
                 ]

-- | Look up a dog by name in the \"database\".
findDog :: String         -- ^ The name of the dog
        -> IO (Maybe Dog)
findDog n = find ((== pack n) . toLower . name) <$> allDogs

instance ToJSON Dog where
  toJSON = asObject [ name  `as` "name"
                    , age   `as` "age"
                    , likes `as` "likes"
                    ]
 
as :: ToJSON b => (a -> b) -> Text -> a -> Pair
as getter = (. getter) . (.=)
 
asObject :: [a -> Pair] -> a -> Value
asObject = (object .) . sequence
