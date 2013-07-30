module Model where

import Data.Char (toLower)
import Data.List (find)
import Happstack.Server (FromReqURI(..))


data Dog = Dog { name  :: String
               , age   :: Int
               , likes :: String
               } deriving Show

instance FromReqURI Dog where
  fromReqURI = findDog

-- Database of dogs
dogs :: [Dog]
dogs = [ Dog "Fluffles" 3 "treats"
       , Dog "Snuffles" 5 "digging"
       ]
       
findDog :: String -> Maybe Dog
findDog n = find ((== n) . map toLower . name) dogs
