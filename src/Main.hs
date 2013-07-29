module Main where

import Control.Monad
import Data.List
import Happstack.Server

main :: IO ()
main = simpleHTTP nullConf $ msum [
           dir "dog"  $ path ((nullDir >>) . ok . showDog)
         , dir "dogs" $ nullDir >> ok (show dogs)
         ]

data Dog = Dog { name :: String
               , age :: Int
               , likes :: String
               } deriving Show

showDog :: Dog -> String
showDog = show

-- Database of dogs
dogs :: [Dog]
dogs = [ Dog "fluffles" 3 "treats"
       , Dog "snuffles" 5 "digging"
       ]
       
findDog :: String -> Maybe Dog
findDog n = find ((== n) . name) dogs

instance FromReqURI Dog where
  fromReqURI = findDog
