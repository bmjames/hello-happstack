module Main where

import Views

import Control.Monad (msum)
import Happstack.Server
import Happstack.Server.HSP.HTML ()


main :: IO ()
main = simpleHTTP nullConf $ msum [
           dir "dogs" $ nullDir >> dogIndex
         , dir "dog"  $ path $ (nullDir >>) . viewDog
         ]
