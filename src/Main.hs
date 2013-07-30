module Main where

import Views

import Control.Monad (msum)
import HSP.Monad (unHSPT)
import Happstack.Server
import Happstack.Server.HSP.HTML ()


main :: IO ()
main = simpleHTTP nullConf $ msum [
           dir "dogs" $ nullDir >> unHSPT dogIndex
         , dir "dog"  $ path $ (nullDir >>) . unHSPT . viewDog
         ]
