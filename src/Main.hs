{-# LANGUAGE OverloadedStrings #-}
module Main where

import Views

import Control.Monad           (msum)
import Control.Concurrent.MVar (newMVar)
import Happstack.Server


main :: IO ()
main = do voteCounter <- newMVar 0
          simpleHTTP nullConf $ msum [
              dir "vote" $ nullDir >> vote voteCounter
            , dir "dogs" $ nullDir >> dogIndex
            , dir "dog"  $ path $ (nullDir >>) . viewDog
            ]
