{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Views where

import Model

import Control.Concurrent.MVar   (MVar, modifyMVar)
import Data.Text.Lazy            (pack)
import Data.Char                 (toLower)
import Control.Monad.Trans       (lift)

import Language.Haskell.HSX.QQ   (hsx)
import Happstack.Server.HSP.HTML (defaultTemplate)
import Happstack.Server.Monads   (ServerPart)
import HSP.XML                   (XML, fromStringLit)
import HSP.XMLGenerator
import HSP.ServerPartT           ()


-- a pointless hit counter, just to demonstrate sharing state across threads
vote :: MVar Int -> ServerPart XML
vote counter = do newCount <- lift $ modifyMVar counter $ \n ->
                    let n' = n + 1 in return (n', n')
                  defaultTemplate "Vote counter" ()
                    [hsx| <h1><% newCount %> votes!</h1> |]

-- lists all the dogs that we know of
dogIndex :: ServerPart XML
dogIndex = defaultTemplate "Dogs Index" ()
  [hsx|
    <ul>
      <% flip map dogs (\dog ->
        <li>
          <a href=(path dog)><% name dog %></a>
        </li>
      ) %>
    </ul>
  |]
  where path dog = "/dog/" ++ map toLower (name dog)

-- detailed view of a single dog
viewDog :: Dog -> ServerPart XML
viewDog dog = defaultTemplate (pack $ name dog) ()
  [hsx|
    <%>
      <h1><% name dog %></h1>
      <dl>
        <dt>Age</dt>
          <dd><% age dog %></dd>
        <dt>Likes</dt>
          <dd><% likes dog %></dd>
      </dl>
    </%>
  |]
