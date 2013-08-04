{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Views where

import Model

import Data.Text.Lazy            (pack)
import Language.Haskell.HSX.QQ   (hsx)
import Happstack.Server.HSP.HTML (defaultTemplate)
import Happstack.Server.Monads   (ServerPart)
import HSP.XML                   (XML, fromStringLit)
import HSP.XMLGenerator
import HSP.ServerPartT           () -- for instance XMLGenerator (ServerPartT m)


dogIndex :: ServerPart XML
dogIndex = defaultTemplate "Dogs Index" ()
  [hsx|
    <ul>
      <% flip map dogs (\dog ->
        <li><% name dog %></li>
      ) %>
    </ul>
  |]

viewDog :: Dog -> ServerPart XML
viewDog dog = defaultTemplate (pack $ name dog) ()
  [hsx|
    <%>
      <h1><% name dog %></h1>
      <dl>
        <dt>Age</dt>
          <dd><% show $ age dog %></dd>
        <dt>Likes</dt>
          <dd><% likes dog %></dd>
      </dl>
    </%>
  |]
