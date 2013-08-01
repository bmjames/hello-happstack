{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Views where

import Model

import Data.Text.Lazy            (pack)
import Language.Haskell.HSX.QQ   (hsx)
import Happstack.Server.HSP.HTML (defaultTemplate)
import HSP.Monad                 (HSPT)
import HSP.XML                   (XML, fromStringLit)
import HSP.XMLGenerator


dogIndex :: (Functor m, Monad m) => (HSPT XML m) XML
dogIndex = defaultTemplate "Dogs Index" ()
  [hsx|
    <ul>
      <% flip map dogs (\dog ->
        <li><% name dog %></li>
      ) %>
    </ul>
  |]

viewDog :: (Functor m, Monad m) => Dog -> (HSPT XML m) XML
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
