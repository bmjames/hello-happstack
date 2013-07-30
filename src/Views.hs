{-# LANGUAGE QuasiQuotes #-}
module Views where

import Model

import Language.Haskell.HSX.QQ (hsx)
import HSP.Monad               (HSPT)
import HSP.XML                 (XML, fromStringLit)
import HSP.XMLGenerator


dogIndex :: (Functor m, Monad m) => (HSPT XML m) XML
dogIndex = unXMLGenT
  [hsx|
    <html>
      <body>
        <ul>
          <% mapM (\dog ->
            <li>
              <% name dog %>
            </li>
          ) dogs %>
        </ul>
      </body>
    </html>
  |]

viewDog :: (Functor m, Monad m) => Dog -> (HSPT XML m) XML
viewDog dog = unXMLGenT
  [hsx|
    <html>
      <body>
        <h1><% name dog %></h1>
        <dl>
          <dt>Age</dt>
            <dd><% show $ age dog %></dd>
          <dt>Likes</dt>
            <dd><% likes dog %></dd>
        </dl>
      </body>
    </html>
  |]
