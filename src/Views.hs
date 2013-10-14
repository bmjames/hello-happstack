{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Views where

import Model

import Control.Concurrent.MVar   (MVar, modifyMVar)
import Control.Monad.Trans       (lift)
import Data.Text                 (append, toLower)
import Data.Aeson                (Value, encode, toJSON, object, (.=), ToJSON)
import Language.Haskell.HSX.QQ   (hsx)
import Happstack.Server.Monads   (ServerPart)
import Happstack.Server.Response (ToMessage(..), ok, flatten)
import Happstack.Server.Types    (Response)
import Happstack.Server.HSP.HTML (defaultTemplate)
import HSP.XML                   (fromStringLit)
import HSP.XMLGenerator
import HSP.ServerPartT           ()


-- a pointless hit counter, just to demonstrate sharing state across threads
vote :: MVar Int -> ServerPart Response
vote counter = do newCount <- lift $ incrementMVar counter
                  ok $ jsonResponse $ object [ "votes" .= newCount ]

incrementMVar :: (Num a) => MVar a -> IO a
incrementMVar mvar = modifyMVar mvar $ \n -> let n' = n + 1 in return (n', n')

-- lists all the dogs that we know of
dogIndex :: ServerPart Response
dogIndex = flatten $ defaultTemplate "Dogs Index" ()
  [hsx|
    <ul>
      <% flip map dogs (\dog ->
        <li>
          <a href=(path dog)><% name dog %></a>
        </li>
      ) %>
    </ul>
  |]
  where path = append "/dog/" . toLower . name

-- detailed view of a single dog
viewDog :: Dog -> ServerPart Response
viewDog = ok . jsonResponse

newtype JsonResponse = JsonResponse Value

jsonResponse :: ToJSON a => a -> Response
jsonResponse = toResponse . JsonResponse . toJSON

instance ToMessage JsonResponse where
  toContentType _ = "application/json; charset=utf-8"
  toMessage (JsonResponse value) = encode value
