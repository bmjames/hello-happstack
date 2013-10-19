{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Views (vote, dogIndex, viewDog) where

import Model

import Control.Concurrent.MVar   (MVar, modifyMVar)
import Control.Monad.Trans       (liftIO)
import Control.Monad             ((<=<))
import Data.Text                 (append, toLower)
import Data.Aeson                (Value, encode, toJSON, object, (.=), ToJSON)
import Data.Traversable          (for)
import Language.Haskell.HSX.QQ   (hsx)
import Happstack.Server.Monads   (ServerPart)
import Happstack.Server.Response (ToMessage(..), ok, notFound, flatten)
import Happstack.Server.Types    (Response)
import Happstack.Server.HSP.HTML (defaultTemplate)
import HSP.XML                   (fromStringLit)
import HSP.XMLGenerator
import HSP.ServerPartT           ()


-- | Pointless hit counter, just to demonstrate sharing state across threads.
vote :: MVar Int -> ServerPart Response
vote counter = do newCount <- liftIO $ incrementMVar counter
                  ok . jsonResponse $ object [ "votes" .= newCount ]

incrementMVar :: (Num a) => MVar a -> IO a
incrementMVar mvar = modifyMVar mvar $ \n -> let n' = n + 1 in return (n', n')

-- | Lists all dogs in the database.
dogIndex :: ServerPart Response
dogIndex = do
  dogs <- liftIO allDogs
  flatten $ defaultTemplate "Dogs Index" ()
    [hsx|
      <ul>
        <% for dogs (\dog ->
          <li>
            <a href=(path dog)><% name dog %></a>
          </li>
        ) %>
     </ul>
   |]
  where path = append "/dog/" . toLower . name

-- | Detailed view of a single dog.
viewDog :: String -> ServerPart Response
viewDog = dogResponse <=< liftIO . findDog
  where dogResponse = maybe notFoundError (ok . jsonResponse)

notFoundError :: ServerPart Response
notFoundError = notFound . jsonResponse $
        object [ "error" .= ("not found" :: String) ]

newtype JsonResponse = JsonResponse Value

jsonResponse :: ToJSON a => a -> Response
jsonResponse = toResponse . JsonResponse . toJSON

instance ToMessage JsonResponse where
  toContentType _ = "application/json; charset=utf-8"
  toMessage (JsonResponse value) = encode value
