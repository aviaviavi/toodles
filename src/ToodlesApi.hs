{-# LANGUAGE DataKinds,
             ScopedTypeVariables,
             TypeOperators #-}

module ToodlesApi where

import Types

import           Data.Proxy         (Proxy)
import           Data.Text          (Text)
import           Servant
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html5   (Html)

type ToodlesAPI = "todos" :> QueryFlag "recompute" :> Get '[JSON] TodoListResult

             :<|> "todos" :> "delete" :> ReqBody '[JSON] DeleteTodoRequest :> Post '[JSON] Text

             :<|> "todos" :> "edit" :> ReqBody '[JSON] EditTodoRequest :> Post '[JSON] Text

             :<|> "static" :> Raw

             :<|> "source_file" :> Capture "id" Integer :> Get '[HTML] Html

             :<|> CaptureAll "anything-else" Text :> Get '[HTML] Html

toodlesAPI :: Proxy ToodlesAPI
toodlesAPI = Proxy
