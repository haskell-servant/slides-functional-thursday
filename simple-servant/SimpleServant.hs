{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SimpleServant where

import           Control.Monad
import           Data.Aeson
import           Data.Proxy
import           Data.String.Conversions
import           Data.Typeable
import           GHC.TypeLits
import           Network.HTTP.Types
import           Network.Wai
import           Text.Read

data Get (a :: *)

data (x :: k) :> api
infixr 5 :>

data a :<|> b
  = a :<|> b
infixr 4 :<|>

data QueryParam (name :: Symbol) (param :: *)

-- docs

docs :: HasDocs api => Proxy api -> String
docs = unlines . docsLines

class HasDocs api where
  docsLines :: Proxy api -> [String]

instance
  HasDocs (Get a) where

  docsLines = error "NYI: Get"

instance
  HasDocs ((path :: Symbol) :> api) where

  docsLines = error "NYI: paths"

instance
  HasDocs (a :<|> b) where

  docsLines = error "NYI: :<|>"

instance
  HasDocs (QueryParam name param :> api) where

  docsLines = error "NYI: QueryParam"

-- server

serve :: HasServer api => Proxy api
  -> Server api -> Application
serve proxy server request send =
  case route proxy server request of
    Just mkResponse ->
      mkResponse >>= send
    Nothing ->
      send $ responseLBS status500 [] "error!"

class HasServer api where
  type Server api :: *
  route :: Proxy api -> Server api
    -> Request -> Maybe (IO Response)

data NYI = NYI

instance
  HasServer (Get a) where

  type Server (Get a) = NYI
  route = error "NYI: Get"

instance
  HasServer ((path :: Symbol) :> api) where

  type Server (path :> api) = NYI
  route = error "NYI: paths"

instance
  HasServer (a :<|> b) where

  type Server (a :<|> b) = NYI
  route = error "NYI: :<|>"

instance
  HasServer (QueryParam name param :> api) where

  type Server (QueryParam name param :> api) = NYI
  route = error "NYI: QueryParam"
