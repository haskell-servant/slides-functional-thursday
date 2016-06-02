{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.Aeson
import           Data.Maybe
import           Data.Proxy
import           GHC.Generics
import           Network.Wai.Handler.Warp as Warp

import           SimpleServant

main :: IO ()
main = do
  putStrLn $ docs myApi
  putStrLn "starting server..."
  Warp.run 8000 $ serve myApi server

type MyApi =
  Get Item

myApi :: Proxy MyApi
myApi = Proxy

data Item
  = Item {
    itemId :: Integer,
    itemData :: String
  }
  deriving (Generic)

instance ToJSON Item

mkExampleItem :: Integer -> Item
mkExampleItem n =
  Item n ("example item: " ++ show n)

server :: Server MyApi
server = NYI
