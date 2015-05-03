{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=))
import Data.ConfigFile (OptionSpec, emptyCP, items, readfile)
import Data.Either.Utils (forceEither)
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple
  ( connect
  , connectDatabase
  , connectPassword
  , connectUser
  , defaultConnectInfo
  , query_
  )
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Snap.Core (Snap, getParam, ifTop, writeBS)
import Snap.Http.Server (quickHttpServe)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data User = User
  { username   :: T.Text
  , profilePic :: T.Text
  }

instance FromRow User where
  fromRow = User <$> field <*> field

instance Show User where
  show (User { username = u, profilePic = p }) =
    "Project { username: " ++ T.unpack u ++ ", profilePic: " ++ T.unpack p ++ " }"

instance A.ToJSON User where
  toJSON (User { username = u, profilePic = p }) = A.object
    [ "username" .= u
    , "profilePic" .= p
    ]

main :: IO ()
main = do
  u <- getRandomUser
  quickHttpServe $ site u

getRandomUser :: IO User
getRandomUser = do
  dbCfg <- readDbCfg
  conn <- connect defaultConnectInfo
    { connectDatabase = getCfgItem "database" dbCfg
    , connectUser = getCfgItem "user" dbCfg
    , connectPassword = getCfgItem "password" dbCfg
    }
  res <- query_ conn $
    "SELECT username, cur_profile_pic FROM users "
    <> "OFFSET RANDOM() * (SELECT COUNT(*) FROM users) LIMIT 1"
  return $ head res

readDbCfg :: IO [(OptionSpec, String)]
readDbCfg = do
  val <- readfile emptyCP "db.ini"
  let cp = forceEither val
  return $ forceEither $ items cp "db"

getCfgItem :: OptionSpec -> [(OptionSpec, String)] -> String
getCfgItem k xs = snd $ head $ filter (\(x, _) -> x == k) xs

site :: User -> Snap ()
site user = do
  Just param <- getParam "callback"
  writeBS param
  writeBS "("
  ifTop $ writeBS $ BL.toStrict $ A.encode user
  writeBS ")"
