{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=))
import Data.ConfigFile (OptionSpec, emptyCP, items, readfile)
import Data.Either.Utils (forceEither)
import Database.PostgreSQL.Simple
  ( Only(Only)
  , connect
  , connectDatabase
  , connectPassword
  , connectUser
  , defaultConnectInfo
  , query
  )
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Snap.Core (Snap, getParam, ifTop,writeBS)
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
  xs <- getUsers
  quickHttpServe $ site xs

getUsers :: IO [User]
getUsers = do
  dbCfg <- readDbCfg
  conn <- connect defaultConnectInfo
    { connectDatabase = getCfgItem "database" dbCfg
    , connectUser = getCfgItem "user" dbCfg
    , connectPassword = getCfgItem "password" dbCfg
    }
  query conn "SELECT username, cur_profile_pic FROM users WHERE username=?" $
    Only ("fractalsea" :: String)

readDbCfg :: IO [(OptionSpec, String)]
readDbCfg = do
  val <- readfile emptyCP "db.ini"
  let cp = forceEither val
  return $ forceEither $ items cp "db"

getCfgItem :: OptionSpec -> [(OptionSpec, String)] -> String
getCfgItem k xs = snd $ head $ filter (\(x, _) -> x == k) xs

site :: [User] -> Snap ()
site users = do
  Just param <- getParam "callback"
  writeBS param
  writeBS "("
  ifTop $ writeBS $ BL.toStrict $ A.encode $ head users
  writeBS ")"
