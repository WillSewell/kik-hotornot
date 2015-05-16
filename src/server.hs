{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson ((.=))
import Data.ConfigFile (OptionSpec, emptyCP, items, readfile)
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple
  ( Connection
  , connect
  , connectDatabase
  , connectPassword
  , connectUser
  , defaultConnectInfo
  , query_
  )
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Snap.Core
  ( Snap
  , getParam
  , ifTop
  , modifyResponse
  , route
  , setContentType
  , writeBS
  )
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

type ReaderErrorSnap a = ReaderT Connection (ErrorT String Snap) a

main :: IO ()
main = do
  r <- runErrorT getDbConn
  case r of
   Right conn -> quickHttpServe $ do
     res <- runErrorT $ runReaderT site conn
     case res of
       Right _ -> return ()
       Left e -> error e
   Left e -> error e

getDbConn :: ErrorT String IO Connection
getDbConn = do
  dbCfg <- readDbCfg
  liftIO $ connect defaultConnectInfo
    { connectDatabase = getCfgItem "database" dbCfg
    , connectUser = getCfgItem "user" dbCfg
    , connectPassword = getCfgItem "password" dbCfg
    }

readDbCfg :: ErrorT String IO [(OptionSpec, String)]
readDbCfg = do
  val <- liftIO $ readfile emptyCP "db.ini"
  cp <- toTextError val
  toTextError $ items cp "db"
 where
  toTextError = either (throwError . show) return

getCfgItem :: OptionSpec -> [(OptionSpec, String)] -> String
getCfgItem k xs = snd $ head $ filter (\(x, _) -> x == k) xs

getRandomUser :: ReaderErrorSnap User
getRandomUser = do
  conn <- ask
  res <- liftIO $ query_ conn (
    "SELECT username, cur_profile_pic FROM users "
    <> "OFFSET FLOOR(RANDOM() * (SELECT COUNT(*) FROM users)) LIMIT 1")
  return $ head res

randomUserHandler :: ReaderErrorSnap ()
randomUserHandler = do
  user <- getRandomUser
  Just param <- getParam "callback"
  modifyResponse (setContentType "application/javascript")
  writeBS param
  writeBS "("
  ifTop $ writeBS $ BL.toStrict $ A.encode user
  writeBS ")"

site :: ReaderErrorSnap ()
site = do
  route [("/random-user", randomUserHandler)]
