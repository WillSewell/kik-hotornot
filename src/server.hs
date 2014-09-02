{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Data.Aeson
import           Data.ConfigFile
import           Data.Either.Utils
import qualified Data.Text                          as Text
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Snap.Core
import           Snap.Extras.JSON
import           Snap.Http.Server

data User = User { username   :: Text.Text
                 , profilePic :: Text.Text
                 }

instance FromRow User where
  fromRow = User <$> field <*> field

instance Show User where
    show (User {username=u, profilePic=p}) =
      "Project { username: " ++ Text.unpack u ++ ", profilePic: " ++ Text.unpack p ++ " }"

instance ToJSON User where
 toJSON (User {username=u, profilePic=p}) =
    object [ "username" .= u
           , "profilePic" .= p
           ]

main :: IO ()
main = do
  xs <- getUsers
  quickHttpServe $ site xs

getUsers :: IO [User]
getUsers = do
    dbCfg <- readDbCfg
    conn <- connect defaultConnectInfo { connectDatabase = getCfgItem "database" dbCfg
                                       , connectUser = getCfgItem "user" dbCfg
                                       , connectPassword = getCfgItem "password" dbCfg
                                       }
    query conn "SELECT username, cur_profile_pic FROM users WHERE username=?" $ Only ("fractalsea" :: String)

readDbCfg :: IO [(OptionSpec, String)]
readDbCfg = do
    val <- readfile emptyCP "../db.ini"
    let cp = forceEither val
    return $ forceEither $ items cp "db"

getCfgItem :: OptionSpec -> [(OptionSpec, String)] -> String
getCfgItem k xs = snd $ head $ filter (\(x, _) -> x == k) xs

site :: [User] -> Snap ()
site users = do
  Just param <- getParam "callback"
  writeBS param
  writeBS "("
  ifTop $ writeJSON $ head users
  writeBS ")"
