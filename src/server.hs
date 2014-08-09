{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as Text
import Data.ConfigFile
import Data.Either.Utils

data User = User { username :: Text.Text
                 , profilePic :: Text.Text
                 }

instance FromRow User where
  fromRow = User <$> field <*> field

instance Show User where
    show (User username profilePic) =
      "Project { username: " ++ Text.unpack username ++ ", profilePic: " ++ Text.unpack profilePic ++ " }"

main = do
    dbCfg <- readDbCfg
    conn <- connect defaultConnectInfo { connectDatabase = getCfgItem "database" dbCfg
                                       , connectUser = getCfgItem "user" dbCfg
                                       , connectPassword = getCfgItem "password" dbCfg
                                       }
    xs <- query conn "SELECT username, cur_profile_pic FROM users WHERE username=?" $ Only ("fractalsea" :: String)
    liftIO $ print (xs :: [User])

readDbCfg = do
    val <- readfile emptyCP "../db.ini"
    let cp = forceEither val
    return $ forceEither $ items cp "db"

getCfgItem k xs = snd $ head $ filter (\(x, _) -> x == k) xs
