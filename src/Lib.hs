{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics
import           Web.Scotty

data User =
  User
    { userId   :: Int
    , userName :: String
    }
  deriving (Generic, Show)

instance ToJSON User

instance FromJSON User

bob :: User
bob = User {userId = 1, userName = "bob"}

jenny :: User
jenny = User {userId = 2, userName = "jenny"}

allUsers :: [User]
allUsers = [bob, jenny]

routes :: ScottyM ()
routes = do
  getHelloName
  getUsers
  getUserById
  postUser

getHelloName :: ScottyM ()
getHelloName = get "/hello/:name" $ do
    name <- param "name"
    text ("hello " <> name <> "!")

getUsers :: ScottyM ()
getUsers = get "/users" $ json allUsers   

getUserById :: ScottyM ()
getUserById = get "/users/:id" $ do
    someId <- param "id"
    json (filter (matchesId someId) allUsers)

postUser :: ScottyM ()
postUser = post "/users" $ do
  user <- jsonData :: ActionM User
  json user

matchesId :: Int -> User -> Bool
matchesId someId user = userId user == someId
