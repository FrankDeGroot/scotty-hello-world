{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Web.Scotty

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 routes
