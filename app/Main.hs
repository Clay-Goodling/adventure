module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

import qualified Adventure as Adv

main :: IO ()
main = do
  file <- B.readFile "adventures/lonely_room.json"
  putStrLn $ case Adv.from_json file of
    Just t -> "success"
    Nothing -> "failure"
