{- |The main entry poit for the game interface. -}
module Main where

import qualified Data.ByteString.Lazy as B

import qualified Adventure as Adv
import qualified Command as Cmd
import qualified State as St

main :: IO ()
main = do
  file <- B.readFile "adventures/lonely_room.json"
  putStrLn $ case Adv.from_json file of
    Just t -> "success"
    Nothing -> "failure"
