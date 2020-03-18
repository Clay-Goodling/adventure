{- |The main entry point for the game interface. -}
module Main where

import Control.Exception
import qualified Data.ByteString.Lazy as B
import Data.List
import System.IO
import System.IO.Error

import qualified Adventure as Adv
import qualified Command as Cmd
import qualified State as St

quit :: Adv.T -> St.T -> IO ()
quit _ _ = return ()

go :: Adv.ExitName -> Adv.T -> St.T -> IO ()
go exit adv st = case St.go exit adv st of
  Nothing -> do
    putStrLn "You can't go that way."
    main_loop adv st
  Just st -> main_loop adv st

score :: Adv.T -> St.T -> IO ()
score adv st = do
  putStr "Score: "
  putStrLn $ show $ St.get_score st
  main_loop adv st

empty :: Adv.T -> St.T -> IO ()
empty adv st = do
  putStrLn "What?"
  main_loop adv st

malformed :: Adv.T -> St.T -> IO ()
malformed adv st = do
  putStrLn "I don't understand."
  main_loop adv st

main_loop :: Adv.T -> St.T -> IO ()
main_loop adv st = do
  putStrLn ""
  case Adv.get_description adv $ St.current_room_id st of
    Nothing -> return ()
    Just s  -> putStrLn s
  putStr "> "
  hFlush stdout
  input <- getLine
  ( case Cmd.parse input of
    Cmd.Quit      -> quit
    Cmd.Go e      -> go $ concat $ intersperse " " e
    Cmd.Score     -> score
    Cmd.Empty     -> empty
    Cmd.Malformed -> malformed
    ) adv st

main :: IO ()
main = catch (
  do
    putStrLn ""
    putStrLn "Welcome to the Adventure game engine."
    putStrLn "Please enter the name of the game you want to run."
    putStr "> "
    hFlush stdout
    fileName <- getLine
    json <- B.readFile $ "adventures/" ++ fileName ++ ".json"
    case Adv.from_json json of
      Nothing -> putStrLn "Failed to parse JSON."
      Just adv -> case St.init_state adv of
        Nothing -> putStrLn "Failed to initialize state."
        Just st -> main_loop adv st
  ) (\ex -> if isEOFError ex then return () else putStrLn "Invalid File")
