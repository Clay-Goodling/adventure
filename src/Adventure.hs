{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |Representation of static adventure data.

    This module represents the data stored in adventure files, including
    the rooms and exits.  It handles loading of that data from JSON as well
    as querying the data.
-}
module Adventure
  ( T
  , RoomId
  , ExitName
  , from_json
  , start_room
  , room_ids
  , get_description
  , get_exits
  , next_room
  , next_rooms
  , room_value
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

{- |The type of room identifiers. -}
type RoomId = String

{- |The type of exit names. -}
type ExitName = String

data Exit = Exit { name :: ExitName
                 , destination :: RoomId
                 } deriving (Show, Generic)

instance FromJSON Exit
instance ToJSON Exit

data Room = Room { roomId :: RoomId
                 , description :: String
                 , value :: Int
                 , exits :: [Exit]
                 } deriving (Show, Generic)

instance FromJSON Room
instance ToJSON Room

{- |The abstract type of values representing adventures. -}
newtype T = T Adventure

{- |The concrete type of values representing adventures. -}
data Adventure = Adventure { rooms :: [Room]
                           , startRoom :: RoomId
                           } deriving (Show, Generic)

instance FromJSON Adventure
instance ToJSON Adventure

from_json :: B.ByteString -> Maybe T
from_json json = do
  adv <- decode json
  return $ T adv

{- |[start_room a] is the identifier of the starting room in adventure
    [a]. -}
start_room :: T -> RoomId
start_room (T adv) = startRoom adv

{- |[room_ids a] is a list of all of the room identifiers in
    adventure [a]. -}
room_ids :: T -> [RoomId]
room_ids (T adv) = map roomId (rooms adv)

get_room :: Adventure -> RoomId -> Maybe Room
get_room adv room = get_room' (rooms adv) room where
  get_room' [] _ = Nothing
  get_room' (h:t) room
    | roomId h == room = Just h
    | otherwise = get_room' t room

get_exit :: Room -> ExitName -> Maybe Exit
get_exit room exit = get_exit' (exits room) exit where
  get_exit' [] _ = Nothing
  get_exit' (h:t) exit
    | name h == exit = Just h
    | otherwise = get_exit' t exit

{- |[description a r] is the description of room [r] in adventure [a]. -}
get_description :: T -> RoomId -> Maybe String
get_description (T adv) room = do
  room <- get_room adv room
  return $ description room

{- |[exits a r] is a set-like list of all exit names from room [r] in
    adventure [a]. -}
get_exits :: T -> RoomId -> Maybe [ExitName]
get_exits (T adv) room = do
  room <- get_room adv room
  return $ map name $ exits room

{- |[next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a]. -}
next_room :: T -> RoomId -> ExitName -> Maybe RoomId
next_room (T adv) room exit = do
  room <- get_room adv room
  exit <- get_exit room exit
  return $ destination exit

{- |[next_rooms a r] is a set-like list of all rooms to which there is an exit
    from [r] in adventure [a]. -}
next_rooms :: T -> RoomId -> Maybe [RoomId]
next_rooms (T adv) room = do
  room <- get_room adv room
  return $ map destination $ exits room

{- |[room_value a r] is the value of room [r] in adventure [a]. -}
room_value :: T -> RoomId -> Maybe Int
room_value (T adv) room = do
  room <- get_room adv room
  return $ value room
