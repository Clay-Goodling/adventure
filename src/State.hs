{- |Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
-}
module State
  ( T
  , init_state
  , current_room_id
  , get_visited
  , get_score
  , go
  ) where

import qualified Adventure as Adv

{- |The concrete type of values representing the game state. -}
data State = State { currentRoom :: Adv.RoomId
                   , score :: Int
                   , visited :: [Adv.RoomId]
                   } deriving Show

{- |The abstract type of values representing the game state. -}
newtype T = T State

{- |[init_state a] is the initial state of the game when playing adventure [a].
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. -}
init_state :: Adv.T -> Maybe T
init_state adv = do
  let room = Adv.start_room adv
  score <- Adv.room_value adv room
  return $ T $ State { currentRoom = room
            , score = score
            , visited = [room]
            }

{- |[current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. -}
current_room_id :: T -> Adv.RoomId
current_room_id (T st) = currentRoom st

{- |[visited st] is a set-like list of the room identifiers the adventurer has
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. -}
get_visited :: T -> [Adv.RoomId]
get_visited (T st) = visited st


get_score :: T -> Int
get_score (T st) = score st
{- |[go exit adv st] is [r] if attempting to go through exit [exit] in state
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the
    adventurer's current room, then [r] is [Legal st'], where in [st'] the
    adventurer is now located in the room to which [exit] leads.  Otherwise,
    the result is [Illegal].
    Effects: none.  [go] is not permitted to do any printing. -}
go :: Adv.ExitName -> Adv.T -> T -> Maybe T
go exit adv (T st) = do
  room <- Adv.next_room adv (currentRoom st) exit
  value <- Adv.room_value adv room
  return $ T $ State { currentRoom = room
                     , score = score st + if elem room v then 0 else value
                     , visited = if elem room v then v else room:v
                     } where
                       v = visited st
