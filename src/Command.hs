{- |Parsing of player commands. -}
module Command
  ( ObjectPhrase
  , Command(..)
  , parse
  ) where

{- |The type [object_phrase] represents the object phrase that can be part of a
    player command.  Each element of the list represents a word of the object
    phrase, where a {i word} is defined as a consecutive sequence of non-space
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words
    in the original player command.  For example:
    - If the player command is ["go clock tower"], then the object phrase is
      [["clock"; "tower"]].
    - If the player command is ["go clock     tower"], then the object phrase is
      again [["clock"; "tower"]].

    An [object_phrase] is not permitted to be the empty list. -}
type ObjectPhrase = [String]

{- |The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. -}
data Command = Go ObjectPhrase
             | Quit
             | Score
             | Empty
             | Malformed

{- |[parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the object phrase.
    Examples:
    - [parse "    go   clock   tower   "] is [Go ["clock"; "tower"]]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.). -}
parse :: String -> Command
parse s = case filter (\x -> x /= "") $ words s of
  "go":[] -> Malformed
  "go":t  -> Go t
  "quit":[] -> Quit
  "score":[] -> Score
  [] -> Empty
  _ -> Malformed
