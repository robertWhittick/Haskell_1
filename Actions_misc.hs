{- This module only stores the parser, save & load functions.
   Actions/ commands are at Actions.hs -}

{-# LANGUAGE ParallelListComp #-}
module Actions_misc where

import Actions
import World
import Parsing
import System.Console.Haskeline
import Control.Monad.IO.Class

{- Converts an Operation to the resulting GameData after 
   applying the Operation and a message for the user through intermediary functions. -}
operations :: MonadIO m => GameData -> Operation -> InputT m (GameData, String)
operations state cmd = case cmd of
                        Go x -> return (go x state)
                        Get x -> return (get x state)
                        Drop x -> return (put x state)
                        Examine x -> return (examine x state)
                        Pour x -> return (pour x state)
                        Drink x -> return (drink x state)
                        Open x -> return (open x state)
                        Inv -> return (inv state)
                        Save x -> do liftIO $ save state x
                                     return (state, "Saved.")
                        Load x -> do state' <- liftIO $ load x
                                     return (state', "Loaded")
                        Quit -> return (quit state)
                        _ -> return (state, "I don't understand")

{- Turns a String (input from the user) into an Operation. -}
operationParser :: String -> Operation
operationParser cmd = case split2 cmd of
     ["go",arg] -> maybe Error Go (directions arg)
     ["get",arg] -> maybe Error Get (object arg)
     ["drop",arg] -> maybe Error Drop (object arg)
     ["examine",arg] -> maybe Error Examine (object arg)
     ["pour",arg] -> maybe Error Pour (object arg)
     ["drink",arg] -> maybe Error Drink (object arg)
     ["open",arg] -> maybe Error Open (object arg)
     ["inventory",_] -> Inv
     ["save",arg] -> Save arg
     ["load",arg] -> Load arg
     ["quit",_] -> Quit
     _ -> Error

--Converts a String into a Direction
directions :: String -> Maybe Direction
directions x = case x of
                  "north" -> Just North
                  "south" -> Just South
                  "west" -> Just West
                  "east" -> Just East
                  "out" -> Just Out
                  "in" -> Just In
                  _ -> Nothing

--Converts a String into an Object
object :: String -> Maybe Object
object x = case x of
            "mug" -> Just mug
            "full mug" -> Just fullmug
            "coffee" -> Just coffeepot
            "note0" -> Just note0
            "note1" -> Just note1
            "note2" -> Just note2
            "note3" -> Just note3
            "note4" -> Just note4
            "mask" -> Just mask
            "key" -> Just key
            "door" -> Just door
            _ -> Nothing

--Converts a String into a Room
rooms :: String -> Maybe Room
rooms x = case x of
            "bedroom" -> Just bedroom
            "toilet" -> Just toilet
            "kitchen" -> Just kitchen
            "hall" -> Just hall
            "garage" -> Just garage
            "street" -> Just street
            _ -> Nothing

savePath = ".\\save\\"

{- Saves the game. -}
save :: GameData -> String -> IO ()
save gd fname = writeFile (savePath ++ fname) content
   where content = init (saveGame gd ++ --'init' removes the trailing '\n'
                         saveRoom (map snd $ world gd))

{- Saves the current GameData. -}
saveGame :: GameData -> [Char]
saveGame gd = location_id gd ++ " " ++ --' ' is used to separate different attributes
              tupleToString (world gd) ++ " " ++
              show (inventory gd) ++ " " ++
              show (poured gd) ++ " " ++
              show (caffeinated gd) ++ " " ++
              show (opened gd) ++ " " ++
              show (finished gd) ++ "\n" --'\n' is used to separate different data

{- Saves the current room (objects only).
   The object name is parsed in a more complicated way
   not to use the predefined show instance as the object parser
   does not recognise the full name of the objects. -}
saveRoom :: [Room] -> [Char]
saveRoom (x:xs) = show (map obj_name $ objects x) ++ "\n" ++
                  saveRoom xs
saveRoom [] = []

{- Converts a tuple containing a String and a Room to a String.
   (Used to save the "world" in GameData to a save file) -}
tupleToString :: [(String, Room)] -> String
tupleToString xs = "[" ++ init (foldr (\x rest -> x ++ "," ++ rest) [] [fst elem | elem <- xs]) ++ "]"

{- Loads a game from a save file. -}
load :: [Char] -> IO GameData
load fname = do content <- readFile $ savePath ++ fname
                return $ loadGame $ split '\n' content --this splits the file into lines

{- Loads the GameData and all Room data. -}
loadGame :: [String] -> GameData
loadGame dat = gd5
   where list = [(fst x,y) | x <- world base | y <- tail dat] --Room data and corresponding room ID
         xs = split ' ' $ head dat --The GameData in text format
         gd0 = loadRoom base (head list) --software pipelining
         gd1 = loadRoom gd0 (list !! 1)
         gd2 = loadRoom gd1 (list !! 2)
         gd3 = loadRoom gd2 (list !! 3)
         gd4 = loadRoom gd3 (list !! 4)
         gd5 = loadRoom gd4 (list !! 5) --Final room
         base = GameData (head xs) --The GameData inside the file
                (stringToTuple $ xs !! 1)
                (stringToList $ xs !! 2)
                (stringToBool $ xs !! 3)
                (stringToBool $ xs !! 4)
                (stringToBool $ xs !! 5)
                (stringToBool $ xs !! 6)

{- Loads a Room data. -}
loadRoom :: GameData -> (String, String) -> GameData
loadRoom gd dat = updateRoom gd rmid new
   where new = Room desc exit (stringToList $ snd dat)
         rmid = fst dat
         rm = maybe undefined id (rooms rmid)
         desc = if rm == hall && opened gd --Check if the door is opened
                then openedhall 
                else room_desc rm
         exit = if rm == hall && opened gd --Check if the door is opened
                then exits rm ++ openedexits
                else exits rm

{- Converts a String to a list of Objects. -}
stringToList ::  String -> [Object]
stringToList xs = if list == [""] then [] else map go list
   where list = split ',' $ init $ tail xs
         go x = maybe undefined id (object $ init $ tail x)

{- Converts a String to a tuple containing a String and a Room.
   (Used to load the "world" in GameData from a save file) -}
stringToTuple :: String -> [(String, Room)]
stringToTuple xs = map go $ split ',' $ init $ tail xs
   where go x = case rooms x of
                  Just sth -> (x, sth)
                  Nothing -> undefined

{- Converts a String to a Bool. -}
stringToBool :: String -> Bool
stringToBool str = str == "True"