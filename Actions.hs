module Actions where

import World

actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

directions :: String -> Maybe Direction
directions "north"   = Just North
directions "west"    = Just West
directions "south"   = Just South
directions "east"    = Just East
directions _         = Nothing

object :: String -> Maybe Object {-objects to be consistent or object to be distinctive ?-}
object "mug"        = Just mug {-cannot distinguish full and normal mug ?!-}
object "coffee"     = Just coffeepot
object _            = Nothing

rooms :: String -> Maybe Room
rooms "bedroom"     = Just bedroom
rooms "kitchen"     = Just kitchen
rooms "hall"        = Just hall
rooms "street"      = Just street
rooms _             = Nothing

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: String -> Room -> Maybe String
move dir rm | null tar = Nothing
            | otherwise = Just $ head tar
   where tar = [room id | id <- exits rm, dir == exit_dir id]

{- Return True if the object appears in the room. -}

objectHere :: String -> Room -> Bool
objectHere o rm = or [o == obj | obj <- map obj_name $ objects rm]

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: String -> Room -> Room
removeObject o rm = rm {objects = [obj | obj <- objects rm, o /= obj_name obj]}

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm {objects = o : objects rm}

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: String -> [Object] -> Object
findObj o ds = tar where (tar:_) = [obj | obj <- ds, o == obj_name obj]

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Object
objectData o rm = findObj o $ objects rm

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = if or [rmid == fst rm | rm <- world gd]
                            then gd {world = [if rmid == fst rm then (fst rm, rmdata) else rm | rm <- world gd]}
                            else gd {world = world gd ++ [(rmid, rmdata)]}

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> String -> GameData
addInv gd obj = gd {inventory = inventory gd ++ [item | item <- objects $ getRoomData gd, obj_name item == obj]}

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> String -> GameData
removeInv gd obj = gd {inventory = filter (\tar -> obj_name tar /= obj) $ inventory gd}

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> String -> Bool
carrying gd obj = or [obj == obj_name item | item <- inventory gd]

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state =
   case move dir (getRoomData state) of
      Nothing -> (state, "Room error")
      Just newroom -> (state { location_id = newroom }, "Moved to new room")

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: Action
get obj state | objectHere obj rm = (state', "Object got!")
              | otherwise         = (state, "Object not found!")
   where state' = updateRoom (addInv state obj) rmid (removeObject obj rm)
         rm = getRoomData state
         rmid = location_id state

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state | carrying state obj = state'
              | otherwise          = (state, "Object not found!")
   where state'= (updateRoom (removeInv state obj) rmid (addObject tar rm), "Object put!")
         rm = getRoomData state
         rmid = location_id state
         (tar:_) = [item | item <- inventory state, obj_name item == obj]

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state | carrying state obj = (state, item)
                  | objectHere obj rm  = (state, obj_desc $ objectData obj rm)
                  | otherwise          = (state, "Object not found!")
  where
      rm = getRoomData state
      rmid = location_id state
      (item:_) = [obj_desc elem | elem <- inventory state, obj_name elem == obj]

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state = if carrying state "coffee" && carrying state obj
                 then (state {inventory = [if obj == mug then fullmug else obj | obj <- inventory state]},
                       "Got a full mug of coffee!")
                 else (state, "Missing coffee pot or mug!")

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state = if or [fullmug == item | item <- inventory state]
                  then (state {inventory = [if obj == mug then fullmug else obj | obj <- inventory state],
                               caffeinated = True},
                       "Drank a mug of coffee. You feel more energetic now!")
                  else (state, "Missing a full mug of coffee!")

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Action
open obj state | caffeinated state = (updateRoom state "hall" hall', "Door opened!")
               | otherwise         = (state, "You not energetic enough for a walk!")
   where hall' = hall {room_desc = openedhall, exits = exits hall ++ openedexits}

{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")
{-
save :: GameData -> String -> IO ()
save gd fname = writeFile path content
   where path = ".\\" ++ fname
         content = location_id gd ++ " " ++
                   tupleToString (world gd) ++ " " ++
                   listToString (inventory gd) ++ " " ++
                   boolToString (poured gd) ++ " " ++
                   boolToString (caffeinated gd) ++ " " ++
                   boolToString (finished gd)

--load :: String -> IO GameData

listToString :: [Object] -> String
listToString xs = "[" ++ foldr (\x rest -> obj_name x ++ "," ++ rest) [] xs ++ "]"

--stringToList ::  String -> [Object]

tupleToString :: [(String, Room)] -> String
tupleToString xs = "[" ++ foldr (\x rest -> x ++ "," ++ rest) [] [fst elem | elem <- xs] ++ "]"

--tupleToString :: String -> [(String, Room)] 

boolToString :: Bool -> String
boolToString bool = if bool then "True" else "False"

stringToBool :: String -> Bool
stringToBool string = string == "True"
-}