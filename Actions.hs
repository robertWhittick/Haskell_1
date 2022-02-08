module Actions where

import World

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists. -}
move :: Direction -> Room -> Maybe String
move dir rm | null tar = Nothing
            | otherwise = Just $ head tar
   where tar = [room id | id <- exits rm, dir == exit_dir id]

{- Return True if the object appears in the room. -}
objectHere :: Object  -> Room -> Bool
objectHere o rm = or [o == obj | obj <- objects rm]

{- Given an object id and a room description, return a new room description
   without that object -}
removeObject :: Object -> Room -> Room
removeObject o rm = rm {objects = [obj | obj <- objects rm, o /= obj]}

{- Given an object and a room description, return a new room description
   with that object added -}
addObject :: Object -> Room -> Room
addObject o rm = rm {objects = o : objects rm}

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}
findObj :: Object -> [Object] -> Object
findObj o ds = tar where (tar:_) = [obj | obj <- ds, o == obj]

{- Use 'findObj' to find an object in a room description -}
objectData :: Object -> Room -> Object
objectData o rm = findObj o $ objects rm

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}
updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = if or [rmid == fst rm | rm <- world gd]
                            then gd {world = [if rmid == fst rm then (fst rm, rmdata) else rm | rm <- world gd]}
                            else gd {world = world gd ++ [(rmid, rmdata)]}

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}
addInv :: GameData -> Object -> GameData
addInv gd obj = gd {inventory = inventory gd ++ [item | item <- objects $ getRoomData gd, item == obj]}

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}
removeInv :: GameData -> Object -> GameData
removeInv gd obj = gd {inventory = filter (/= obj) $ inventory gd}

{- Given a game state and an object id, find if it is inside 
   player's inventory. -}
carrying :: GameData -> Object -> Bool
carrying gd obj = or [obj == item | item <- inventory gd]

{- Given a Direction and a GameData, update the GameData with a message.
   The original GameData and an error message is returned if the player
   cannot move in that direction. -}
go :: Action'
go dir state =
   case move dir (getRoomData state) of
      Nothing -> (state, "Room error")
      Just newroom -> (state { location_id = newroom }, "Moved to new room")

{- Remove an Object from the current room and add it to inventory.
   Only works if the Object is in the current room.
   Also gives back a message to be displayed to the user. -}
get :: Action
get obj state | objectHere obj rm = (state', "Object got!")
              | otherwise         = (state, "Object not found!")
   where state' = updateRoom (addInv state obj) rmid (removeObject obj rm)
         rm = getRoomData state
         rmid = location_id state

{- Remove an Object from inventory and place it in the room.
   Only works if the Object is in inventory.
   Also gives back a message to be displayed to the user. -}
put :: Action
put obj state | carrying state obj = state'
              | otherwise          = (state, "Object not found!")
   where state'= (updateRoom (removeInv state obj) rmid (addObject tar rm), "Object put!")
         rm = getRoomData state
         rmid = location_id state
         (tar:_) = [item | item <- inventory state, item == obj]

{- Examines an Object that is either in the current Room or in inventory.
   A message describing the Object is displayed if found.
   An error message otherwise. -}
examine :: Action
examine obj state | carrying state obj = (state, item)
                  | objectHere obj rm  = (state, obj_desc $ objectData obj rm)
                  | otherwise          = (state, "Object not found!")
  where
      rm = getRoomData state
      rmid = location_id state
      (item:_) = [obj_desc elem | elem <- inventory state, elem == obj]

{- Pours coffee into the mug if and only if the player is carrying 
   both the coffee pot and the mug. -}
pour :: Action
pour obj state = if carrying state coffeepot && carrying state obj
                 then (state {inventory = [if obj == mug then fullmug else obj | obj <- inventory state]},
                       "Got a full mug of coffee!")
                 else (state, "Missing coffee pot or mug!")

{- Drink coffee. Only works if the player is carrying a *full* mug. -}
drink :: Action
drink obj state = if or [fullmug == item | item <- inventory state]
                  then (state {inventory = [if obj == mug then fullmug else obj | obj <- inventory state],
                               caffeinated = True},
                       "Drank a mug of coffee. You feel more energetic now!")
                  else (state, "Missing a full mug of coffee!")

{- Opens the door if in the hall and updates the Room to be able to go Out.
   Only works if the player has drank coffee & has the key and the mask. -}
open :: Action
open obj state | caffeinated state &&
                 carrying state key &&
                 carrying state mask = (updateRoom state "hall" hall', "Door opened!")
               | otherwise           = (state, "You are not ready for a walk!")
   where hall' = hall {room_desc = openedhall, exits = exits hall ++ openedexits}

{- Lists player's inventory. -}
inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

{- Quits the game.-}
quit :: Command
quit state = (state { finished = True }, "Bye bye")