module World where

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String,
                    obj_charges :: Int }
   deriving Eq

instance Show Object where
   show obj = obj_longname obj

data Direction = North | West | South | East | Out | In
   deriving Eq

data Exit = Exit { exit_dir :: Direction,
                   exit_desc :: String,
                   room :: String }
   deriving Eq

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           finished :: Bool, -- set to True at the end
                           caffeineLevel :: Int, --How caffeinated is the player
                           stress :: Int --How stressed is the player 
                         }

-- Checks the win condition (the player being in the street)
won :: GameData -> Bool
won gd = location_id gd == "street"

--Checks if the player has met any of the lose conditions (too stressed or too caffeinated)
isDead :: GameData -> Bool
isDead player = caffeineLevel player > 2 || stress player > 2

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
                                  

instance Show GameData where
    show gd = show (getRoomData gd)

-- Things which do something to an object and update the game state
type Action  = Object -> GameData -> (GameData, String)

-- Things which move the player from the current room in a given direction, updating the game state
type Action'  = Direction -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)

-- Objects

mug, fullmug, coffeepot :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug" 0
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee" 3
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee" 3


note0, note1, note2, note3, note4 :: Object
note0     = Obj "note0" "note0" "you = 0 ; you + coffee = 20" 0
note1     = Obj "note1" "note1" "To do: Find my key" 0
note2     = Obj "note2" "note2" "Where's the face masks?" 0
note3     = Obj "note3" "note3" "From Dad: Is there something inside your dirty clothes?" 0
note4     = Obj "note4" "note4" "From Mom: There are some face masks in the garage." 0

mask, key, door :: Object
mask      = Obj "mask" "face mask" "Face mask to protect yourself" 0
key       = Obj "key" "key" "Key to open the door in the hallway" 0
door      = Obj "door" "door" "Door in the hallway" 0

-- Rooms

bedroom, toilet, kitchen, hall, garage, street :: Room

bedroom = Room "You are in your bedroom."
               [Exit North "To the north is a kitchen. " "kitchen",
                Exit South "To the south is a toilet. " "toilet"]
               [mug,note1,note2]

toilet = Room "You are in your toilet."
               [Exit South "To the south is a your bedroom. " "bedroom"]
               [key]

kitchen = Room "You are in the kitchen."
               [Exit South "To the south is your bedroom. " "bedroom",
                Exit West "To the west is a hallway. " "hall"]
               [coffeepot, note3, note4]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " "kitchen",
             Exit West "To the west is a garage. " "garage"]
            []

garage = Room "You are in the garage."
               [Exit West "To the west is a hallway. " "hall"]
               [mask]

-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit East "To the east is a kitchen. " "kitchen",
               Exit Out "You can go outside. " "street"]

street = Room "You have made it out of the house."
              [Exit In "You can go back inside if you like. " "hall"]
              []

gameworld = [("bedroom", bedroom),
             ("toilet", toilet),
             ("kitchen", kitchen),
             ("hall", hall),
             ("garage", garage),
             ("street", street)]

-- Initial game state

initState :: GameData
initState = GameData "bedroom" gameworld [] False False 0 0

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))
