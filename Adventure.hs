module Main where

import World
import Actions
import Parsing

import Control.Monad
import System.IO
import System.Exit

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do print state
                putStr "What now? "
                hFlush stdout
                cmd <- getLine
                let (state', msg) = operations state $ operationParser cmd
                putStrLn msg
                if won state' then do putStrLn winmessage
                                      return state'
                               else repl state'

main :: IO ()
main = do repl initState
          return ()
