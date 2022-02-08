module Main where

import World
import Actions_misc

import System.Console.Haskeline

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}
repl state = do outputStrLn (show state)
                outputStr "What now?"
                input <- getInputLine " "
                case input of
                   Just cmd -> do (state', msg) <- operations state $ operationParser cmd
                                  outputStrLn msg
                                  if finished state' then return state'
                                  else if won state' then do outputStrLn winmessage
                                                             return state'
                                  else repl state'
                   Nothing -> do outputStrLn "I do not understand"
                                 repl state              

main :: IO ()
main = do runInputT defaultSettings (repl initState)
          return ()
