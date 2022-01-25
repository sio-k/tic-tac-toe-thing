module Main where

import Lib

import SDL
import SDL.Video.Renderer

import Data.IORef

data ClientInfo = ClientInfo { session :: ClientSession
                             } -- TODO: mutable structure containing what server we're connecting to etc.

main :: IO ()
main = do
  initializeAll -- init all SDL subsystems (this is probably fine)
  window <- createWindow "Tic-Tac-Toe reference client" $
              defaultWindow { windowPosition  = Centered
                            , windowResizable = True
                            }
  renderer <- createRenderer window -1 defultRenderer
  client <- error "TODO: init servant client gunk; create session"
  mainLoop window renderer client
  destroyRenderer renderer
  destroyWindow window

mainLoop :: Window -> Renderer -> ClientInfo -> IO ()
mainLoop window renderer ci = do
  (V2 w h) <- get $ windowSize window
  ia <- inputActions window ci w h
  if not $ null $ filter (== InputQuit) ia
    then do
      error "TODO: notify server that we're quitting and close the connection"
      pure ()
    else do
      error "TODO: apply state changes locally, if okay to do so"
      error "TODO: check whether we got any state changes from the server, and apply any of those"
      clear renderer
      error "TODO: rendering of our game's state"
      present renderer
      mainLoop window renderer ci

data InputAction = InputQuit | InputMark Int32 Int32 deriving (Eq)

inputActions :: Window -> ClientInfo -> Int32 -> Int32 -> IO [InputAction]
inputActions window ci w h =
  let f :: Event -> [InputAction]
      f (Event _timestamp payload) =
        case payload of
          WindowClosedEvent _  -> pure InputQuit
          KeyboardEvent ke     -> keyboardEvent
          MouseButtonEvent me  -> mouseClick
          QuitEvent            -> pure InputQuit
          TouchFingerEvent tfe -> touchEvent
          _                    -> [] -- ignore everything else

      keyboardEvent :: KeyboardEventData -> [InputAction]
      keyboardEvent (KeyboardEventData _ Pressed _ KeycodeEscape) = pure InputQuit
      keyboardEvent _ = []

      mouseClick :: MouseButtonEventData -> [InputAction]
      mouseClick (MouseButtonEventData _ Released _ ButtonLeft _ (Point (V2 x y))) =
        processClick x y
      mouseClick _ = []

      touchEvent :: TouchFingerEventData -> [InputAction]
      touchEvent (TouchFingerEventData _ _ Released (Point (V2 x y)) _) =
        processClick x y
      touchEvent _ = []

      processClick :: Int32 -> Int32 -> [InputAction]
      processClick x y
        | x > w || y > h = [] -- out of bounds
        | otherwise = error "TODO: check whether any field on the screen was clicked"

  in (f =<<) <$> pollEvents
