-- Copyright (C) 2022 Sio Kreuzer. All Rights Reserved.
module Input where

import ClientInfo
import FieldCoords
import SDL
import Lib

data InputAction = InputQuit
                 | InputMark { markX :: Int
                             , markY :: Int
                             } -- x, y coords in fields vec
                 deriving (Eq)

inputActions :: Window -> ClientInfo -> Int32 -> Int32 -> IO [InputAction]
inputActions window
             ci@(ClientInfo (ClientSession _ (Game _ (GameConfig _ bs) _) _ _))
             width
             height =
  let f :: Event -> [InputAction]
      f (Event _timestamp payload) =
        case payload of
          WindowClosedEvent _  -> pure InputQuit
          KeyboardEvent ke     -> keyboardEvent ke
          MouseButtonEvent me  -> mouseClick me
          QuitEvent            -> pure InputQuit
          TouchFingerEvent tfe -> touchEvent tfe
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
      processClick clickX clickY
        | clickX > width || clickY > height = [] -- out of bounds
        | otherwise = let fcs =
                            ((\ (FieldCoords x y w h -> (x, y, x + w, y + h)))
                             <$>)
                            <$> fieldCoords bs w h
                          between x x1 x2 = x >= x1 && x < x2
                          isIn (x1, y1, x2, y2)=
                            between clickX x1 x2 && between clickY y1 y2
                          areIn = (isIn <$>) <$> fcs
                          isJust = \case
                            Nothing -> False
                            Just _ -> True
                          idxx = elemIndex True <$> areIn
                          idx = findIndex isJust idxx
                      in case idx of
                           Nothing -> []
                           Just y -> let x = idxx !! y
                                     in case x of
                                          Nothing ->
                                            error "Condition should never occur\
                                                  \;client/Main.inputActions/\
                                                  \processClick failed."
                                          Just xx -> pure $ Mark x y

  in (f =<<) <$> pollEvents
