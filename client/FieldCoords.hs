-- Copyright (C) 2022 Sio Kreuzer. All Rights Reserved.
module FieldCoords where

import SDL
import Lib

data FieldCoords = FieldCoords { fieldX :: Int32
                               , fieldY :: Int32
                               , fieldW :: Int32
                               , fieldH :: Int32
                               } deriving (Eq, Show)

-- simple, bad impl: every field takes up equal space on screen, no blank space whatsoever
fieldCoords :: BoardSize
            -> Int32
            -> Int32
            -> Vector (Vector FieldCoords)
fieldCoords bs w h = fc (case bs of
                           ThreeByThree -> 3
                           FiveByFive -> 5)
                        w
                        h
  where fc :: Int32 -> Int32 -> Int32 -> Vector (Vector FieldCoords)
        fc div w h =
          let width = w / div
              height = h / div
          in take div $ fmap (take div) $
               (\ x y z w -> FieldCoords (x * width) (y * height) y z w)
                 <$> [0, 1, 2, 3, 4]
                 <*> [0, 1, 2, 3, 4]
                 <*> [width]
                 <*> [height]

fieldCoordsAsSDLRects :: BoardSize
                      -> Int32
                      -> Int32
                      -> Vector (Vector Rectangle CInt)
fieldCoordsAsSDLRects = fieldCoordsToSDLRects <<< fieldCoords

fieldCoordsToSDLRects :: Vector (Vector FieldCoords)
                      -> Vector (Vector Rectangle CInt)
fieldCoordsToSDLRects = fmap $
  \ (FieldCoords x y w h) ->
      CInt <$> Rectangle (Point (V2 x y)) (V2 w h)
