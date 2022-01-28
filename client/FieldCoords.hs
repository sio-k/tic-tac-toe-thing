-- Copyright (C) 2022 Sio Kreuzer. All Rights Reserved.
module FieldCoords where

import SDL hiding (Vector)
import Linear

import Lib
import GHC.Int
import Foreign.C.Types

import Data.List as L (groupBy, take, drop, sortBy)

import Data.Vector as V

import Control.Category
import Data.Functor

data FieldCoords = FieldCoords { fieldX :: Int32
                               , fieldY :: Int32
                               , fieldW :: Int32
                               , fieldH :: Int32
                               } deriving (Eq, Show)

toInt32 :: Int -> Int32
toInt32 = fromInteger <<< toInteger

toInt :: Int32 -> Int
toInt = fromInteger <<< toInteger

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
        fc d w h =
          let width = w `div` d
              height = h `div` d
          in V.take (toInt d) $ fmap (V.take $ toInt d) $
               fromList $ fmap fromList $
                 [[FieldCoords x y | x <- [0..4]] | y <- [0..4]]
                 <&> ((<*> pure width) >>> (<*> pure height))

                 {- let l = --(\ x y z w -> FieldCoords (x * width) (y * height) z w)
                         --  <$> [0, 1, 2, 3, 4]
                         --  <*> [0, 1, 2, 3, 4]
                         (flip fmap) [[FieldCoords x y | x <- [0..4]] | y <- [0..4]]
                           (<*> pure width <*> pure height)
                     --f :: [a] -> [[a]] -- hacky as fuck
                     --f [] = []
                     --f xs = L.take 5 xs : f (L.drop 5 xs)
                     --sorter (FieldCoords x1 y1 _ _) (FieldCoords x2 y2) -- TODO: figure out whether this sorts in the correct order or not for all of this.
                     --  | y1 < y2             = LT
                     --  | y1 > y2             = GT
                     --  | y1 == y2 && x1 > x2 = GT
                     --  | y1 == y2 && x1 < x2 = LT
                     --  | otherwise           = EQ
                 in f $ sortBy sorter l -}



fieldCoordsAsSDLRects :: BoardSize
                      -> Int32
                      -> Int32
                      -> Vector (Vector (Rectangle CInt))
fieldCoordsAsSDLRects b x y = fieldCoordsToSDLRects $ fieldCoords b x y

fieldCoordsToSDLRects :: Vector (Vector FieldCoords)
                      -> Vector (Vector (Rectangle CInt))
fieldCoordsToSDLRects = fmap $ fmap $
  \ (FieldCoords x y w h) ->
      CInt <$> Rectangle (P (V2 x y)) (V2 w h)
