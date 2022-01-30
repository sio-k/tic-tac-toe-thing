-- Copyright (C) 2022 Sio Kreuzer. All Rights Reserved.
module Main where

import Lib

import Input
import FieldCoords
import ClientInfo
import Args

import SDL hiding (Vector)
import SDL.Video.Renderer as Renderer hiding (Vector)

import Data.IORef

import GHC.Word
import GHC.Int
import Data.Vector as Vector hiding (sequence_, copy)

import Foreign.C

import Prelude hiding (null, head, filter, init, tail, head, last, zip, replicate, length)
import qualified Prelude

main :: IO ()
main = do
  args <- parseArgs
  initializeAll -- init all SDL subsystems (this is probably fine)
  window <- createWindow "Tic-Tac-Toe reference client" $
              defaultWindow { windowPosition  = Centered
                            , windowResizable = True
                            }
  renderer <- createRenderer window
                             (-1)
                             (RendererConfig AcceleratedVSyncRenderer False)

  client <- mkClientInfo renderer args

  mainLoop window renderer client

  destroyRenderer renderer
  destroyWindow window

unCInt :: CInt -> Int32
unCInt (CInt x) = x

mainLoop :: Window -> Renderer -> ClientInfo -> IO ()
mainLoop window renderer ci = do
  (V2 w h) <- fmap unCInt <$> (get $ windowSize window)
  ia <- fromList <$> inputActions window ci w h
  if not $ Vector.null $ Vector.filter (== InputQuit) ia
    then do
      error "TODO: notify server that we're quitting and close the connection"
      pure ()
    else do
      error "TODO: poll for state changes"
      error "TODO: apply state changes locally, if okay to do so"
      error "TODO: check whether we got any state changes from the server, and apply those if we got any"

      rendererDrawColor renderer $= clearColor
      clear renderer

      rendererDrawColor renderer $= drawColor
      renderState window renderer ci w h

      present renderer

      -- recurse
      mainLoop window renderer ci

clearColor :: V4 Word8
clearColor = V4 0x13 0x13 0x13 0xFF

drawColor :: V4 Word8
drawColor = V4 0xBF 0xBF 0xBF 0xFF

renderState :: Window -> Renderer -> ClientInfo -> Int32 -> Int32 -> IO ()
renderState window renderer ci w h = do
  let fcs = fieldCoords (boardSize $ config $ sessionGame $ session ci) w h
  drawFields renderer fcs w h
  drawMarks renderer ci fcs
  drawCurrentTurnIndicator renderer ci

drawFields :: Renderer -> Vector (Vector FieldCoords) -> Int32 -> Int32 -> IO ()
drawFields renderer fcs w h =
  let ys = (\ x -> fieldY x + fieldH x) <$> (init $ head <$> fcs)
      xs = (\ x -> fieldX x + fieldW x) <$> (init $ head fcs)
      mkLineBits x y = P <$> (uncurry V2) <$> zip (CInt <$> x) (CInt <$> y)
      mkNPointsForList l value = replicate (length l) value
      mk0ptsForList l = mkNPointsForList l 0
      mkEndPtsForList l endPoint = mkNPointsForList l endPoint
      xlineStarts = mkLineBits xs $ mk0ptsForList xs
      xlineEnds = mkLineBits xs $ mkEndPtsForList xs h
      ylineStarts = mkLineBits (mk0ptsForList ys) ys
      ylineEnds = mkLineBits (mkEndPtsForList ys w) ys
      xlines = zip xlineStarts xlineEnds
      ylines = zip ylineStarts ylineEnds
      lines = xlines <> ylines
  in sequence_ $ uncurry (drawLine renderer) <$> lines

drawMarks :: Renderer -> ClientInfo -> Vector (Vector FieldCoords) -> IO ()
drawMarks renderer ci fields = do
  let allRects = fieldCoordsToSDLRects fields
  (FrozenBoard bd) <- freezeBoard $ board $ sessionGame $ session $ ci
  let bothzipped = uncurry Vector.zip <$> Vector.zip allRects bd -- TODO: this is probably broken
  let tex X = xTex ci
  let tex O = oTex ci
  let draw (_, Nothing) = pure ()
  let draw (rect, (Just x)) = copy renderer (tex x) Nothing $ Just rect
  sequence_ $ sequence_ <$> (fmap draw <$> bothzipped)

-- top left, 32x32px
turnIndicatorPos :: Rectangle CInt
turnIndicatorPos = Rectangle (P $ CInt <$> V2 0 0) $ CInt <$> V2 32 32

drawCurrentTurnIndicator :: Renderer -> ClientInfo -> IO ()
drawCurrentTurnIndicator renderer ci = do
  curr <- readIORef $ current $ sessionGame $ session ci
  let tex = case curr of
        X -> xTex ci
        O -> oTex ci
  copy renderer tex Nothing $ Just turnIndicatorPos
