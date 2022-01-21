module Lib
  ( Field (..)
  , fieldComplement
  , Index (..)
  , indexToInt
  , Board (..)
  , boardToFrozen
  , Action (..)
  , BoardSize (..)
  , validateIndex
  , connectToWin
  , GameConfig (..)
  , configFromRemote
  , config3x3
  , config5x5
  , Game (..)
  , performAction
  , endTurn
  , hasWon
  ) where

import Data.Aeson

import Data.Vector (Vector, (!))
import Data.IORef

data Field = X | O deriving (Show, Eq)
instance ToJSON Field
instance FromJSON Field

fieldComplement :: Field -> Field
fieldComplement X = O
fieldComplement O = X

data Index = Zero | One | Two | Three | Four deriving (Show, Eq, Ord)
instance ToJSON Index
instance FromJSON Index

indexToInt :: Index -> Int
indexToInt Zero  = 0
indexToInt One   = 1
indexToInt Two   = 2
indexToInt Three = 3
indexToInt Four  = 4

-- NOTE: mutable. Requires some fiddling to get ToJSON/FromJSON working right maybe
data Board = Board (Vector (Vector (IORef (Maybe Field)))) deriving (Show, Eq)
instance ToJSON Board
instance FromJSON Board

boardToFrozen :: Board -> IO (Vector (Vector (Maybe Field)))
boardToFrozen (Board bd) =
  sequence $ sequence <$> (fmap (fmap readIORef) bd)

data Action = Pass | Mark Index Index deriving (Show, Eq)
instance ToJSON Action
instance FromJSON Action

data BoardSize = FiveByFive | ThreeByThree deriving (Show, Eq)
instance ToJSON BoardSize
instance FromJSON BoardSize

validateIndex :: BoardSize -> Index -> Bool
validateIndex ThreeByThree Three = False
validateIndex ThreeByThree Four = False
validateIndex _ _ = True

connectToWin :: BoardSize -> Int
connectToWin ThreeByThree = 3
connectToWin FiveByFive = 4

data GameConfig = GameConfig
  { localPlayer :: Field
  , boardSize :: BoardSize -- either 3x3 or 5x5
  } deriving (Show, Eq)
instance ToJSON GameConfig
instance FromJSON GameConfig

configFromRemote :: GameConfig -> GameConfig
configFromRemote gc@(GameConfig remote _) =
  gc { localPlayer = fieldComplement remote }

config3x3 :: Field -> GameConfig
config3x3 local = GameConfig local ThreeByThree

config5x5 :: Field -> GameConfig
config5x5 local = GameConfig local FiveByFive

-- NOTE: mutable. Requires some fiddling to get ToJSON/FromJSON working right maybe
data Game = Game { board   :: Board
                 , config  :: GameConfig
                 , current :: IORef Field
                 } deriving (Show, Eq)
instance ToJSON Game
instance FromJSON Game

performAction :: Game -> Action -> IO ()
performAction g Pass = endTurn g
performAction g@((Board bd) (GameConfig local size) curr) (Mark x y) =
  if not (validateIndex size x) || not (validateIndex size y)
    then endTurn g -- invalid moves mean whoever's making the invalid move loses their turn.
    else let field = (bd ! y) ! x
         in readIORef field >>= \case
           Nothing -> writeIORef field curr *> endTurn g
           x -> endTurn g
    -- check if there's anything there, and if not, set, and finally end the turn

endTurn :: Game -> IO ()
endTurn g = modifyIORef (current g) fieldComplement

-- TODO: a way to check whether someone has won, specialized on either 3x3 or 5x5

-- returns winning player, if any.
hasWon :: Game -> IO (Maybe Field)
hasWon (Game bd (GameConfig _ ThreeByThree) _) =
  let aresame  (x, y, z) = if x == y && y == z && x == z then Just x else Nothing
      getCoord b (x, y) = (b ! y) ! x
      sameCoords b c@((x1, y1), (x2, y2), (x3, y3)) = areSame $ getCoord <$> c
      l b = sameCoords b <$> [ ((0, 0), (0, 1), (0, 2)) -- horizontal, top
                             , ((1, 0), (1, 1), (1, 2)) -- horizontal, middle
                             , ((2, 0), (2, 1), (2, 2)) -- horizontal, bottom
                             , ((0, 0), (1, 0), (2, 0)) -- vertical, left
                             , ((0, 1), (1, 1), (2, 1)) -- vertical, middle
                             , ((0, 2), (1, 2), (2, 2)) -- vertical, right
                             , ((0, 0), (1, 1), (2, 2)) -- diagonal, left upper to right lower
                             , ((0, 2), (1, 1), (2, 0)) -- diagonal, right upper to left lower
                             ] -- that's all the possible winning positions
  in boardToFrozen bd >>= (\ x -> pure $ headMay $ filter isJust $ l <*> [x])
hasWon (Game bd (GameConfig _ size) _) = do
  let toWin = connectToWin size
      subseqHor = error "TODO"
      subseqVert = error "TODO"
      subseqDiag = error "TODO"
  error "TODO: check if there are n in a row anywhere, for n either 3 or 5"
  where headMay :: [a] -> Maybe a
        headMay [] = Nothing
        headMay (x:_) = Just x
        isJust :: Maybe a -> Bool
        isJust Nothing = False
        isJust _       = True
