module Lib where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}

import Data.Aeson

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVec

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.IORef

import Control.Category
import Control.Arrow
import Control.Monad

import GHC.Generics
import GHC.Word

data Field = X | O deriving (Show, Eq, Generic)
instance ToJSON Field
instance FromJSON Field

fieldComplement :: Field -> Field
fieldComplement X = O
fieldComplement O = X

data Index = Zero | One | Two | Three | Four deriving (Show, Eq, Generic, Ord)
instance ToJSON Index
instance FromJSON Index

indexToInt :: Index -> Int
indexToInt Zero  = 0
indexToInt One   = 1
indexToInt Two   = 2
indexToInt Three = 3
indexToInt Four  = 4

-- NOTE: mutable. Requires some fiddling to get ToJSON/FromJSON working right maybe
data Board = Board (Vector (Vector (IORef (Maybe Field)))) deriving (Generic)

newtype FrozenBoard = FrozenBoard (Vector (Vector (Maybe Field))) deriving (Generic)
instance ToJSON FrozenBoard
instance FromJSON FrozenBoard

freezeBoard :: Board -> IO FrozenBoard
freezeBoard (Board bd) =
  FrozenBoard <$> (sequence $ sequence <$> (fmap (fmap readIORef) bd))

thawBoard :: FrozenBoard -> IO Board
thawBoard (FrozenBoard fbd) =
  Board <$> (sequence $ sequence <$> (fmap (fmap newIORef) fbd))

data Action = Pass | Mark Index Index deriving (Show, Eq, Generic)
instance ToJSON Action
instance FromJSON Action

data BoardSize = FiveByFive | ThreeByThree deriving (Show, Eq, Generic)
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
  } deriving (Show, Eq, Generic)
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
                 } deriving (Generic)

data FrozenGame = FrozenGame { frozenBoard :: FrozenBoard
                             , frozenConfig :: GameConfig
                             , frozenCurrent :: Field
                             } deriving (Generic)
instance ToJSON FrozenGame
instance FromJSON FrozenGame

gameToJSON :: Game -> IO Value
gameToJSON = freezeGame >>> fmap toJSON

gameFromJSON :: Value -> IO Game
gameFromJSON =
  fromJSON
  >>> (\case
          Error str -> error $ "Could not get game from JSON" <> str
          Success x -> thawGame x)

freezeGame :: Game -> IO FrozenGame
freezeGame (Game bd cfg curr) = do
  player <- readIORef curr
  fbd <- freezeBoard bd
  pure $ FrozenGame fbd cfg player

thawGame :: FrozenGame -> IO Game
thawGame (FrozenGame fbd cfg player) =
  Game <$> thawBoard fbd <*> pure cfg <*> newIORef player

performAction :: Game -> Action -> IO ()
performAction g Pass = endTurn g
performAction g@(Game (Board bd) (GameConfig local size) curr) (Mark x y) =
  if not (validateIndex size x) || not (validateIndex size y)
    then endTurn g -- invalid moves mean whoever's making the invalid move loses their turn.
    else let field = (bd ! indexToInt y) ! indexToInt x
         in do
           player <- readIORef curr
           modifyIORef field (\case
                                 Nothing -> Just player
                                 x -> x)
           endTurn g
    -- check if there's anything there, and if not, set, and finally end the turn

endTurn :: Game -> IO ()
endTurn g = modifyIORef (current g) fieldComplement

-- returns winning player, if any.
hasWon :: Game -> IO (Maybe Field)
hasWon (Game bd (GameConfig _ ThreeByThree) _) =
  let areSame  (x, y, z) = if x == y && y == z && x == z then x else Nothing
      getCoord b (x, y) = (b ! y) ! x
      map3 f (x, y, z) = (f x, f y, f z)
      sameCoords :: FrozenBoard
                 -> ((Int, Int), (Int, Int), (Int, Int))
                 -> Maybe Field
      sameCoords (FrozenBoard b) c = areSame $ getCoord b `map3` c
      l b = sameCoords b <$> [ ((0, 0), (0, 1), (0, 2)) -- horizontal, top
                             , ((1, 0), (1, 1), (1, 2)) -- horizontal, middle
                             , ((2, 0), (2, 1), (2, 2)) -- horizontal, bottom
                             , ((0, 0), (1, 0), (2, 0)) -- vertical, left
                             , ((0, 1), (1, 1), (2, 1)) -- vertical, middle
                             , ((0, 2), (1, 2), (2, 2)) -- vertical, right
                             , ((0, 0), (1, 1), (2, 2)) -- diagonal, left upper to right lower
                             , ((0, 2), (1, 1), (2, 0)) -- diagonal, right upper to left lower
                             ] -- that's all the possible winning positions
  in freezeBoard bd >>= (\ x -> pure $ join $ headMay $ filter isJust $ l x) -- <*> [x])
hasWon (Game bd (GameConfig _ size) _) = do
  let toWin = connectToWin size
      subseqHor = error "TODO: hasWon for 5x5"
      subseqVert = error "TODO"
      subseqDiag = error "TODO"
  error "TODO: check if there are n in a row anywhere, for n either 3 or 5"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

newtype PlayerID = PlayerID Int deriving (Eq, Generic, Show)
instance ToJSON PlayerID
instance FromJSON PlayerID

newtype SessionID = SessionID Int deriving (Eq, Generic, Show)
instance ToJSON SessionID
instance FromJSON SessionID

data ClientSession = ClientSession { sessionID :: SessionID
                                   , sessionGame :: Game
                                   , remotePlayerID :: PlayerID
                                   , localPlayerID :: PlayerID
                                   } deriving (Generic)

data ServerPlayer = ServerPlayer { playerAddress :: ByteString
                                 , playerID :: PlayerID
                                 } deriving (Generic)

-- NOTE: this is a time_t, it counts seconds since 1970-01-01T00:00:00 UTC
newtype Timestamp = Timestamp Word64 deriving (Eq, Ord, Generic, Num)

data ServerSession = ServerSession { sessionID :: SessionID
                                   , playerA   :: ServerPlayer
                                   , playerB   :: ServerPlayer
                                   , startTime :: Timestamp
                                   , lastActionTime :: IORef Timestamp -- if currentTime - lastActionTime > timeout, destroy/invalidate this session
                                   } deriving (Generic)

data ServerSessions = ServerSessions (IOVec.IOVector ServerSession)

-- TODO: environment in which server sessions can be read/written and IO actions performed

data ServerCommand = ServerNewSession -- returns sessionID and new player ID, waits for other player
                   | ServerJoinSession SessionID -- joins a player to a session and starts it
                   | ServerStartSession SessionID PlayerID PlayerID
                   | ServerEndSession SessionID PlayerID PlayerID -- all must match
                   | ServerGameAction Action PlayerID -- player takes an action
                   deriving (Generic, Eq, Show)
instance ToJSON ServerCommand
instance FromJSON ServerCommand

data ClientCommand = ClientStartSession SessionID PlayerID PlayerID
                   | ClientEndSession SessionID PlayerID PlayerID
                   | ClientGameAction Action -- player has taken an action
                   deriving (Generic, Eq, Show)
instance ToJSON ClientCommand
instance FromJSON ClientCommand

newtype LineCommand = LineCommand (Maybe (Either ClientCommand ServerCommand))
                    deriving (Generic, Eq, Show)
instance ToJSON LineCommand
instance FromJSON LineCommand

withClientCommand :: LineCommand -> (ClientCommand -> a) -> Maybe a
withClientCommand (LineCommand (Just (Left cc))) f = Just $ f cc
withClientCommand _ _ = Nothing

withServerCommand :: LineCommand -> (ServerCommand -> a) -> Maybe a
withServerCommand (LineCommand (Just (Right sc))) f = Just $ f sc
withServerCommand _ _ = Nothing
