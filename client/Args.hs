-- Copyright (C) 2022 Sio Kreuzer. All Rights Reserved.
module Args where

import Lib

import Options.Applicative
import Data.Semigroup ((<>))

import Control.Category

parseArgs :: IO Args
parseArgs =
  execParser $
    info (args <**> helper)
         ( briefDesc
           <> progDesc "tic-tac-toe reference client"
           <> failureCode 3)

data Args = Args { serverURL       :: String
                 , remotePlayerID  :: Maybe PlayerID
                 , remoteSessionID :: Maybe SessionID
                 }

args :: Parser Args
args = Args
  <$> strOption
      ( long "server" <> short 's' <> help "server to connect to")
  <*> option ((Just <<< PlayerID) <$> auto)
      ( long "player"
        <> short 'p'
        <> help "player to play against, if any"
        <> showDefault
        <> value Nothing)
  <*> option ((Just <<< SessionID) <$> auto)
      ( long "session"
        <> short 'k'
        <> help "session to join, if any"
        <> showDefault
        <> value Nothing)
