-- Copyright (C) 2022 Sio Kreuzer. All Rights Reserved.
module ClientInfo where

import Lib
import Args

import SDL

data ClientInfo = ClientInfo { session :: ClientSession
                             , xTex :: Texture
                             , oTex :: Texture
                             } -- TODO: mutable structure containing what server we're connecting to etc.

mkClientInfo :: Renderer -> Args -> IO ClientInfo
mkClientInfo renderer args = do
  error "TODO: get the server to connect to from args, as well as optional player id etc"
  error "TODO: connect to server, try to connect to/create a session"
  error "TODO: print session id to terminal"
  error "TODO: draw x and o surfaces and create the corresponding textures"
