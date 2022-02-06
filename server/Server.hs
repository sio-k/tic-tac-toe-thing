module Main where

import Lib
import Servant
import Network.Wai
import Network.Wai.Handler.Warp

import Control.Concurrent.STM
import StmContainers.Map as M

import Control.Monad.IO.Class

import GHC.Word

-- Server State. Individual parts are mutable in the STM (software transactional
-- memory) monad, to ensure that this entire thing is thread-safe to mutate.
data ServerState =
  ServerState
    { sessions :: M.Map SessionID (TVar ServerSession)
    , maxSessionID :: TVar Word
    , maxPlayerID :: TVar Word
    }

mkServerState :: IO ServerState
mkServerState =
  ServerState
  <$> (atomically $ M.new)
  <*> newTVarIO 0
  <*> newTVarIO 0

main :: IO ()
main = do
  state <- mkServerState
  run 8080 $ app $ server state

app :: Server ServerAPI -> Application
app = serve serverAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

newID :: TVar Word -> STM Word
newID x = stateTVar x $ \ v -> (v, v + 1)

withSession :: (MonadIO m)
            => ServerState
            -> SessionID
            -> (TVar ServerSession -> STM (Maybe a))
            -> m (Maybe a)
withSession s sid f = withSessionElse s sid (pure Nothing) f

withSessionElse :: (MonadIO m)
                => ServerState
                -> SessionID
                -> m (Maybe a)
                -> (TVar ServerSession -> STM (Maybe a))
                -> m (Maybe a)
withSessionElse s sid e f =
  (liftIO $ atomically $ M.lookup sid $ sessions s)
  >>= \case
    Nothing -> e
    (Just x) -> liftIO $ atomically $ f x

server :: ServerState -> Server ServerAPI
server _ Nothing = pure Nothing
server s (Just sc) =
  case sc of
    ServerNewSession -> fmap Just $ liftIO $ do
      (sid, pid) <- atomically $ do
          session <-
            newTVar =<< (ServerSession
                           <$> (SessionID <$> (newID $ maxSessionID s))
                           <*> (PlayerID <$> (newID $ maxPlayerID s))
                           <*> pure Nothing
                           <*> newTVar (Timestamp 0)
                           <*> newTVar 0
                           <*> newTVar Nothing)
          (ServerSession sid pid _ _ _ _) <- readTVar session
          insert session sid (sessions s)
          pure (sid, pid)
      pure $ ClientSessionCreated sid pid

    ServerQuerySessionStatus sid ->
      withSession s sid $ \ x -> readTVar x >>= \case
       { (ServerSession _ pa (Just pb) _ _ _) ->
           pure $ Just $ ClientStartSession sid pa pb
       ; _ -> pure Nothing
       }

    ServerJoinSession sid -> withSession s sid $ \ session -> do
      npid <- PlayerID <$> newID (maxPlayerID s)
      modifyTVar session $ \ x -> x { playerB = Just npid }
      Just <$> ClientStartSession sid npid <$> playerA <$> readTVar session

    ServerEndSession sid pid -> fmap Just $ liftIO $ atomically $ do
      delete sid (sessions s)
      pure $ ClientEndSession sid

    ServerGameAction sid pid act -> withSessionElse s sid (pure $ Just $ ClientEndSession sid) $
      \ session -> do
        s <- readTVar session
        writeTVar (lastAction s) $ Just act
        modifyTVar (lastActionIndex s) (+ 1)
        returnLastAction session

    ServerQueryAction sid pid ->
      withSessionElse s
                      sid
                      (pure $ Just $ ClientEndSession sid)
                      returnLastAction

  where returnLastAction session = do
          (ServerSession _ _ _ _ lai la) <- readTVar session
          Just <$> (ClientGameAction <$> readTVar la <*> readTVar lai)

{-
TODO
[ ] timestamps need created/updated properly
[ ] regularly scheduled cleanup according to timestamps & timeout values
-}
