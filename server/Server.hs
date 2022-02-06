module Main where

import Lib
import Servant
import Network.Wai
import Network.Wai.Handler.Warp

import Control.Concurrent.STM
import StmContainers.Map as M

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
  (liftIO $ atomically $ lookup sid $ sessions s)
  >>= \case
    Nothing -> e
    (Just x) -> atomically $ f x

server :: ServerState -> Server ServerAPI
server _ Nothing = pure Nothing
server s (Just sc) =
  case sc of
    ServerNewSession -> fmap Just $ liftIO $
      uncurry ClientSessionCreated
        <$> atomically $ do
          session <-
            newTVar =<< (ServerSession
                           <$> (SessionID <$> (newID $ maxSessionID s))
                           <*> (PlayerID <$> (newID $ maxPlayerID s))
                           <*> pure Nothing
                           <*> newTVar (Timestamp 0)
                           <*> newTVar 0
                           <*> newTVar Nothing)
          x <- (sessionID <$> readTVar session) >>= readTVar
          insert session x (sessions s)
          pure (x, y)

    ServerQuerySessionStatus sid ->
      withSession s sid $ \ x -> atomically $ case x of
       { (ServerSession _ (Just pa) (Just pb) _) ->
           pure $ Just $ ClientStartSession sid pa pb
       ; _ -> pure Nothing
       }

    ServerJoinSession sid -> withSession s sid $ \ session -> do
      npid <- PlayerID <$> newID (maxPlayerID s)
      modifyTVar session $ \ x -> x { playerB = Just npid }
      remotepid <- playerA <$> readTVar session
      case remotepid of
        Nothing -> error "FIXME: Trying to join invalid session. This indicates a server bug, wherein an invalid session was created."
        Just r -> pure $ ClientStartSession sid npid r

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
          pure $ Just $ ClientGameAction la lai

{-
TODO
[ ] timestamps need created/updated properly
[ ] regularly scheduled cleanup according to timestamps & timeout values
-}
