{-# Language TemplateHaskell #-}

module ChatClient where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

chatSend :: (String, SendPort String) -> Process ()
chatSend x = do Just monitor <- whereis "control"
                link monitor --Implement disconnect for regular chats
                chatSend' x

chatSend' :: (String, SendPort String) -> Process ()
chatSend' (usrname, chan) = do msg <- liftIO getLine
                               case msg of 
                                "/disconnect" -> do Just monitor <- whereis "control"
                                                    sendChan chan "/disconnect"
                                                    liftIO $ putStrLn "Disconnecting from chat..."
                                                    send monitor "EVERYONE DIES"
                                _             -> do sendChan chan (usrname ++ ">" ++ msg)
                                                    chatSend' (usrname, chan)

roomSend :: (String, SendPort (String, NodeId)) -> Process ()
roomSend x = do Just monitor <- whereis "control"
                link monitor
                roomSend' x

roomSend' :: (String, SendPort (String, NodeId)) -> Process ()
roomSend' (usrname, chan) = do self <- getSelfNode
                               msg <- liftIO getLine
                               sendChan chan ((usrname ++ ">" ++ msg), self)
                               roomSend' (usrname, chan)

chatReceive :: () -> ReceivePort String -> Process ()
chatReceive () chan = do Just monitor <- whereis "control"
                         link monitor
                         chatReceive' chan

chatReceive' :: ReceivePort String -> Process ()
chatReceive' chan = do self <- getSelfPid
                       msg <- receiveChan chan
                       case msg of
                        "/disconnect" -> do Just monitor <- whereis "control"
                                            liftIO $ putStrLn "Other user disconnected, ending chat..."
                                            send monitor "EVERYONE DIES"
                        _             -> do liftIO $ putStrLn msg
                                            chatReceive' chan

                       
serverMessage :: String -> Process ()
serverMessage s = liftIO $ putStrLn s           
              

waitForInput :: (String, ProcessId) -> Process () 
waitForInput (msg, pid) = do 
    liftIO $ putStrLn msg
    input <- liftIO $ getLine 
    self <- getSelfNode
    let args  = words input
    let cmd   = head args
    let args' = tail args
    send pid (cmd, args', self)       

sdictString :: SerializableDict String
sdictString = SerializableDict               

remotable ['chatSend, 'chatReceive, 'waitForInput, 'sdictString, 'roomSend, 'serverMessage]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable 

clientIgnition :: Backend -> Process ()
clientIgnition back = do 
      server <- findServer back
      case server of
         Nothing    -> liftIO $ putStrLn "Server down. Try again later"
         (Just pid) -> do self <- getSelfNode
                          send pid ("/connect", ([]::[String]),  self)
                          selfPid <- getSelfPid
                          register "control" selfPid
                          expect :: Process String
                          return ()

findServer :: Backend -> Process (Maybe ProcessId)
findServer back = do 
                    nodes <- liftIO $ findPeers back 500
                    isServer nodes
    where isServer :: [NodeId] -> Process (Maybe ProcessId)
          isServer [] = return Nothing
          isServer (x:xs) = do 
                             whereisRemoteAsync x "server"
                             msg <- expect :: Process WhereIsReply
                             case msg of
                                WhereIsReply _ (Just pid) -> return $ Just pid
                                _                         -> isServer xs







