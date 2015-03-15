{-# Language TemplateHaskell #-}

module ChatClient where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

--Setup for the chatSend' function. Links the process
--to the "control" process on the local node,
--so that when said process dies, so does this process.
chatSend :: (String, SendPort String) -> Process ()
chatSend x = do Just monitor <- whereis "control"
                link monitor 
                chatSend' x

--Send function for simple client to client chat.
--Sends messages on the given channel, which is 
--directly connected to the other client.
chatSend' :: (String, SendPort String) -> Process ()
chatSend' (usrname, chan) = do msg <- liftIO getLine
                               case msg of 
                                "/disconnect" -> do Just monitor <- whereis "control"
                                                    sendChan chan "/disconnect"
                                                    liftIO $ putStrLn "Disconnecting from chat..."
                                                    send monitor "EVERYONE DIES"
                                _             -> do sendChan chan (usrname ++ ">" ++ msg)
                                                    chatSend' (usrname, chan)

--Setup for the roomSend' function. Links the process
--to the "control" process on the local node,
--so that when said process dies, so does this process.
roomSend :: (String, SendPort (String, NodeId)) -> Process ()
roomSend x = do Just monitor <- whereis "control"
                link monitor
                roomSend' x

--Send function for a chatRoom. Gets inoput from the user
--and uses the given SendPort to send this message along 
--with the local nodes ID to the process on the server
--that manages the chatRoom.
roomSend' :: (String, SendPort (String, NodeId)) -> Process ()
roomSend' (usrname, chan) = do self <- getSelfNode
                               msg <- liftIO getLine
                               sendChan chan ((usrname ++ ">" ++ msg), self)
                               roomSend' (usrname, chan)

--Setup for the chatReceive function. You know the drill.
chatReceive :: () -> ReceivePort String -> Process ()
chatReceive () chan = do Just monitor <- whereis "control"
                         link monitor
                         chatReceive' chan

--Receive function for clients. Receives messages
--on the given receiveport and outputs them on stdout.
chatReceive' :: ReceivePort String -> Process ()
chatReceive' chan = do self <- getSelfPid
                       msg <- receiveChan chan
                       case msg of
                        "/disconnect" -> do Just monitor <- whereis "control"
                                            liftIO $ putStrLn "Other user disconnected, ending chat..."
                                            send monitor "EVERYONE DIES"
                        _             -> do liftIO $ putStrLn msg
                                            chatReceive' chan

--Used by the server to output a single message                       
serverMessage :: String -> Process ()
serverMessage s = liftIO $ putStrLn s           
              
--Waits for input from the user and sends the given
--message to the given ProcessId
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

--RemoteTable containing the functions that other nodes 
--may use to spawn processes on this node
rtable :: RemoteTable
rtable = __remoteTable initRemoteTable 

--Starts a client on the given backend, if the server is up.
--If the server is available, the function sends a connect
--request to the server, then registers the current process
--as the "control" process on this node. Other processes on 
--this node will later link themselves to this process.
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

--Attempts to find the server on the given backend
--by searching for a process registered as "server"
--on the available nodes
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







