import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad 
--import Data.String.Utils
import Control.Distributed.Process.Closure

chatSend :: (String, SendPort String) -> Process ()
--chatSend :: (String, ProcessId) -> Process ()
chatSend (usrname, chan) = do self <- getSelfPid
                              liftIO $ putStrLn ("Ready to send to " ++ (show chan))
                              msg <- liftIO getLine
                              sendChan chan (usrname ++ ">" ++ msg)
                              --send pid (usrname ++ ">" ++ msg)
                              (s, r) <- newChan :: Process (SendPort String, ReceivePort String)
                     --         liftIO $ putStrLn ("Sent message to " ++ (show s))
                              chatSend (usrname, chan)

chatReceive :: (ReceivePort String) -> Process ()
chatReceive chan = do self <- getSelfPid
                      liftIO $ putStrLn $ "Ready to receive at process " ++ (show self)
                      msg <- receiveChan chan
                 --     msg <- expect :: Process String
                      liftIO $ putStrLn "GOOOOOOOD DAMN IT"
                      liftIO $ putStrLn msg
                      chatReceive chan

chatSetup :: ProcessId -> Process ()
chatSetup pid = do liftIO $ putStrLn "Setting up chat..."
                   (sPort, rPort) <- newChan :: Process (SendPort String, ReceivePort String)
             --      self <- getSelfPid
                   spawnLocal (chatReceive rPort)
                   --spawnLocal chatReceive
                   send pid sPort
                --   send pid self
               

initClient :: ProcessId -> Process ()
initClient pid = do 
                 liftIO $ putStrLn greetMessage
                 usrname <- liftIO getLine
                 self <- getSelfNode
                -- send pid ("/newUser", [usrname], self)
                 liftIO $ putStrLn "Would you like to join a chatroom, contact a user, or wait for someone"
               --         "to contact you? Enter \"passive\" to wait, \"room\" to connect to a chatroom" ++ 
               --         "and \"contact\" to contact a user.")
                 choice <- liftIO getLine
                 case choice of
                    "passive" -> --do self <- getSelfNode
                                 send pid ("/newUser", [usrname], self)
                    "contact" -> do liftIO $ putStrLn "Please enter the name of the user you wish to chat with"
                                    otherUsr <- liftIO getLine
                                    send pid ("/startChat", [usrname, otherUsr], self)
                    "chatroom" -> undefined                
                 
greetMessage :: String
greetMessage = "Welcome to the Awesomely Radical chat client! \n" ++
               "Please start by connecting to a user or chat room. \n" ++
               "For a list of available users, enter /users \n" ++
               "For a list of available chatrooms, enter /rooms. \n" ++
               "For other commands, please enter /help \n \n" ++
               "Please enter a username"

helpMessage :: String
helpMessage = "Poopypoopypoop" --TODO

waitForInput :: (String, ProcessId) -> Process () 
waitForInput (msg, pid) = do 
    liftIO $ putStr msg
    input <- liftIO $ getLine --What happens if another process runs getLine while this is in effect?
    self <- getSelfNode
    let args  = words input
    let cmd   = head args
    let args' = tail args
    send pid (cmd, args', self)            

printHelp :: Process ()
printHelp = liftIO $ putStrLn ("/startChat: Start a chat with another user")             

--startGroupChat :: ProcessId -> Process ()
remotable ['chatSend, 'chatSetup, 'initClient, 'printHelp, 'waitForInput]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable 

main :: IO ()
main = do
  args <- getArgs
  
  putStrLn $ show args
  case args of
    ["server", host, port] -> do --start a chat server
      backend <- initializeBackend host port rtable
      startMaster backend (master backend)
    ["client", host, port] -> do
      backend <- initializeBackend host port rtable
   {-   putStrLn ("Welcome to the Awesomely Radical chat client! \n" ++
               "Please start by connecting to a user or chat room. \n" ++
               "For a list of available users, enter /users \n" ++
               "For a list of available chatrooms, enter /rooms. \n" ++
               "For other commands, please enter /help") -} --Move this to when client has managed to connect
      node <- newLocalNode backend
      runProcess node (clientIgnition backend)
    _        -> putStrLn "Illegal command" 
    where newLocalNode = Control.Distributed.Process.Backend.SimpleLocalnet.newLocalNode

clientIgnition :: Backend -> Process ()
clientIgnition back = do 
      server <- findServer back
      case server of
         Nothing    -> liftIO $ putStrLn "Server down. Try again later"
         (Just pid) -> do self <- getSelfNode
                          send pid ("/connect", ([]::[String]),  self)
                          liftIO $ startSlave back

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
--[(String, ProcessId)] = list of chat rooms [(String, NodeId)] = Users & their NodeId
master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
    self <- getSelfPid
    register "server" self
    server backend [] [] slaves

server :: Backend -> [(String, ProcessId)] -> [(String, NodeId)]-> [NodeId] -> Process ()
server backend chatRooms users slaves = do
  --  forever $ receiveWait [match (spawnCommand backend chatRooms users), match singleChat]
    (cmd, args, nid) <- expect :: Process (String, [String], NodeId)
    --Perhaps change this to use receiveWait and match
    self <- getSelfPid
    case cmd of --Need an initial handshake message for setting username and such
        "/startChat" -> let usr = head args 
                            otherUsr = head (drop 1 args)
                        in case findUsr otherUsr users of
                            Nothing -> let msg = "No such user."
                                       in do spawn nid ($(mkClosure 'waitForInput) (msg, self))
                                             server backend chatRooms users slaves
                            Just n  -> do spawn nid ($(mkClosure 'chatSetup) (self))
                                          sPort1 <- expect :: Process (SendPort String)
                                       --   sPort1 <- expect :: Process ProcessId
                                          spawn n ($(mkClosure 'chatSetup) (self))
                                          sPort2 <- expect :: Process (SendPort String)
                                        --  sPort2 <- expect :: Process ProcessId
                                          spawn nid ($(mkClosure 'chatSend) (usr, sPort2))
                                          spawn n ($(mkClosure 'chatSend) (otherUsr, sPort1))
                                          server backend chatRooms users slaves
        "/startRoom" -> undefined
        "/joinRoom"  -> undefined
        "/help"      -> do spawn nid ($(mkClosure 'waitForInput) (helpMessage, self))
                           server backend chatRooms users slaves
        "/connect"   -> do spawn nid ($(mkClosure 'initClient) (self)) 
                           server backend chatRooms users slaves
        "/newUser"   -> server backend chatRooms ((head args, nid):users) slaves
        _            -> undefined
    where findUsr :: String -> [(String, NodeId)] -> Maybe NodeId
          findUsr x [] = Nothing
          findUsr x ((y, n):ys) = case x == y of
                                True  -> Just n
                                False -> findUsr x ys 


                                
{-spawnCommand :: Backend -> [(String, ProcessId)] -> [(String, NodeId)] -> (String, NodeId) -> Process ()   
spawnCommand back chatRooms users (msg, pid) = 
    case msg of
        "/connect" -> spawn pid ($(mkClosure 'initClient) (self))
        "/help"    -> spawn pid ($(mkClosure 'printHelp))
-}                                     



    





