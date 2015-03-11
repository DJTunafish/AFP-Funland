import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad 
--import Data.String.Utils
import Control.Distributed.Process.Closure

startChat :: Process ()
startChat = undefined

initClient :: Process ()

--startGroupChat :: ProcessId -> Process ()
remotable ['startChat, 'initClient]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable 

main :: IO ()
main = do
  args <- getArgs
  
  putStrLn $ show args
  case args of
    ["server", host, port] -> do --start a chat server
      backend <- initializeBackend host port rtable
      startMaster backend (master backend [] [])
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
        Nothing  -> liftIO $ putStrLn "Server down. Try again later"
        Just pid -> msg <- liftIO getLine --Change this section to send a handshake message
                    self <- getSelfNode
                    send pid (msg, self)
                    startSlave back

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
master :: Backend -> [(String, ProcessId)] -> [(String, NodeId)] -> [NodeId] -> Process ()
master backend chatRooms users slaves = do
    self <- getSelfPid
    register "server" self
    (msg, pid) <- expect :: Process (String, NodeId)
    case msg of --Need an initial handshake message for setting username and such
        "/startChat" -> do 
                         spawn pid ($(mkClosure 'startChat))
                         usr <- liftIO getLine
                         case findUsr usr users of --Send Either String ProcessId ?
                            Nothing -> send pid $ Left "No such user"
                            Just n  -> --Spawn process on node, send channel back?
        "/startRoom" -> undefined
        "/joinRoom"  -> undefined
        "/help"      -> undefined
        _            -> undefined
    where findUsr :: String -> [(String, NodeId)]
          findUsr x [] = Nothing
          findUsr x ((y, n):ys) = case x == y of
                                True  -> Just n
                                False -> fundUsr x ys
                                
                                
     


    





