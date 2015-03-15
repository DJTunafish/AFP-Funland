{-# Language TemplateHaskell #-}

module ChatServer where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import ChatClient

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
    self <- getSelfPid
    register "server" self
    liftIO $ putStrLn "Server ready!"
    server backend [] [] slaves

server :: Backend -> [(String, SendPort (String, NodeId))] -> 
            [(String, NodeId)]-> [NodeId] -> Process ()
server backend chatRooms users slaves = do
    (cmd, args, nid) <- expect :: Process (String, [String], NodeId)
    self <- getSelfPid
    peers <- liftIO $ findPeers backend 500
    case (cmd, args) of 
     ("/startChat", [usr, otherUsr]) -> 
         case findX otherUsr users of
          Nothing -> let msg = "No such user."
                     in do spawn nid ($(mkClosure 'waitForInput) (msg, self))
                           server backend chatRooms users slaves
          Just n  -> do sPort1 <- spawnChannel $(mkStatic 'sdictString) nid 
                                               ($(mkClosure 'chatReceive) (()))
                        sPort2 <- spawnChannel $(mkStatic 'sdictString) n 
                                                  ($(mkClosure 'chatReceive) (()))
                        spawn nid ($(mkClosure 'chatSend) (usr, sPort2))
                        spawn n ($(mkClosure 'chatSend) (otherUsr, sPort1))
                        spawn nid ($(mkClosure 'serverMessage) 
                                    ("Now chatting with " ++ otherUsr))
                        spawn n ($(mkClosure 'serverMessage) 
                                   ("Now chatting with " ++ usr))  
                        server backend chatRooms users slaves
     ("/startRoom", [roomName, usr]) -> do 
            sPortC <- spawnChannel $(mkStatic 'sdictString) nid 
                                    ($(mkClosure 'chatReceive) (()))
            sPortR <- spawnChannelLocal $ roomHandler roomName [(usr, sPortC)]
            spawn nid ($(mkClosure 'roomSend) (usr, sPortR))
            spawn nid ($(mkClosure 'serverMessage) 
                         ("Started room " ++ roomName))
            server backend ((roomName, sPortR):chatRooms) users slaves
     ("/joinRoom", [room, user])  -> do 
            case findX room chatRooms of
             Nothing    -> do spawn nid ($(mkClosure 'waitForInput)
                                            ("No such room", self)) 
                              server backend chatRooms users slaves
             Just sPort -> do sendChan sPort (("//addUser " ++ user), nid)
                              spawn nid ($(mkClosure 'roomSend) (user, sPort))
                              spawn nid ($(mkClosure 'serverMessage) 
                                            ("Joined room " ++ room))
                              server backend chatRooms users slaves
     ("/help", _)      -> do spawn nid ($(mkClosure 'waitForInput) 
                                            (helpMessage, self))
                             server backend chatRooms users slaves
     ("/connect", _)   -> do spawn nid ($(mkClosure 'waitForInput) 
                                        (initMessage, self)) 
                             server backend chatRooms users slaves
     ("/remRoom", [roomName]) -> let chatRooms' = remRoom roomName chatRooms
                                 in  server backend chatRooms' users slaves
     ("/passive", [usr])   -> 
        case findX usr users of
                Nothing -> do spawn nid ($(mkClosure 'serverMessage) 
                               ("Waiting for another user to start chat..."))
                              server backend chatRooms ((usr, nid):users) slaves
                _       -> do spawn nid ($(mkClosure 'waitForInput)
                                           ("Username taken", self)) 
                              server backend chatRooms users slaves
     _            -> do spawn nid ($(mkClosure 'waitForInput)
                                    ("No command found, "++ 
                                     "or incorrect arguments", self)) 
                        server backend chatRooms users slaves
    where findX :: String -> [(String, a)] -> Maybe a
          findX x [] = Nothing
          findX x ((y, n):ys) = case x == y of
                                 True  -> Just n
                                 False -> findX x ys 
          remRoom :: String -> [(String, a)] -> [(String, a)]
          remRoom _ []          = []
          remRoom x ((y, n):ys) = if x == y then ys else (y, n):(remRoom x ys)

roomHandler :: String -> [(String, SendPort String)] -> 
                ReceivePort (String, NodeId) -> Process ()
roomHandler self clients rPort = do
    (msg, nid) <- receiveChan rPort
    let msg' = tail $ dropWhile (\a -> a /= '>' && a /= '/') msg 
    let msgWords = words msg' 
    case head msgWords of
        "/addUser"    -> do  
            let usr = head (drop 1 msgWords)
            sPort <- spawnChannel $(mkStatic 'sdictString) nid 
                                  ($(mkClosure 'chatReceive) (()))  
            mapM (\(_, chan) -> sendChan chan ("User " ++ usr ++ " joined!")) clients
            roomHandler self ((usr, sPort):clients) rPort
        "/disconnect" -> do 
            let usr = takeWhile (\a -> a /= '>') msg
            let clients' = remUser usr clients
            liftIO $ putStrLn $ "Disconnecting user. Clients: " ++ (show clients')
            whereisRemoteAsync nid "control"
            WhereIsReply _ (Just con) <- expect :: Process WhereIsReply
            spawn nid ($(mkClosure 'serverMessage) ("Exiting chatroom..."))
            send con "8==D"
            mapM (\(_, chan) -> 
                   sendChan chan ("User " ++ usr ++ " left")) clients'
            if clients' == []
            then do nid <- getSelfNode
                    Just serv <- whereis "server"
                    send serv ("/remRoom", [self], nid)
                    return ()
            else roomHandler self (remUser usr clients') rPort
        _            -> do 
                         mapM (\(_, chan) -> sendChan chan msg) clients
                         roomHandler self clients rPort
  where remUser :: String -> [(String, SendPort String)] -> 
                    [(String, SendPort String)]
        remUser _ []           = []
        remUser s ((y, sp):ys) = if s == y
                                 then ys
                                 else (y, sp):(remUser s ys)

greetMessage :: String
greetMessage = "Welcome to the Awesomely Radical chat client! \n" ++
               "Please start by connecting to a user or chat room. \n" ++
               "For a list of available users, enter /users \n" ++
               "For a list of available chatrooms, enter /rooms. \n" ++
               "For other commands, please enter /help \n \n" ++
               "Please enter a username"

initMessage :: String 
initMessage = "Welcome to the Awesomely Radical chat client! \n" ++
              "To see a list of available commands, enter /help. \n" ++
              "Otherwise, please enter a command now"

helpMessage :: String
helpMessage = "/startChat ownUsrName usrToContact : Start a chat with another user \n" ++
              "/startRoom roomName userName : Start a new chatroom \n" ++
              "/joinRoom roomName userName : Connect to an existing chatroom \n" ++
              "/passive usrname , wait for someone to contact you \n" ++
              "/disconnect , disconnect from current chat \n" ++
              "/help , displays all commands"

