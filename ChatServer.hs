{-# Language TemplateHaskell #-}

module ChatServer where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import ChatClient

--Setup for the server. Registers the server pid 
--befor launching server
master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
    self <- getSelfPid
    register "server" self
    liftIO $ putStrLn "Server ready!"
    server backend [] [] slaves


--The main server function. Listens for messages and 
--responds by spawning processes on the sending node
--and possibly by spawning handlers on the local node.
server :: Backend -> [(String, SendPort (String, NodeId))] -> 
            [(String, NodeId)]-> [NodeId] -> Process ()
server backend chatRooms users slaves = do
    (cmd, args, nid) <- expect :: Process (String, [String], NodeId)
    self <- getSelfPid
    peers <- liftIO $ findPeers backend 500
    case (cmd, args) of 
     --usr requests to start a chat with otherUsr
     ("/startChat", [usr, otherUsr]) -> 
         case findX otherUsr users of
          --If otherUsr does not exist, inform user 
          Nothing -> let msg = "No such user."
                     in do spawn nid ($(mkClosure 'waitForInput) (msg, self))
                           server backend chatRooms users slaves
          --If otherUsr does not exist, spawn a listening process
          --on both usr and otherUsrs node. Then, spawn a sending
          --process on both usr & otherUsr, with the sendPort
          --corresponding to the other users receivePort
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
            --Start a chatRoom by spawning a local process running
            --the roomHandler function. Then, send a message on the
            --channel belonging to that process, including the NodeId
            --belonging to usr, informing it that usr wishes to join the room.
            --Then, spawn a process on usrs node that will send messages to the
            --room process.
            spawn nid ($(mkClosure 'serverMessage) 
                        ("Started room " ++ roomName))
            sPortR <- spawnChannelLocal $ roomHandler roomName []
            sendChan sPortR (("//addUser " ++ usr), nid)
            spawn nid ($(mkClosure 'roomSend) (usr, sPortR))
            server backend ((roomName, sPortR):chatRooms) users slaves
     ("/joinRoom", [room, user])  -> do 
     --Attempt to join an existing chatroom
            case findX room chatRooms of
             --If the room does not exist, the user will be told as much
             Nothing    -> do spawn nid ($(mkClosure 'waitForInput)
                                            ("No such room", self)) 
                              server backend chatRooms users slaves
             --If the room does exist, send a message on its channel,
             --saying that usr wishes to connect to the room. Then,
             --spawn a process on usrs node that will send messages
             --to the room process.
             Just sPort -> do sendChan sPort (("//addUser " ++ user), nid)
                              spawn nid ($(mkClosure 'roomSend) (user, sPort))
                              spawn nid ($(mkClosure 'serverMessage) 
                                            ("Joined room " ++ room))
                              server backend chatRooms users slaves
     ("/help", _)      -> --Print a message on the clients terminal,
                          --informing him/her about the available commands
                          do spawn nid ($(mkClosure 'waitForInput) 
                                            (helpMessage, self))
                             server backend chatRooms users slaves
     ("/connect", _)   -> --Spawn a process on the user that will ask for
                          --input and forward commands to the server
                          do spawn nid ($(mkClosure 'waitForInput) 
                                        (initMessage, self)) 
                             server backend chatRooms users slaves
     ("/remRoom", [roomName]) -> --Remove the given room from the list of
                                 --available rooms
                                 let chatRooms' = remRoom roomName chatRooms
                                 in  server backend chatRooms' users slaves
     ("/passive", [usr])   -> --Add the given user to the list of available 
                              --users to chat with, if the chosen username
                              --is available.
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


--A handler for chatrooms. Listens for messages
--and broadcasts to all connected clients, unless it's a command
--to add/remove a client. 
roomHandler :: String -> [(String, SendPort String)] -> 
                ReceivePort (String, NodeId) -> Process ()
roomHandler self clients rPort = do
    (msg, nid) <- receiveChan rPort
    let msg' = tail $ dropWhile (\a -> a /= '>' && a /= '/') msg 
    let msgWords = words msg' 
    case head msgWords of
        "/addUser"    -> do  
            --Upon receiving a request to add a user,
            --the room process will spawn a listening process
            --on the users node and store a sendPort to said process.
            --Then, a message is broadcast to all other users that 
            --the user has joined the channel.
            let usr = head (drop 1 msgWords)
            sPort <- spawnChannel $(mkStatic 'sdictString) nid 
                                  ($(mkClosure 'chatReceive) (()))  
            mapM (\(_, chan) -> sendChan chan ("User " ++ usr ++ " joined!")) clients
            roomHandler self ((usr, sPort):clients) rPort
        "/disconnect" -> do 
            --Upon receiving a disconnect request from a client,
            --the room process will find the control process on the
            --clients node and send a message to it, causing all
            --processes on said node to terminate, as they are 
            --all linked to the control process.
            let usr = takeWhile (\a -> a /= '>') msg
            let clients' = remUser usr clients
            liftIO $ putStrLn $ "Disconnecting user. Clients: " ++ (show clients')
            whereisRemoteAsync nid "control"
            WhereIsReply _ (Just con) <- expect :: Process WhereIsReply
            spawn nid ($(mkClosure 'serverMessage) ("Exiting chatroom..."))
            send con "Ded"
            mapM (\(_, chan) -> 
                   sendChan chan ("User " ++ usr ++ " left")) clients'
            --If there are no clients left in the room after removing
            --the requested client, then kill the room and inform the
            --main server process to remove it from the list of available
            --rooms.
            if clients' == [] 
            then do nid <- getSelfNode
                    Just serv <- whereis "server"
                    send serv ("/remRoom", [self], nid)
                    return ()
            else roomHandler self (remUser usr clients') rPort
        _            -> do --Send the received message to all users
                         mapM (\(_, chan) -> sendChan chan msg) clients
                         roomHandler self clients rPort
  where remUser :: String -> [(String, SendPort String)] -> 
                    [(String, SendPort String)]
        remUser _ []           = []
        remUser s ((y, sp):ys) = if s == y
                                 then ys
                                 else (y, sp):(remUser s ys)

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

