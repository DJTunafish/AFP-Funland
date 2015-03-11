import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad 
import Control.Distributed.Process.Closure
--import qualified Control.Distributed.Process.ManagedProcess.Client as C
--import qualified Control.Distributed.Process.Extras.Internal.Types as C

slave :: NodeId -> Process ()
slave s = do liftIO $ putStrLn "Sweet"
             whereisRemoteAsync s "server" 
             WhereIsReply _ (Just pid) <- expect
             liftIO $ putStrLn "Duuuuude"
             send (pid) "Greetings!"
             liftIO $ putStrLn $ "Sent message to pid: " ++ (show pid)
           --  liftIO $ putStrLn $ "NodesId output: " ++ (show (slaves', "A string"))


remotable ['slave]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable 

main :: IO ()
main = do
  args <- getArgs
  
  putStrLn $ show args
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
    --  self <- getSelfPid
    --  register "server" self
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend

master :: Backend -> [NodeId] -> Process ()
master backend slaves = 
  -- Do something interesting with the slaves
   -- case slaves of
     --   [] -> do 
                do
                  slaves' <- liftIO (findPeers backend 500) 
                  --liftIO $ putStrLn $ "NodesId output: " ++ (show (slaves', "A string"))
                  self <- getSelfPid
                  register "server" self
                  liftIO $ putStrLn "Registered server"
                  Just pid <- whereis "server"
                  liftIO $ putStrLn "Found server!"
                  forM_ (zip [processNodeId self] slaves) $ \(s, them) ->
                    spawn them ($(mkClosure 'slave) (s))
                  sts <- expect :: Process String
                  liftIO $ putStrLn sts
                  terminateAllSlaves backend 
         {-       master backend (tail slaves')
        _  -> do
            self <- getSelfPid
            forM_ (zip ([1, 2, 3, 4] :: [Integer]) (cycle slaves)) $ \(s, them) -> do
        --    forM_ ([((1 :: Integer), (head slaves))]) $ \(s, them) -> do
              spawn them ($(mkClosure 'slave) (self, s))
              --_ <- expectTimeout 0 :: Process (Maybe DidSpawn)
              return ()
            liftIO $ putStrLn "PINGAS"
            res <- (expect :: Process Integer)
            liftIO $ putStrLn (show res)
        --    slaves' <- liftIO $ findPeers backend 500
           -- liftIO $ putStrLn $ "NodesId output: " ++ (show slaves)
          -- Terminate the slaves when the master terminates (this is optional)
          --  terminateAllSlaves backend-}


