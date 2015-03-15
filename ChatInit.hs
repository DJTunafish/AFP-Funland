import ChatServer
import ChatClient
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process
import System.Environment (getArgs)
import Control.Distributed.Process.Node

main :: IO ()
main = do args <- getArgs
          case args of 
            ["client", port] -> do
                 backend <- initializeBackend "127.0.0.1" port rtable
                 node <- newLocalNode backend
                 runProcess node (clientIgnition backend)
            ["server"]       -> do 
                 backend <- initializeBackend "127.0.0.1" "3000" initRemoteTable
                 liftIO $ putStrLn "Starting server..."
                 startMaster backend (master backend)
            _                -> 
                 putStrLn "Invalid startup command"
 where newLocalNode = Control.Distributed.Process.Backend.SimpleLocalnet.newLocalNode
