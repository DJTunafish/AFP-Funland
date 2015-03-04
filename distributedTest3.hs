import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad 
import Control.Distributed.Process.Closure

slave :: String -> Process ()
slave s = liftIO $ putStrLn s

remotable ['slave]

main :: IO ()
main = do
  args <- getArgs
  
  putStrLn $ show args
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- Do something interesting with the slaves
    forM_ (zip ["1", "2", "3"] (cycle slaves)) $ \(s, them) -> 
      spawn them ($(mkClosure 'slave) s)
  -- Terminate the slaves when the master terminates (this is optional)
    terminateAllSlaves backend


