module System.Posix.Daemonize (Logger, Program, daemonize) where
      
{- originally based on code from 
   http://sneakymustard.com/2008/12/11/haskell-daemons -}

import Control.Concurrent
import Control.Exception
import Prelude hiding (catch)
import System
import System.Posix
import System.Posix.Syslog

-- | The simplest possible interface to syslog.
type Logger = String -> IO ()

-- | A program type.
type Program = Logger -> IO ()

-- | Turns a program into a UNIX daemon, doing the necessary daemon
--   rain dance, providing it with the simplest possible interface to
--   the system log, writing a /var/run/$name.pid file, which
--   guarantees that only one instance is running, dropping
--   priviledges to $name:$name or daemon:daemon if $name is not
--   available, and handling start, stop, restart command-line
--   arguments. The stop argument does a soft kill first, and if that
--   fails for 1 second, does a hard kill.
daemonize :: Program -> IO ()
daemonize program = do name <- getProgName
                       args <- getArgs
                       process name args
    where

      process name ["start"] = startd name
      process name ["stop"]  = 
          do pid <- pidRead name
             let ifdo x f = x >>= \x -> if x then f else pass
             case pid of
               Nothing  -> pass
               Just pid -> 
                   (do signalProcess sigTERM pid
                       usleep (10^6)
                       ifdo (pidLive pid) $ 
                            do usleep (3*10^6)
                               ifdo (pidLive pid) (signalProcess sigKILL pid))
                   `finally`
                   removeLink (pidFile name)
      process name ["restart"] = do process name ["stop"]
                                    process name ["start"]
      process name _ = 
          putStrLn $ "usage: " ++ name ++ " {start|stop|restart}"

      startd name = pidExists name >>= p1
          where

            p1 False = 
                do setFileCreationMask 0 
                   forkProcess p2
                   exitImmediately ExitSuccess

            p1 True = 
                error "PID file exists. Process already running?"
                exitImmediately ExitFailure

            p2 = do createSession
                    pid <- forkProcess p3
                    pidWrite name pid
                    exitImmediately ExitSuccess

            p3 = withSyslog name [] DAEMON $ 
                do changeWorkingDirectory "/"
                   dropPriviledges name
                   closeFileDescriptors
                   blockSignal sigHUP
                   let log = syslog Notice 
                   forever log (program log)

forever :: Logger -> IO () -> IO ()
forever log program =     
    program `catch` restart where
        restart :: SomeException -> IO () 
        restart e = 
            do log ("Unexpected exception: " ++ show e)
               log "Restarting in 5 seconds.."
               usleep (5 * 10^6)
               forever log program

closeFileDescriptors :: IO ()
closeFileDescriptors = 
    do null <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
       let sendTo fd' fd = closeFd fd >> dupTo fd' fd
       mapM_ (sendTo null) $ [stdInput, stdOutput, stdError]

blockSignal :: Signal -> IO () 
blockSignal sig = installHandler sig Ignore Nothing >> pass

getGroupID :: String -> IO (Maybe GroupID)
getGroupID group = 
    try (fmap groupID (getGroupEntryForName group)) >>= return . f where
        f :: Either IOException GroupID -> Maybe GroupID
        f (Left e)    = Nothing
        f (Right gid) = Just gid

getUserID :: String -> IO (Maybe UserID)
getUserID user = 
    try (fmap userID (getUserEntryForName user)) >>= return . f where
        f :: Either IOException UserID -> Maybe UserID
        f (Left e)    = Nothing
        f (Right uid) = Just uid

dropPriviledges :: String -> IO ()
dropPriviledges name = 
    do Just ud <- getUserID "daemon"
       Just gd <- getGroupID "daemon"
       u       <- fmap (maybe ud id) $ getUserID name
       g       <- fmap (maybe gd id) $ getGroupID name
       setGroupID g 
       setUserID u

pidFile:: String -> String
pidFile name = "/var/run/" ++ name ++ ".pid"

pidExists :: String -> IO Bool
pidExists name = fileExist (pidFile name)

pidRead :: String -> IO (Maybe CPid)
pidRead name = pidExists name >>= choose where
    choose True  = fmap (Just . read) $ readFile (pidFile name)
    choose False = return Nothing

pidWrite :: String -> CPid -> IO ()
pidWrite name pid =
    writeFile (pidFile name) (show pid)

pidLive :: CPid -> IO Bool
pidLive pid = 
    (getProcessPriority pid >> return True) `catch` f where
        f :: IOException -> IO Bool
        f _ = return False
        
pass :: IO () 
pass = return ()
