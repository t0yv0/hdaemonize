module System.Posix.Daemonize (Logger, Program, daemonize, serviced) where
      
{- originally based on code from 
   http://sneakymustard.com/2008/12/11/haskell-daemons -}


import Control.Concurrent
import Control.Exception.Extensible
import Prelude hiding (catch)
import System
import System.Exit
import System.Posix
import System.Posix.Syslog


-- | The simplest possible interface to syslog.
type Logger = String -> IO ()


-- | A program is any IO computation. It also accepts a syslog handle.
type Program = Logger -> IO ()


-- | Daemonizes a given IO computation by forking twice, closing
--   standard file descriptors, blocking sigHUP, setting file creation
--   mask, starting a new session, and changing the working directory
--   to root.

daemonize :: IO () ->  IO () 
daemonize program = 

    do setFileCreationMask 0 
       forkProcess p
       exitImmediately ExitSuccess

    where

      p  = do createSession
              forkProcess p'
              exitImmediately ExitSuccess
                              
      p' = do changeWorkingDirectory "/"
              closeFileDescriptors
              blockSignal sigHUP
              program


-- | Turns a program into a UNIX daemon (system service) ready to be
--   deployed to /etc/rc.d or similar startup folder.  The resulting
--   program handles command-line arguments (start, stop, or restart).
--
--   With start option it writes out a PID to /var/run/$name.pid where
--   $name is the executable name. If PID already exists, it refuses
--   to start, guaranteeing there is only one live instance.
--
--   With stop option it reads the PID from /var/run/$name.pid and
--   terminates the corresponding process (first a soft kill, SIGTERM,
--   then a hard kill, SIGKILL).
--
--   Another addition over the daemonize function is dropping
--   privileges.  If a system user and group with a name that matches
--   the executable name exist, privileges are dropped to that user and
--   group.  Otherwise, they are dropped to the standard daemon user
--   and group.
--
--   Finally, exceptions in the program are caught, logged to syslog,
--   and the program restarted.

serviced :: Program -> IO ()
serviced program = do name <- getProgName
                      args <- getArgs
                      process name args
    where

      program' name = withSyslog name [] DAEMON $
                      do let log = syslog Notice
                         log "starting"
                         pidWrite name
                         dropPrivileges name
                         forever log program

      process name ["start"] = pidExists name >>= f where
          f True  = do error "PID file exists. Process already running?"
                       exitImmediately (ExitFailure 1)
          f False = daemonize (program' name)
                 
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


{- implementation -}

forever :: Logger -> Program -> IO ()
forever log program =     
    program log `catch` restart where
        restart :: SomeException -> IO () 
        restart e = 
            do log ("unexpected exception: " ++ show e)
               log "restarting in 5 seconds"
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

dropPrivileges :: String -> IO ()
dropPrivileges name = 
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

pidWrite :: String -> IO ()
pidWrite name =
    getProcessID >>= \pid ->
    writeFile (pidFile name) (show pid)

pidLive :: CPid -> IO Bool
pidLive pid = 
    (getProcessPriority pid >> return True) `catch` f where
        f :: IOException -> IO Bool
        f _ = return False
        
pass :: IO () 
pass = return ()
