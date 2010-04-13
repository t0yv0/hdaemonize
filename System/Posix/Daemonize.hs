module System.Posix.Daemonize (Logger, Program, daemonize, serviced) where
      
{- originally based on code from 
   http://sneakymustard.com/2008/12/11/haskell-daemons -}


import Control.Concurrent
import Control.Exception.Extensible
import Prelude hiding (catch)
import System
import System.Exit
import System.Posix
import System.Posix.Syslog (withSyslog,Option(..),Priority(..),Facility(..),syslog)
import System.Posix.Types (UserID, GroupID)
import Data.Maybe (isNothing, fromMaybe, fromJust)

-- | You can simply daemonize any `IO ()` value with the `daemonize` function below, but for more sophistication, use the `serviced` function.  `serviced` takes a `CreateDaemon` record to describe how the daemon should be initialized.

data CreateDaemon = CreateDaemon {
  name :: Maybe String, -- If Nothing, defaults to the value of getProgName
  user :: Maybe String, -- If Nothing, defaults to user = daemonName, otherwise "daemon"
  group :: Maybe String,
  syslogOptions :: [Option],
  pidfileDirectory :: FilePath,
  sigtermTimeout :: Int, -- measured in us, any value below 10^6 is treated as 10^6
  program :: Program
}

simpleDaemon :: Program -> CreateDaemon
simpleDaemon p = CreateDaemon {
  name = Nothing,
  user = Nothing,
  group = Nothing,
  syslogOptions = [PID],
  pidfileDirectory = "/var/run",
  sigtermTimeout = 4*10^6,
  program = p
}
  

-- | The simplest possible interface to syslog.
type Logger = Priority -> String -> IO ()


-- | A program is any IO computation. It also accepts a syslog handle.
type Program = Logger -> IO ()


-- | Daemonizes a given IO computation by forking twice, closing
--   standard file descriptors, blocking sigHUP, setting file creation
--   mask, starting a new session, and changing the working directory
--   to root.

daemonize :: IO () -> IO () 
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

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM def Nothing = def
fromMaybeM _ (Just x)  = return x

serviced :: CreateDaemon -> IO ()
serviced daemon = do 
  systemName <- getProgName
  let daemon' = daemon { name = if isNothing (name daemon) 
                                then Just systemName else name daemon }
  args <- getArgs
  process daemon' args
    where
      
      program' daemon = withSyslog (fromJust $ name daemon) (syslogOptions daemon) DAEMON $
                      do let log = syslog Notice
                         log "starting"
                         pidWrite daemon
                         dropPrivileges daemon
                         forever syslog (program daemon)

      process daemon ["start"] = pidExists (fromJust $ name daemon) >>= f where
          f True  = do error "PID file exists. Process already running?"
                       exitImmediately (ExitFailure 1)
          f False = daemonize (program' daemon)
                 
      process daemon ["stop"]  = 
          do pid <- pidRead (fromJust $ name daemon)
             let ifdo x f = x >>= \x -> if x then f else pass
             case pid of
               Nothing  -> pass
               Just pid -> 
                   (do signalProcess sigTERM pid
                       usleep (10^6)
                       ifdo (pidLive pid) $ 
                            do usleep $ (sigtermTimeout daemon - 10^6) `max` 0
                               ifdo (pidLive pid) (signalProcess sigKILL pid))
                   `finally`
                   removeLink (pidFile $ fromJust $ name daemon)

      process daemon ["restart"] = do process daemon ["stop"]
                                      process daemon ["start"]
      process daemon _ = 
        getProgName >>= \pname -> putStrLn $ "usage: " ++ pname ++ " {start|stop|restart}"


{- implementation -}

forever :: Logger -> Program -> IO ()
forever log program =     
    program log `catch` restart where
        restart :: SomeException -> IO () 
        restart e = 
            do log Error ("unexpected exception: " ++ show e)
               log Error "restarting in 5 seconds"
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

dropPrivileges :: CreateDaemon -> IO ()
dropPrivileges daemon = 
    do Just ud <- getUserID "daemon"
       Just gd <- getGroupID "daemon"
       let targetUser = fromMaybe (fromJust $ name daemon) (user daemon)
           targetGroup = fromMaybe (fromJust $ name daemon) (group daemon)
       u       <- fmap (maybe ud id) $ getUserID targetUser
       g       <- fmap (maybe gd id) $ getGroupID targetGroup
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

pidWrite :: CreateDaemon -> IO ()
pidWrite daemon =
    getProcessID >>= \pid ->
    writeFile (pidFile $ fromJust $ name daemon) (show pid)

pidLive :: CPid -> IO Bool
pidLive pid = 
    (getProcessPriority pid >> return True) `catch` f where
        f :: IOException -> IO Bool
        f _ = return False
        
pass :: IO () 
pass = return ()
