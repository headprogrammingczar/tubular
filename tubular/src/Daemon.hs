module Daemon where

import Foreign.C.String
import System.Directory
import System.Posix.Syslog
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (Fd)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Monoid
import System.Exit
import System.IO

runDaemon :: FilePath -> String -> IO () -> IO ()
runDaemon pidFile' daemonName daemonAction = do
  -- check if running
  checkPid pidFile'
  -- first fork
  void $ forkProcess $ do
    -- setsid
    void createSession
    -- second fork
    void $ forkProcess $ do
      -- umask - prevent files from being group or world usable
      void $ setFileCreationMask (unionFileModes groupModes otherModes)
      -- handle signals - use CatchOnce because if it doesn't exit the first time, something is wrong
      mainThreadId <- myThreadId
      void $ installHandler keyboardTermination (CatchOnce (gracefulExit mainThreadId pidFile')) Nothing
      void $ installHandler softwareTermination (CatchOnce (gracefulExit mainThreadId pidFile')) Nothing
      -- detach stdin
      detachFd stdInput
      -- create pid file
      pid <- getProcessID
      writeFile pidFile' (show pid)
      -- set up syslog
      withSyslog daemonName [LogPID, ImmediateOpen] Daemon $ do
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering
        logFd "stdout" stdOutput
        logFd "stderr" stdError
        -- run service
        daemonAction

-- return if program can continue
-- otherwise exit
checkPid :: FilePath -> IO ()
checkPid pidFilePath = do
  fileExists <- doesPathExist pidFilePath
  case fileExists of
    -- no other instances running
    False -> pure ()
    True -> do
      -- see if the pid file references an active process
      pidstr <- readFile pidFilePath
      pid <- readIO pidstr
      guard (pid > 1)
      running <- catch (signalProcess nullSignal pid >> pure True) (\(SomeException _) -> pure False)
      case running of
        False -> pure ()
        True -> error ("Already running in pid "++pidstr)

gracefulExit :: ThreadId -> FilePath -> IO ()
gracefulExit mainThreadId pidFilePath = do
  removeFile pidFilePath
  -- signal handlers run in a forkIO thread
  -- but only the main thread can exit the program
  throwTo mainThreadId ExitSuccess

-- redirects an Fd to syslog
-- the Fd is consumed from a forkIO thread
logFd :: String -> Fd -> IO ()
logFd label fd = do
  (r, w) <- createPipe
  rh <- fdToHandle r
  void (dupTo w fd)
  void $ forkIO $ do
    forever $ do
      line <- hGetLine rh
      syslog' Warning label line

syslog' :: Priority -> String -> String -> IO ()
syslog' p label s = withCStringLen (label <> ": " <> s) $ syslog Nothing p

detachFd :: Fd -> IO ()
detachFd fd = do
  devNull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
  void (dupTo devNull fd)
  closeFd devNull

