module GitHUD.Daemon.Network (
  sendOnSocket,
  fromSocket
  ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.UTF8 as BSU
import Network.Socket (Family(AF_UNIX), socket, defaultProtocol, Socket, SocketType(Stream), close, listen, accept, bind, SockAddr(SockAddrUnix), connect)
import Network.Socket.ByteString (recv, sendAll)
import System.Posix.Files (fileExist)

sendOnSocket :: FilePath
             -> String
             -> IO Bool
sendOnSocket socketPath msg =
  E.bracket open mClose (mTalkOnClientSocket msg)
  where
    open = do
        socketExists <- fileExist socketPath
        if socketExists
          then do
            sock <- socket AF_UNIX Stream defaultProtocol
            connect sock (SockAddrUnix socketPath)
            return $ Just sock
          else return Nothing
    mClose = maybe (return ()) close

mTalkOnClientSocket :: String
                    -> Maybe Socket
                    -> IO Bool
mTalkOnClientSocket _ Nothing = return False
mTalkOnClientSocket msg (Just sock) = do
    sendAll sock $ BSU.fromString msg
    return True

fromSocket :: FilePath
           -> (String -> IO m)
           -> IO ()
fromSocket socketPath withMessageCb = E.bracket open close loop
  where
    open = do
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock (SockAddrUnix socketPath)
      listen sock 1
      return sock
    loop sock = forever $ do
      (conn, peer) <- accept sock
      void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = (readPacket conn "") >>= withMessageCb
    readPacket conn acc = do
      msg <- recv conn 1024
      if (S.null msg)
        then return acc
        else readPacket conn (acc ++ (BSU.toString msg))
