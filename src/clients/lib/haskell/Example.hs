import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString (hGet)
import Data.List
import Network
import System (getArgs)
import System.IO
import System.Environment (getEnv)

import Xmms.Client
import Xmms.Client.Value
import Xmms.Client.Message
import Xmms.Client.Generated

main = withSocketsDo $ do
    args <- getArgs

    let host = args !! 0
    let port = PortNumber (fromIntegral (read (args !! 1)))

    h <- connectTo host port

    let firstClient = (Client "Haskell!" h [] 0)
    (betterClient, hResult) <- hello firstClient 16 (clientName firstClient)
    --putStrLn (show (fromIntegral(clientNextCookie betterClient)))

    hFlush h
    pll <- messageReadHeader h
    byteString <- hGet h pll
    let helloResultValue = (decode (B.fromChunks [byteString]) :: Value)

    (betterClient, lpResult) <- listPlaylistEntries betterClient "_active"
    --putStrLn (show (fromIntegral(clientNextCookie betterClient)))
    hFlush h

    pll <- messageReadHeader h
    byteString <- hGet h pll

    --putStrLn "playlist entry IDs:"
    --return (decode (B.fromChunks [byteString]) :: Value)

    (betterClient, mgiResult) <- medialibGetInfo betterClient 1
    --putStrLn (show (fromIntegral(clientNextCookie betterClient)))
    hFlush h

    pll <- messageReadHeader h
    byteString <- hGet h pll

    return (decode (B.fromChunks [byteString]) :: Value)

