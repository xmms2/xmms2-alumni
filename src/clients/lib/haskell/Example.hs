import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (hGet)

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)

import System (getArgs)
import System.Environment (getEnv)

import Xmms.Client
import Xmms.Client.Value
import Xmms.Client.Message
import Xmms.Client.Generated

main = withSocketsDo $ do
    args <- getArgs

    let host = args !! 0
    let port = args !! 1

    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    h <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect h (addrAddress serveraddr)

    let firstClient = (Client "Haskell!" h [] 0)
    (betterClient, hResult) <- hello firstClient 16 (clientName firstClient)
    --putStrLn (show (fromIntegral(clientNextCookie betterClient)))

    (_, _, _, pll) <- messageReadHeader h
    byteString <- recv h (fromIntegral pll)
    let helloResultValue = (decode (BL.fromChunks [byteString]) :: Value)

    (betterClient, lpResult) <- listPlaylistEntries betterClient "_active"
    --putStrLn (show (fromIntegral(clientNextCookie betterClient)))

    (_, _, _, pll) <- messageReadHeader h
    byteString <- recv h (fromIntegral pll)

    --putStrLn "playlist entry IDs:"
    --return (decode (BL.fromChunks [byteString]) :: Value)

    (betterClient, mgiResult) <- medialibGetInfo betterClient 1
    --putStrLn (show (fromIntegral(clientNextCookie betterClient)))

    (_, _, _, pll) <- messageReadHeader h
    byteString <- recv h (fromIntegral pll)

    return (decode (BL.fromChunks [byteString]) :: Value)

