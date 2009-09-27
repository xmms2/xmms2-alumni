--  XMMS2 - X Music Multiplexer System
--  Copyright (C) 2003-2009 XMMS2 Team
--
--  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.

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
    (betterClient, hResult) <- mainHello firstClient 16 (clientName firstClient)
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

