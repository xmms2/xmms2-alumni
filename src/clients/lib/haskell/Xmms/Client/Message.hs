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

module Xmms.Client.Message (
      messageWriteInt
    , messageWriteString
    , messageWriteHeader
    , messageReadHeader
) where

import Data.Word
import Network (Socket)
import Network.Socket.ByteString (recv, sendMany)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Int
import Data.Binary
import Data.Binary.Put (runPut)

putWord32 :: Word32 -> Put
putWord32 = put

putInt32 :: Int32 -> Put
putInt32 = put

-- Write a raw string (ie without the type tag)
myPutRawStr :: String -> Put
myPutRawStr s = putWord32 (fromIntegral (succ (length s))) >> mapM_ put s >> put '\0'

-- Write a raw int (ie without the type tag)
myPutRawInt = putInt32

messageWriteInt :: Socket -> Int -> IO ()
messageWriteInt h i = do
    let x = runPut (myPutRawInt (fromIntegral i))
    sendMany h (BL.toChunks x)

messageReadInt :: Socket -> IO Int
messageReadInt sock = do
    msg <- recv sock 4

    let i = (decode (BL.fromChunks [msg]) :: Int32)
    return (fromIntegral i)

messageWriteString :: Socket -> String -> IO ()
messageWriteString h s = do
    let x = runPut (myPutRawStr s)
    sendMany h (BL.toChunks x)

messageWriteHeader :: Socket -> (Int32, Int32, Int32, Int32) -> IO ()
messageWriteHeader h t = sendMany h (BL.toChunks (encode t))

messageReadHeader :: Socket -> IO (Int32, Int32, Int32, Int32)
messageReadHeader handle = do
    msg <- recv handle 16

    return (decode (BL.fromChunks [msg]) :: (Int32, Int32, Int32, Int32))

