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
    , messageWriteCollection
    , messageWriteBinary
    , messageWriteStringList
    , messageWriteStringDictionary
    , messageBuildHeader
    , messageReadHeader
) where

import Data.Word
import Network (Socket)
import Network.Socket.ByteString (recv, sendMany)

import qualified Data.ByteString.Lazy as BL

import Data.Int
import Data.Binary
import Data.Binary.Put (runPut)

import Xmms.Client.Value

messageWriteInt :: Int -> BL.ByteString
messageWriteInt i = encode (IntValue (fromIntegral i))

messageWriteString :: String -> BL.ByteString
messageWriteString s = encode (StringValue s)

messageWriteCollection = undefined
messageWriteBinary = undefined

messageWriteStringList :: [String] -> BL.ByteString
messageWriteStringList ss = encode (ListValue (map StringValue ss))

messageWriteStringDictionary = undefined

messageBuildHeader :: (Int32, Int32, Int32, Int32) -> BL.ByteString
messageBuildHeader t = encode t

messageReadHeader :: Socket -> IO (Int32, Int32, Int32, Int32)
messageReadHeader handle = do
    msg <- recv handle 16

    return (decode (BL.fromChunks [msg]) :: (Int32, Int32, Int32, Int32))

