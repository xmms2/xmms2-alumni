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
    , messageEncodeHeader
    , messageReadHeader
) where

import Control.Monad (liftM)
import Data.Word
import Network (Socket)
import Network.Socket.ByteString (recv, sendMany)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Int
import Data.Binary
import Data.Binary.Put (runPut)

import Xmms.Client.Collection
import Xmms.Client.Value

messageWriteInt :: Int -> BL.ByteString
messageWriteInt = encode . IntValue . fromIntegral

messageWriteString :: String -> BL.ByteString
messageWriteString = encode . StringValue

messageWriteCollection :: Collection -> BL.ByteString
messageWriteCollection = encode . CollValue

messageWriteBinary :: [Word8] -> BL.ByteString
messageWriteBinary = encode . BinValue

messageWriteStringList :: [String] -> BL.ByteString
messageWriteStringList = encode . ListValue . map StringValue

messageWriteStringDictionary = undefined

messageEncodeHeader :: (Int32, Int32, Int32, Int32) -> BL.ByteString
messageEncodeHeader = encode

messageDecodeHeader :: B.ByteString -> (Int32, Int32, Int32, Int32)
messageDecodeHeader = decode . BL.fromChunks . (:[])

messageReadHeader :: Socket -> IO (Int32, Int32, Int32, Int32)
messageReadHeader = liftM messageDecodeHeader . flip recv 16
