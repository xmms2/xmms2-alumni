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

module Xmms.Client where

import Data.Word
import Network (Socket)

-- cookies are unsigned 32 bit integers
type Cookie = Word32

data Result = VoidResult {
    resultCookie :: Cookie
} | IntListResult {
    resultCookie :: Cookie
} | UnknownDictDictResult {
    resultCookie :: Cookie
}

data Client = Client {
      clientName       :: String
    , clientSocket     :: Socket
    , clientResults    :: [Result]
    , clientNextCookie :: Cookie
}

methodPrelude :: Client -> (Client, Cookie)
methodPrelude (Client name socket results 4294967295) =
    ((Client name socket results nextCookie), nextCookie)
    where nextCookie = 0

methodPrelude (Client name socket results oldNextCookie) =
    ((Client name socket results nextCookie), nextCookie)
    where nextCookie = succ oldNextCookie
