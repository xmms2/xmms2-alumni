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

module Xmms.Client.Generated (
      hello
    , listPlaylistEntries
    , medialibGetInfo
) where

import Xmms.Client
import Xmms.Client.Message

hello :: Client -> Int -> String -> IO (Client, Result)
hello client protocolVersion clientName = do
    let (betterClient, cookie) = methodPrelude client
    let h = clientSocket client

    let payloadLength = (4 + 4 + 1 + (length clientName))
    messageWriteHeader h (1, 32, fromIntegral cookie, fromIntegral payloadLength)

    messageWriteInt h protocolVersion
    messageWriteString h clientName

    return (betterClient, VoidResult cookie)

listPlaylistEntries :: Client -> String -> IO (Client, Result)
listPlaylistEntries client playlistName = do
    let (betterClient, cookie) = methodPrelude client
    let h = clientSocket client

    let payloadLength = (5 + (length playlistName))
    messageWriteHeader h (2, 43, fromIntegral cookie, fromIntegral payloadLength)

    messageWriteString h playlistName

    return (betterClient, IntListResult cookie)

medialibGetInfo :: Client -> Int -> IO (Client, Result)
medialibGetInfo client id = do
    let (betterClient, cookie) = methodPrelude client
    let h = clientSocket client

    messageWriteHeader h (5, 32, fromIntegral cookie, 4)

    messageWriteInt h id

    return (betterClient, UnknownDictDictResult cookie)
