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

    -- object id
    messageWriteInt h 1

    -- command id
    messageWriteInt h 32

    -- cookie
    messageWriteInt h (fromIntegral cookie)

    -- payload length
    messageWriteInt h (4 + 4 + 1 + (length clientName))

    -- protocol
    messageWriteInt h protocolVersion

    messageWriteString h clientName

    return (betterClient, VoidResult cookie)

listPlaylistEntries :: Client -> String -> IO (Client, Result)
listPlaylistEntries client playlistName = do
    let (betterClient, cookie) = methodPrelude client
    let h = clientSocket client

    -- object id
    messageWriteInt h 2

    -- command id
    messageWriteInt h 43

    -- cookie
    messageWriteInt h (fromIntegral cookie)

    -- payload length
    messageWriteInt h (5 + (length playlistName))

    messageWriteString h playlistName

    return (betterClient, IntListResult cookie)

medialibGetInfo :: Client -> Int -> IO (Client, Result)
medialibGetInfo client id = do
    let (betterClient, cookie) = methodPrelude client
    let h = clientSocket client

    -- object id
    messageWriteInt h 5

    -- command id
    messageWriteInt h 32

    -- cookie
    messageWriteInt h (fromIntegral cookie)

    -- payload length
    messageWriteInt h 4

    messageWriteInt h id

    return (betterClient, UnknownDictDictResult cookie)
