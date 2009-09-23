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
