module Xmms.Client where

import Data.Word
import System.IO

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
    , clientSocket     :: Handle
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
