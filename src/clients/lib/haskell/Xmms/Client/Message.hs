module Xmms.Client.Message where

import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO

pokeByte :: Ptr Word8 -> Int -> Word8 -> IO ()
pokeByte = pokeElemOff

peekByte :: Ptr Word8 -> Int -> IO (Word8)
peekByte = peekElemOff

messageWriteInt :: Handle -> Int -> IO ()
messageWriteInt h i = do
    allocaBytes 4 $ \buffer -> do
        pokeByte buffer 0 (fromIntegral((i `shiftR` 24) .&. 0xff))
        pokeByte buffer 1 (fromIntegral((i `shiftR` 16) .&. 0xff))
        pokeByte buffer 2 (fromIntegral((i `shiftR` 8) .&. 0xff))
        pokeByte buffer 3 (fromIntegral(i .&. 0xff))

        hPutBuf h buffer 4

messageReadInt :: Handle -> IO Int
messageReadInt handle = do
    allocaBytes 4 $ \buffer -> do
        readBytes <- hGetBuf handle buffer 4

        a <- peekByte buffer 0
        b <- peekByte buffer 1
        c <- peekByte buffer 2
        d <- peekByte buffer 3

        return (( (fromIntegral a) `shiftL` 24) .|. (  (fromIntegral b) `shiftL` 16) .|. ( (fromIntegral c) `shiftL` 8) .|. (fromIntegral d))

messageWriteString :: Handle -> String -> IO ()
messageWriteString h s = do
    messageWriteInt h (succ (length s))
    hPutStr h s
    hPutChar h '\0'

-- Read an IPC Message's header (16 bytes).
-- Returns the length in bytes of the message's payload.
messageReadHeader :: Handle -> IO (Int)
messageReadHeader handle = do
    object <- messageReadInt handle
    command <- messageReadInt handle
    cookie <- messageReadInt handle
    messageReadInt handle

