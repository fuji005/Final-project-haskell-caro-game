module Main where

import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forM_, forever, when)
import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import Data.List (isPrefixOf, isInfixOf)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)

data Player = X | O deriving (Show, Eq, Read)
data GamePhase = Playing | GameOver deriving (Eq, Show)
type ClientState = TVar (Maybe Player, GamePhase, Bool) -- (Player, Phase, IsMyTurn)

main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "127.0.0.1" "3000"
  -- dong socket 
  E.bracket (open addr) close run
  where
    resolve host port = do
        let hints = defaultHints {addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        putStrLn "Connecting to server..."
        connect sock (addrAddress addr)
        return sock

-- ====================================================================
-- LOGIC CLIENT
-- ====================================================================
run :: Socket -> IO ()
run sock = do
  hSetBuffering stdout NoBuffering
  clearScreen
  putStrLn "Connected."

  -- Tao lock ngan 2 luong ghi de 
  lock <- newMVar ()
  clientState <- newTVarIO (Nothing, Playing, False)
  -- race hai luong
  race_ (serverHandler sock lock clientState) (userInputHandler sock clientState)

-- ====================================================================
-- FOLLOW SERVER UPDATE GIAO DIEN
-- ====================================================================
serverHandler :: Socket -> MVar () -> ClientState -> IO ()
serverHandler sock lock clientState = forever $ do
  msgBytes <- recv sock 1024
  -- null se nem loi de end client
  if C8.null msgBytes
    then E.throwIO (E.ErrorCall "Connection closed by server")
    else do
      let messages = C8.lines msgBytes
      let unpackedMsgs = map C8.unpack messages
      
      -- Update trang thai
      withMVar lock $ \_ -> do
        forM_ unpackedMsgs $ \msg -> atomically $
            case break (== ':') msg of
              -- Tin nhan player
              ("PLAYER", ':' : playerStr) ->
                let player = if playerStr == "X" then Just X else Just O
                in modifyTVar' clientState (\(_, phase, turn) -> (player, phase, turn))
              -- Tin nhan win/draw
              (prefix, _) | "STATUS:Winner" `isPrefixOf` msg || "STATUS:Draw" `isPrefixOf` msg ->
                modifyTVar' clientState (\(p, _, turn) -> (p, GameOver, turn))
              -- Tin nhan turn
              ("TURN", ':' : turn) ->
                modifyTVar' clientState (\(p, phase, _) -> (p, phase, turn == "YOURS"))
              _ -> return ()
        
        (maybePlayer, currentPhase, isMyTurn) <- atomically $ readTVar clientState

        -- Update giao dien
        let isBoardMsg = any ("BOARD:" `isPrefixOf`) unpackedMsgs
        when isBoardMsg $ do
          clearScreen
          setCursorPosition 0 0
          case maybePlayer of
            Just player -> putStrLn $ "You are Player " ++ show player
            Nothing       -> return ()

        forM_ unpackedMsgs $ \msg -> parseAndDisplay currentPhase msg

        -- Hien thi loi nhac
        let shouldExit = any ("Disconnecting." `isInfixOf`) unpackedMsgs
        when (not shouldExit) $ do
            if currentPhase == GameOver
                then do
                    putStr "Play again? (yes/no): "
                    hFlush stdout
                else when isMyTurn $ do
                    putStr "> Enter your move (e.g., MOVE:1,1): "
                    hFlush stdout
      
      -- Exit game
      let shouldExit = any ("Disconnecting." `isInfixOf`) unpackedMsgs
      when shouldExit $ E.throwIO (E.ErrorCall "Exiting game.")

-- ====================================================================
-- DOC INPUT
-- ====================================================================
userInputHandler :: Socket -> ClientState -> IO ()
userInputHandler sock clientState = forever $ do
  line <- getLine
  (_, phase, _) <- atomically $ readTVar clientState
  
  let cmd = if phase == Playing
            then C8.pack line
            else case map toLower line of
                   "yes" -> C8.pack "REMATCH"
                   "y"   -> C8.pack "REMATCH"
                   _     -> C8.pack "QUIT"
  sendAll sock cmd
  
  -- reset lai Playing neu yes
  when (phase == GameOver && (map toLower line == "yes" || map toLower line == "y")) $ do
      atomically $ modifyTVar' clientState (\(p, _, _) -> (p, Playing, False))

-- ====================================================================
-- IN TIN NHAN TU SERVER
-- ====================================================================
parseAndDisplay :: GamePhase -> String -> IO ()
parseAndDisplay phase msg =
  case break (== ':') msg of
    ("BOARD", ':' : boardStr) -> printBoard boardStr
    ("STATUS", ':' : status)  -> putStrLn $ "Game status: " ++ status
    ("PLAYER", ':' : _)       -> return ()
    ("TURN", ':' : turn)      -> when (phase == Playing) $ putStrLn $ "Turn: " ++ turn
    ("ERROR", ':' : err)      -> putStrLn $ "Error: " ++ err
    _                         -> return ()

-- CHUYEN NUOC DI THANH X/O
cellToChar :: Maybe Player -> Char
cellToChar Nothing = ' '
cellToChar (Just X) = 'X'
cellToChar (Just O) = 'O'

-- IN BAN CO
printBoard :: String -> IO ()
printBoard boardStr = do
  let board = read boardStr :: [[Maybe Player]]
  putStrLn ""
  putStrLn "  0 1 2"
  putStrLn " -------"
  mapM_ printRow (zip [0..] board)
  putStrLn " -------"
  where
    printRow (i, row) = putStrLn $ show i ++ "|" ++ [cellToChar (row !! 0)] ++ " " ++ [cellToChar (row !! 1)] ++ " " ++ [cellToChar (row !! 2)] ++ "|"