module Main where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import Data.Char (toUpper)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.ByteString.Char8 as C8
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)

data Player = X | O deriving (Show, Eq, Read)
type Cell = Maybe Player
type Board = [[Cell]]
data GameStatus = Playing | Winner Player | Draw deriving (Show, Eq)
data Command = Move (Int, Int) | Rematch | Quit deriving (Show)

type GameState = TVar (Board, Player, GameStatus) -- Trang thai van dau
type PlayerConnections = (Socket, Socket) -- Ket noi cua nguoi choi
type RematchState = TVar (Maybe Bool, Maybe Bool) -- Lua chon choi lai

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "3000"
    E.bracket (open addr) close loop
  where
    resolve host port = do
        let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 2
        putStrLn "Server is listening on port 3000..."
        return sock

-- ====================================================================
-- MAIN LOOP KET NOI NGUOI CHOI
-- ====================================================================
loop :: Socket -> IO ()
loop sock = forever $ do
    putStrLn "\nWaiting for 2 players to connect..."
    (conn1, peer1) <- accept sock
    putStrLn $ "Player X connected from: " ++ show peer1
    sendAll conn1 (C8.pack "PLAYER:X\n")
    sendAll conn1 (C8.pack "STATUS:Waiting for another player to connect...\n")

    (conn2, peer2) <- accept sock
    putStrLn $ "Player O connected from: " ++ show peer2
    sendAll conn2 (C8.pack "PLAYER:O\n")
    
    let connections = (conn1, conn2)
    void $ forkFinally (gameSession connections) (\_ -> close conn1 >> close conn2)

-- ====================================================================
-- QUAN LY VAN DAU
-- ====================================================================
gameSession :: PlayerConnections -> IO ()
gameSession connections@(connX, connO) = do
    let initialBoard = replicate 3 (replicate 3 Nothing)
    gameState <- newTVarIO (initialBoard, X, Playing)
    rematchState <- newTVarIO (Nothing, Nothing)

    initialState <- atomically $ readTVar gameState
    broadcastNewState connections initialState

    race_
      (playerListener connX connections gameState rematchState X)
      (playerListener connO connections gameState rematchState O)
    putStrLn "Game session ended."

-- ====================================================================
-- GUI TRANG THAI GAME
-- ====================================================================
broadcastNewState :: PlayerConnections -> (Board, Player, GameStatus) -> IO ()
broadcastNewState (connX, connO) (board, currentPlayer, status) = do
    let boardMsg = C8.pack $ "BOARD:" ++ show board ++ "\n"
    let statusMsg = C8.pack $ "STATUS:" ++ show status ++ "\n"
    
    let turnMsgX = C8.pack $ if currentPlayer == X then "TURN:YOURS\n" else "TURN:WAIT\n"
    sendAll connX $ C8.concat [boardMsg, statusMsg, turnMsgX]

    let turnMsgO = C8.pack $ if currentPlayer == O then "TURN:YOURS\n" else "TURN:WAIT\n"
    sendAll connO $ C8.concat [boardMsg, statusMsg, turnMsgO]

-- ====================================================================
-- NHAN TIN NHAN TU CLIENT
-- ====================================================================   
playerListener :: Socket -> PlayerConnections -> GameState -> RematchState -> Player -> IO ()
playerListener conn connections@(connX, connO) gameState rematchState myPlayer = forever $ do
    msgBytes <- recv conn 1024
    when (not (C8.null msgBytes)) $ do
        case parseCommand (C8.unpack msgBytes) of
            Just (Move (r, c)) -> handleMove r c
            Just Rematch -> handleRematch True
            Just Quit -> handleRematch False
            Nothing -> sendAll conn (C8.pack "ERROR:Invalid command\n")
  where
    handleMove r c = do
        (board, currentPlayer, status) <- atomically $ readTVar gameState
        -- Khong hop le
        if status /= Playing then
            sendAll conn (C8.pack "ERROR:Game is over. Choose to rematch or quit.\n")
        else if currentPlayer /= myPlayer then
            sendAll conn (C8.pack "ERROR:Not your turn\n")
        else if not (isValidMove board r c) then
            sendAll conn (C8.pack "ERROR:Invalid move\n")
        -- Hop le
        else do
            let newBoard = updateBoard board r c myPlayer
            let newStatus = checkWinner newBoard myPlayer r c
            let nextPlayer = if myPlayer == X then O else X
            let newState = (newBoard, nextPlayer, newStatus)
            atomically $ writeTVar gameState newState
            broadcastNewState connections newState
    
    -- Xu ly choi lai
    handleRematch choice = do
        atomically $ do
            (pX, pO) <- readTVar rematchState
            if myPlayer == X
                then writeTVar rematchState (Just choice, pO)
                else writeTVar rematchState (pX, Just choice)
        
        (choiceX, choiceO) <- atomically $ do
            (pX, pO) <- readTVar rematchState
            check (isJust pX && isJust pO) -- Tam dung luong den khi dk la True
            return (fromJust pX, fromJust pO)
        
        when (myPlayer == X) $ do
            if choiceX && choiceO then do
                putStrLn "Both players agreed to rematch. Resetting game."
                let initialBoard = replicate 3 (replicate 3 Nothing)
                atomically $ do
                    writeTVar gameState (initialBoard, X, Playing) -- reset ban co
                    writeTVar rematchState (Nothing, Nothing) -- reset lua chon
                broadcastNewState connections (initialBoard, X, Playing)
            else do
                putStrLn "A player chose to quit. Ending session."
                let youQuitMsg = C8.pack "STATUS:You have left the game. Disconnecting.\n"
                let opponentQuitMsg = C8.pack "STATUS:Opponent has left the game. Disconnecting.\n"
                if not choiceX then do -- Player X quit
                    sendAll connX youQuitMsg
                    sendAll connO opponentQuitMsg
                else do -- Player O quit
                    sendAll connX opponentQuitMsg
                    sendAll connO youQuitMsg
                E.throwIO (E.ErrorCall "Game Over")

-- ====================================================================
-- PHAN TICH CHUOI
-- ====================================================================
parseCommand :: String -> Maybe Command
parseCommand str =
  let cleanedStr = filter (\c -> c /= '\n' && c /= '\r') str
      (cmd, rest) = break (== ':') cleanedStr
      upperCmd = map toUpper cmd
  in case (upperCmd, rest) of
    ("MOVE", ':' : coords) -> case break (== ',') coords of
                                (rowStr, ',' : colStr) -> do
                                    r <- readMaybe rowStr
                                    c <- readMaybe colStr
                                    return (Move (r, c))
                                _ -> Nothing
    ("REMATCH", "") -> Just Rematch
    ("QUIT", "")    -> Just Quit
    _               -> Nothing

-- Ktra nuoc di
isValidMove :: Board -> Int -> Int -> Bool
isValidMove board r c = r >= 0 && r < 3 && c >= 0 && c < 3 && isNothing (board !! r !! c)

-- Update ban co
updateBoard :: Board -> Int -> Int -> Player -> Board
updateBoard board r c player = take r board ++ [take c (board !! r) ++ [Just player] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board

-- Ktra ai thang
checkWinner :: Board -> Player -> Int -> Int -> GameStatus
checkWinner board player r c
    | all (== Just player) (board !! r) = Winner player
    | all (== Just player) [board !! i !! c | i <- [0..2]] = Winner player
    | r == c && all (== Just player) [board !! i !! i | i <- [0..2]] = Winner player
    | r + c == 2 && all (== Just player) [board !! i !! (2-i) | i <- [0..2]] = Winner player
    | all (all isJust) board = Draw
    | otherwise = Playing