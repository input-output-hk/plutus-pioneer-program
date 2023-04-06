
module State where

import           Control.Monad.State (State, get, put, runState)

---------------------------------------------------------------------------------------------------
--------------------------------- HELPER FUNCTIONS/TYPES ------------------------------------------

-- Mock UTxO type
data UTxO = UTxO { owner :: String , value :: Integer }
    deriving (Show, Eq)

-- Mock blockchain type
newtype Mock = Mock { utxos :: [UTxO] }
    deriving (Show, Eq)

-- Initial blockchain state
initialMockS :: Mock
initialMockS = Mock [ UTxO "Alice" 1000 ]

---------------------------------------------------------------------------------------------------
------------------------------------ WITHOUT STATE MONAD ------------------------------------------

sendValue :: String -> Integer -> String -> Mock -> (Bool, Mock)
sendValue from amount to mockS =
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    in if totalSenderFunds >= amount
        then (True, Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos)
        else (False, mockS)


multipleTx :: (Bool, Mock)
multipleTx =
    let (isOk,  mockS1) = sendValue "Alice" 100 "Bob"   initialMockS
        (isOk2, mockS2) = sendValue "Alice" 300 "Bob"   mockS1
        (isOk3, mockS3) = sendValue "Bob"   200 "Rick"  mockS2
    in (isOk && isOk2 && isOk3, mockS3)

---------------------------------------------------------------------------------------------------
-------------------------------------- WITH STATE MONAD -------------------------------------------

-- newtype State s a = State { runState :: s -> (a, s) }

sendValue' :: String -> Integer -> String -> State Mock Bool
sendValue' from amount to = do
    mockS <- get
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    if totalSenderFunds >= amount
        then do
            put $ Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos
            return True
        else return False

multipleTx' :: (Bool, Mock)
multipleTx' = runState (do
    isOk  <- sendValue' "Alice" 100 "Bob"
    isOk2 <- sendValue' "Alice" 300 "Bob"
    isOk3 <- sendValue' "Bob"   200 "Rick"
    return (isOk && isOk2 && isOk3))
    initialMockS


type Run a = State Mock a
