{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.HomeworkHandleErrorNestedContract where

import Control.Monad.Freer.Extras as Extras
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import GHC.Generics               (Generic)
import Data.Void                  (Void)
import Ledger
import Ledger.Ada                 as Ada
import Ledger.Constraints         as Constraints
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

-- Two versions of the same contract below 

payContract :: Contract () PaySchema Void ()
payContract = do
    Contract.logError `Contract.handleError` contract
    payContract
    where 
         contract:: Contract () PaySchema Text ()
         contract = do
             pp <- endpoint @"pay"
             let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
             Contract.logInfo @String "Before Transaction"
             void $ submitTx tx
             Contract.logInfo @String "After Transaction"

payContract' :: Contract () PaySchema Void ()
payContract' = do
    Contract.logError `Contract.handleError` contract
    payContract'
    where 
         contract = do
             pp <- endpoint @"pay" @_ @_ @_ @Text
             let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
             Contract.logInfo @String "Before Transaction"
             void $ submitTx tx
             Contract.logInfo @String "After Transaction"

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace p1 p2 = do
     h1 <- activateContractWallet (Wallet 1) payContract
     callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace    = p1
        }
     s1 <- Emulator.waitNSlots 1
     Extras.logInfo $ show p1 ++ " attempted to be given at slot " ++ show (s1-1)
     callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace    = p2
        }
     s2 <- Emulator.waitNSlots 1
     Extras.logInfo $ show p2 ++ " attempted to be given at slot " ++ show (s2-1)


payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
