{-# LANGUAGE OverloadedStrings #-}
module ReadReceiptsTests (readReceiptsTests) where

import ChatClient
import ChatTests.DBUtils (TestParams)
import ChatTests.Utils
import Test.Hspec hiding (it)

-- Minimal direct chat read receipt flow test.
-- Confirms that marking a direct chat as read returns ok after a message is received.
readReceiptsTests :: SpecWith TestParams
readReceiptsTests = do
  let spec = it "marks direct chat read and returns ok" testReadReceiptDirect
  describe "Read receipts" spec
  describe "ReadReceipts" spec

testReadReceiptDirect :: TestParams -> IO ()
testReadReceiptDirect =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice #> "@bob hello_rr"
    bob   <# "alice> hello_rr"
    bob  ##> "/_read chat *1"
    bob  <## "ok"
