{-# LANGUAGE OverloadedStrings #-}
module ReadReceiptsTests (readReceiptsTests) where

import ChatClient
import ChatTests.Utils
import Test.Hspec as H

-- | Tests basic direct chat read receipts flow.
-- Scenario:
-- 1. Connect alice and bob.
-- 2. Alice sends a message to Bob.
-- 3. Bob sees incoming message (status implicit CISRcvNew).
-- 4. Bob marks chat read via /_read chat @alice (by chat ref *1).
-- 5. Alice should eventually see status update indicating read (we assert by refiring tail and expecting no errors).
-- We cannot easily introspect internal status text without extending test harness, so we focus on command 'ok' response
-- and absence of errors. Future enhancement: query internal DB for item_status = 'snd_read'.
readReceiptsTests :: Spec
readReceiptsTests = H.describe "Read receipts" $ do
  H.it "direct chat read emits receipt without error" $ \ps ->
    -- testChat2 sets up 2 users and passes TestCC handles
    testChat2 aliceProfile bobProfile $ \alice bob -> do
      -- connect users
      connectUsers alice bob
      -- Alice sends a message
      alice #> "@bob hello_rr"
      bob   <# "alice> hello_rr"
      -- Bob marks chat read (direct chat ref for first contact is @alice -> *1)
      bob  ##> "/_read chat *1"
      bob  <## "ok"
      -- Alice receives (could include status updates); we just ensure tail works
      alice ##> "/tail @bob"
      -- message still present
      alice <# "@bob hello_rr"
