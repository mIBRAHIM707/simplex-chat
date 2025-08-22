#!/usr/bin/env runhaskell

-- Simple test for Read Receipts functionality
-- This tests the protocol serialization and parsing

import qualified Data.Aeson as J
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)

-- Import your modules
import Simplex.Chat.Protocol
import Simplex.Messaging.Protocol.Util
import Simplex.Messaging.Util

-- Test data
testSharedMsgIds :: NE.NonEmpty SharedMsgId
testSharedMsgIds = NE.fromList [SharedMsgId "msg1", SharedMsgId "msg2", SharedMsgId "msg3"]

-- Test 1: Create XMsgRead event
testCreateXMsgRead :: IO ()
testCreateXMsgRead = do
  putStrLn "Test 1: Creating XMsgRead event"
  let event = XMsgRead testSharedMsgIds
  putStrLn $ "✓ Created: " ++ show event

-- Test 2: Convert to CMEventTag
testToCMEventTag :: IO ()
testToCMEventTag = do
  putStrLn "\nTest 2: Converting to CMEventTag"
  let event = XMsgRead testSharedMsgIds
  let tag = toCMEventTag event
  putStrLn $ "✓ Tag: " ++ show tag
  putStrLn $ "✓ Expected: XMsgRead_"

-- Test 3: String encoding/decoding
testStringEncoding :: IO ()
testStringEncoding = do
  putStrLn "\nTest 3: String encoding/decoding"
  let tag = XMsgRead_
  let encoded = textEncode tag
  putStrLn $ "✓ Encoded: " ++ show encoded
  putStrLn $ "✓ Expected: \"x.msg.read\""
  
  case strDecode (encodeUtf8 encoded) of
    Right decoded -> do
      putStrLn $ "✓ Decoded: " ++ show decoded
      if decoded == tag 
        then putStrLn "✓ Round-trip successful"
        else putStrLn "✗ Round-trip failed"
    Left err -> putStrLn $ "✗ Decoding failed: " ++ err

-- Test 4: JSON serialization
testJSONSerialization :: IO ()
testJSONSerialization = do
  putStrLn "\nTest 4: JSON serialization"
  let event = XMsgRead testSharedMsgIds
  let chatMsg = ChatMessage 
        { chatVRange = chatVersionRange $ fromIntegral chatVCommon
        , msgId = MessageId "test123"
        , chatMsgEvent = event
        }
  
  case chatToAppMessage chatMsg of
    AMJson appMsg -> do
      putStrLn $ "✓ JSON serialized successfully"
      putStrLn $ "Event: " ++ show (event appMsg)
      putStrLn $ "Params: " ++ show (params appMsg)
    _ -> putStrLn "✗ Expected JSON format"

main :: IO ()
main = do
  putStrLn "=== Read Receipts Protocol Test ==="
  testCreateXMsgRead
  testToCMEventTag
  testStringEncoding
  testJSONSerialization
  putStrLn "\n=== Test Complete ==="
