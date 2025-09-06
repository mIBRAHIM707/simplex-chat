{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.ReadReceipts where

import Simplex.Chat.Protocol
import Simplex.Chat.Messages
import Simplex.Chat.Store
import Simplex.Chat.Types
import Data.Time (UTCTime)

-- | Send a read receipt for a message
sendReadReceipt :: ChatMonad m => User -> Contact -> SharedMsgId -> m ()
sendReadReceipt user contact sharedMsgId = do
  -- Check if user has read receipts enabled for this contact
  userSettings <- getUserReadReceiptSettings user
  contactSettings <- getContactReadReceiptSettings user contact
  
  when (shouldSendReadReceipt userSettings contactSettings) $ do
    let readReceiptEvent = XMsgRead sharedMsgId
    -- Send the read receipt through the existing message sending infrastructure
    sendMsgToContact user contact readReceiptEvent

-- | Process an incoming read receipt
processReadReceipt :: ChatMonad m => User -> Contact -> SharedMsgId -> UTCTime -> m ()
processReadReceipt user contact sharedMsgId timestamp = do
  -- Update message status to read in database
  updateMessageReadStatus user contact sharedMsgId timestamp
  -- Notify UI of status change
  notifyMessageStatusUpdate user contact sharedMsgId CISSndRead

-- | Check if read receipt should be sent based on settings
shouldSendReadReceipt :: UserReadReceiptSettings -> Maybe ContactReadReceiptSettings -> Bool
shouldSendReadReceipt userSettings contactSettings =
  case contactSettings of
    Just (ContactReadReceiptSettings enabled) -> enabled
    Nothing -> userReadReceiptsContacts userSettings

-- | Update message read status in database
updateMessageReadStatus :: ChatMonad m => User -> Contact -> SharedMsgId -> UTCTime -> m ()
updateMessageReadStatus user contact sharedMsgId timestamp = do
  withStore $ \db -> do
    updateChatItemStatus db user (contactId' contact) sharedMsgId CISSndRead
    insertReadReceipt db user contact sharedMsgId timestamp

-- | Insert read receipt record
insertReadReceipt :: ChatMonad m => DatabaseConnection -> User -> Contact -> SharedMsgId -> UTCTime -> m ()
insertReadReceipt db user contact sharedMsgId timestamp = 
  liftIO $ execute db
    "INSERT INTO read_receipts (user_id, contact_id, shared_msg_id, read_at) VALUES (?,?,?,?)"
    (userId user, contactId' contact, sharedMsgId, timestamp)
