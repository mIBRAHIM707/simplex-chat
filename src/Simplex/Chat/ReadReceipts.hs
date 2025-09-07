{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.ReadReceipts where

import Simplex.Chat.Protocol
import Simplex.Chat.Messages
import Simplex.Chat.Store
import qualified Simplex.Chat.Store.Profiles as P
import Simplex.Chat.Types
import Simplex.Chat.Controller (CM)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

-- Contact-specific read receipt settings
newtype ContactReadReceiptSettings = ContactReadReceiptSettings
  { enabled :: Bool
  }
  deriving (Show)

-- | Send a read receipt for a message
sendReadReceipt :: User -> Contact -> SharedMsgId -> CM ()
sendReadReceipt user contact sharedMsgId = do
  -- Check if user has read receipts enabled for this contact
  userSettings <- getUserReadReceiptSettings user
  contactSettings <- getContactReadReceiptSettings user contact
  
  when (shouldSendReadReceipt userSettings contactSettings) $ do
    -- Create and send read receipt message
    let readReceiptMsg = XMsgRead sharedMsgId
    -- TODO: Send the message through the existing message sending infrastructure
    -- This will be integrated with the message sending system
    
    -- Record that we sent a read receipt
    withStore $ \db -> do
      timestamp <- liftIO getCurrentTime
      insertReadReceipt db user contact sharedMsgId timestamp

-- | Process an incoming read receipt
processReadReceipt :: Contact -> SharedMsgId -> RcvMessage -> MsgMeta -> CM ()
processReadReceipt contact sharedMsgId _msg _msgMeta = do
  -- TODO: Find the corresponding sent message and update its status to read
  -- This will involve:
  -- 1. Finding the message by sharedMsgId in the database
  -- 2. Updating its status to CISSndRead
  -- 3. Notifying the UI of the status change
  -- 
  -- For now, we'll implement the basic structure
  timestamp <- liftIO getCurrentTime
  updateMessageReadStatus contact sharedMsgId timestamp

-- | Check if read receipt should be sent based on settings
shouldSendReadReceipt :: P.UserReadReceiptSettings -> Maybe ContactReadReceiptSettings -> Bool
shouldSendReadReceipt userSettings contactSettings =
  case contactSettings of
    Just (ContactReadReceiptSettings enabled) -> enabled
    Nothing -> P.enableContacts userSettings

-- | Update message read status in database
updateMessageReadStatus :: Contact -> SharedMsgId -> UTCTime -> CM ()
updateMessageReadStatus contact sharedMsgId timestamp = do
  withStore $ \db -> do
    -- TODO: Find and update the message status
    -- For now, just insert the read receipt record
    insertReadReceipt db undefined undefined sharedMsgId timestamp

-- | Insert read receipt record
insertReadReceipt :: DB.Connection -> User -> Contact -> SharedMsgId -> UTCTime -> CM ()
insertReadReceipt db user contact@Contact{contactId} sharedMsgId timestamp = 
  liftIO $ DB.execute db
    "INSERT INTO read_receipts (user_id, contact_id, shared_msg_id, read_at) VALUES (?,?,?,?)"
    (userId user, contactId, sharedMsgId, timestamp)

-- | Get user's read receipt settings
getUserReadReceiptSettings :: User -> CM P.UserReadReceiptSettings
getUserReadReceiptSettings _user = do
  -- TODO: Implement actual database lookup
  -- For now, return default settings
  let defaultSettings = P.UserReadReceiptSettings { P.enableContacts = True, P.clearOverrides = False }
  return defaultSettings

-- | Get contact-specific read receipt settings
getContactReadReceiptSettings :: User -> Contact -> CM (Maybe ContactReadReceiptSettings)
getContactReadReceiptSettings _user _contact = do
  -- TODO: Implement actual database lookup for contact-specific settings
  -- For now, return Nothing (use user default)
  return Nothing
  return Nothing
