{-# LANGUAGE OverloadedStrings #-}

-- | Read receipts support implementation
module Simplex.Chat.ReadReceipts
  ( ContactReadReceiptSettings (..),
    sendReadReceipt,
    processReadReceipt,
    shouldSendReadReceipt,
    contactCanSendReadReceipts,
    getUserReadReceiptSettings,
    getContactReadReceiptSettings,
    markMessageAsRead,
    updateReadReceiptStatus
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Query, execute, query_)
import Database.SQLite.Simple.QQ (sql)
-- Use Protocol for SharedMsgId and XMsg constructor; MsgMeta comes from Agent Protocol
import Simplex.Chat.Protocol (SharedMsgId, ChatMsgEvent (XMsgRead))
import Simplex.Chat.Messages (RcvMessage, CIStatus (..))
import Simplex.Chat.Types (Contact, User (..), ChatItem (..), CIMeta (..))
import Simplex.Messaging.Agent.Protocol (MsgMeta)
import qualified Database.SQLite.Simple as DB
import qualified Simplex.Chat.Store.Profiles as P

-- | Contact-specific read receipt override (if Nothing - follow user preference)
newtype ContactReadReceiptSettings = ContactReadReceiptSettings
  { enableReadReceipts :: Bool
  }
  deriving (Show, Eq)

-- | Send a read receipt for a message when it's read by the user
sendReadReceipt :: User -> Contact -> SharedMsgId -> IO ()
sendReadReceipt user contact sharedMsgId = do
  userSettings <- getUserReadReceiptSettings user
  contactSettings <- getContactReadReceiptSettings user contact
  when (shouldSendReadReceipt userSettings contactSettings) $ do
    -- TODO: Integrate with actual messaging system to send XMsgRead
    let _readReceiptMsg = XMsgRead sharedMsgId
    -- This will be implemented when integrating with Controller
    pure ()

-- | Process an incoming read receipt and update message status
processReadReceipt :: DB.Connection -> Contact -> SharedMsgId -> RcvMessage -> MsgMeta -> IO ()
processReadReceipt db contact sharedMsgId _msg _msgMeta = do
  timestamp <- getCurrentTime
  -- Update the status of the sent message to "read"
  execute db
    [sql|
      UPDATE chat_items 
      SET item_status = 'snd_read', read_at = ?, updated_at = ?
      WHERE shared_msg_id = ? AND chat_dir = 'snd'
    |]
    (timestamp, timestamp, sharedMsgId)
  
  -- Store the read receipt in the read_receipts table
  execute db
    [sql|
      INSERT INTO read_receipts (chat_item_id, user_id, contact_id, read_at, created_at)
      SELECT chat_item_id, user_id, ?, ?, ?
      FROM chat_items 
      WHERE shared_msg_id = ? AND chat_dir = 'snd'
    |]
    (contact, timestamp, timestamp, sharedMsgId)

-- | Mark a received message as read and send read receipt if enabled
markMessageAsRead :: DB.Connection -> User -> Contact -> SharedMsgId -> IO ()
markMessageAsRead db user contact sharedMsgId = do
  timestamp <- getCurrentTime
  -- Update the received message status
  execute db
    [sql|
      UPDATE chat_items 
      SET item_status = 'rcv_read', read_at = ?, updated_at = ?
      WHERE shared_msg_id = ? AND chat_dir = 'rcv' AND user_id = ?
    |]
    (timestamp, timestamp, sharedMsgId, userId user)
  
  -- Send read receipt if enabled
  sendReadReceipt user contact sharedMsgId

-- | Update message status when read receipt is received
updateReadReceiptStatus :: DB.Connection -> SharedMsgId -> IO ()
updateReadReceiptStatus db sharedMsgId = do
  timestamp <- getCurrentTime
  execute db
    [sql|
      UPDATE chat_items 
      SET item_status = 'snd_read', read_at = ?, updated_at = ?
      WHERE shared_msg_id = ? AND chat_dir = 'snd'
    |]
    (timestamp, timestamp, sharedMsgId)

-- | Decide whether to send read receipt given user-wide and per-contact settings.
shouldSendReadReceipt :: P.UserReadReceiptSettings -> Maybe ContactReadReceiptSettings -> Bool
shouldSendReadReceipt userSettings m = case m of
  Just (ContactReadReceiptSettings enabled) -> enabled
  Nothing -> P.enableContacts userSettings

-- | Whether we accept read receipts from this contact (used for UI filtering later).
contactCanSendReadReceipts :: Maybe ContactReadReceiptSettings -> Bool
contactCanSendReadReceipts m = case m of
  Just (ContactReadReceiptSettings enabled) -> enabled
  Nothing -> True

-- | Fetch user-level read receipt settings from database
getUserReadReceiptSettings :: User -> IO P.UserReadReceiptSettings
getUserReadReceiptSettings _user = do
  -- TODO: Actually fetch from database using user ID
  -- For now, return default settings with read receipts enabled
  pure $ P.UserReadReceiptSettings True False

-- | Fetch contact-specific read receipt override from database
getContactReadReceiptSettings :: User -> Contact -> IO (Maybe ContactReadReceiptSettings)
getContactReadReceiptSettings _user _contact = do
  -- TODO: Actually fetch from database using contact ID
  -- For now, return Nothing to use user defaults
  pure Nothing
