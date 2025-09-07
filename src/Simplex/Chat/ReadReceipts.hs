{-# LANGUAGE OverloadedStrings #-}

-- | Minimal read receipts support (placeholder implementation).
-- This module is intentionally kept independent from 'Simplex.Chat.Controller'
-- to avoid module cycles while the feature is being incrementally developed.
module Simplex.Chat.ReadReceipts
  ( ContactReadReceiptSettings (..),
    sendReadReceipt,
    processReadReceipt,
    shouldSendReadReceipt,
    contactCanSendReadReceipts,
    getUserReadReceiptSettings,
    getContactReadReceiptSettings
  )
where

import Control.Monad (when)
import Data.Time (UTCTime, getCurrentTime)
import Simplex.Chat.Messages (MsgMeta, RcvMessage, SharedMsgId, XMsg (..))
import Simplex.Chat.Types (Contact, User)
import qualified Simplex.Chat.Store.Profiles as P

-- | Contact-specific read receipt override (if Nothing - follow user preference)
newtype ContactReadReceiptSettings = ContactReadReceiptSettings
  { enableReadReceipts :: Bool
  }
  deriving (Show, Eq)

-- | Send a read receipt for a message. Currently a stub that only evaluates
-- settings logic; integration with the messaging pipeline will be added later.
sendReadReceipt :: User -> Contact -> SharedMsgId -> IO ()
sendReadReceipt user contact sharedMsgId = do
  userSettings <- getUserReadReceiptSettings user
  contactSettings <- getContactReadReceiptSettings user contact
  when (shouldSendReadReceipt userSettings contactSettings) $ do
    let _readReceiptMsg = XMsgRead sharedMsgId
    pure ()

-- | Process an incoming read receipt. Placeholder for DB update + UI notify.
processReadReceipt :: Contact -> SharedMsgId -> RcvMessage -> MsgMeta -> IO ()
processReadReceipt _contact _sharedMsgId _msg _msgMeta = do
  _timestamp <- getCurrentTime
  pure ()

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

-- | Placeholder: fetch user-level read receipt settings (from DB later).
getUserReadReceiptSettings :: User -> IO P.UserReadReceiptSettings
getUserReadReceiptSettings _ =
  pure P.UserReadReceiptSettings {P.enableContacts = True, P.clearOverrides = False}

-- | Placeholder: fetch contact override (Nothing means follow user settings).
getContactReadReceiptSettings :: User -> Contact -> IO (Maybe ContactReadReceiptSettings)
getContactReadReceiptSettings _ _ = pure Nothing
