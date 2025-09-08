{-# LANGUAGE OverloadedStrings #-}

-- | Read receipts support implementation
module Simplex.Chat.ReadReceipts
  ( ContactReadReceiptSettings (..),
  shouldSendReadReceipt
  )
where

import qualified Simplex.Chat.Store.Profiles as P

-- | Contact-specific read receipt override (if Nothing - follow user preference)
newtype ContactReadReceiptSettings = ContactReadReceiptSettings
  { enableReadReceipts :: Bool
  }
  deriving (Show, Eq)

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

-- NOTE: Remaining functions removed for minimal implementation now handled in Commands logic.
