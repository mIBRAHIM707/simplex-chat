{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250823_read_receipts_settings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- | Add separate read receipt settings to distinguish from delivery receipts
-- This migration adds read receipt specific columns to users, contacts, and groups tables
-- to allow independent control of read receipts vs delivery receipts
m20250823_read_receipts_settings :: Query
m20250823_read_receipts_settings =
  [sql|
-- Add read receipt settings for users (separate from delivery receipts)
ALTER TABLE users ADD COLUMN send_read_rcpts_contacts INTEGER NOT NULL DEFAULT 0;
ALTER TABLE users ADD COLUMN send_read_rcpts_small_groups INTEGER NOT NULL DEFAULT 0;

-- Add read receipt settings for individual contacts (override user defaults)
ALTER TABLE contacts ADD COLUMN send_read_rcpts INTEGER;

-- Add read receipt settings for individual groups (override user defaults)
ALTER TABLE groups ADD COLUMN send_read_rcpts INTEGER;

-- Create index for faster queries on read receipt settings
CREATE INDEX idx_contacts_send_read_rcpts ON contacts(send_read_rcpts) WHERE send_read_rcpts IS NOT NULL;
CREATE INDEX idx_groups_send_read_rcpts ON groups(send_read_rcpts) WHERE send_read_rcpts IS NOT NULL;

-- Add read receipt statistics to message delivery events for better tracking
ALTER TABLE msg_delivery_events ADD COLUMN read_receipt_requested INTEGER DEFAULT 0;
ALTER TABLE msg_delivery_events ADD COLUMN read_receipt_received_at TEXT;

-- Create index for read receipt tracking
CREATE INDEX idx_msg_delivery_events_read_receipt ON msg_delivery_events(read_receipt_requested, read_receipt_received_at);
|]

down_m20250823_read_receipts_settings :: Query
down_m20250823_read_receipts_settings =
  [sql|
-- Remove read receipt settings
ALTER TABLE users DROP COLUMN send_read_rcpts_contacts;
ALTER TABLE users DROP COLUMN send_read_rcpts_small_groups;
ALTER TABLE contacts DROP COLUMN send_read_rcpts;
ALTER TABLE groups DROP COLUMN send_read_rcpts;

-- Remove read receipt tracking from delivery events
ALTER TABLE msg_delivery_events DROP COLUMN read_receipt_requested;
ALTER TABLE msg_delivery_events DROP COLUMN read_receipt_received_at;

-- Remove indexes
DROP INDEX IF EXISTS idx_contacts_send_read_rcpts;
DROP INDEX IF EXISTS idx_groups_send_read_rcpts;
DROP INDEX IF EXISTS idx_msg_delivery_events_read_receipt;
|]
