{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250906_read_receipts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250906_read_receipts :: Query
m20250906_read_receipts =
  [sql|
-- Add read receipt settings for users and contacts
ALTER TABLE users ADD COLUMN send_read_rcpts_contacts INTEGER NOT NULL DEFAULT 0;
ALTER TABLE contacts ADD COLUMN send_read_rcpts INTEGER;

-- Add read receipt timestamp to chat items
ALTER TABLE chat_items ADD COLUMN read_at TEXT;

-- Add read receipt status tracking 
CREATE TABLE IF NOT EXISTS read_receipts (
  chat_item_id INTEGER NOT NULL,
  user_id INTEGER NOT NULL,
  contact_id INTEGER,
  read_at TEXT NOT NULL,
  created_at TEXT NOT NULL,
  FOREIGN KEY (chat_item_id) REFERENCES chat_items (chat_item_id) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users (user_id) ON DELETE CASCADE,
  FOREIGN KEY (contact_id) REFERENCES contacts (contact_id) ON DELETE CASCADE
);

CREATE INDEX idx_read_receipts_chat_item_id ON read_receipts (chat_item_id);
CREATE INDEX idx_read_receipts_contact_id ON read_receipts (contact_id);
|]

down_m20250906_read_receipts :: Query
down_m20250906_read_receipts =
  [sql|
ALTER TABLE users DROP COLUMN send_read_rcpts_contacts;
ALTER TABLE contacts DROP COLUMN send_read_rcpts;
ALTER TABLE chat_items DROP COLUMN read_at;
DROP TABLE read_receipts;
|]
