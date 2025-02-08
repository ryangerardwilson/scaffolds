CREATE TABLE events (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  endpoint TEXT NOT NULL,
  data_received TEXT,
  data_returned TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP  
);
