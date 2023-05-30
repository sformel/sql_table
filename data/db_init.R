#Based on https://github.com/Tychobra/shiny_crud

library(RSQLite)
library(tibble)

# Create a connection object with SQLite
conn <- dbConnect(
  RSQLite::SQLite(),
  "data/DwC.sqlite3"
)

# Create a query to prepare the 'DwC Event Terms' table with additional 'uid'. Eventually will add the 4 created/modified columns

sql_prep <- data.frame(attribute = names(DwC_events),
                       type = lapply(DwC_events, class) %>% unlist()
                       ) %>% 
  mutate(type = case_when(type == "character" ~ "TEXT"))

create_DwC_query = "CREATE TABLE events (
  uid                             TEXT PRIMARY KEY,
  name                            TEXT,
  description                     TEXT,
  created_at                      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by                      TEXT,
  modified_at                     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_by                     TEXT
)"

# dbExecute() executes a SQL statement with a connection object
# Drop the table if it already exists
dbExecute(conn, "DROP TABLE IF EXISTS event")

# Execute the query created above
dbExecute(conn, create_DwC_query)

# Read in the RDS file created in 'data_prep.R'
dat <- DwCevents

# add uid column to the `dat` data frame
dat$uid <- uuid::UUIDgenerate(n = nrow(dat))

# reorder the columns
dat <- dat %>%
  select(uid, everything())

# Fill in the SQLite table with the values from the RDS file
DBI::dbWriteTable(
  conn,
  name = "events",
  value = dat,
  overwrite = FALSE,
  append = TRUE
)

# List tables to confirm 'event' table exists
dbListTables(conn)

# disconnect from SQLite before continuing
dbDisconnect(conn)
