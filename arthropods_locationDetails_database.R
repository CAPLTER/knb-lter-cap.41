# READ ME ----

# Unlike birds and herpetofauna, arthropod sampling locations will not move but
# rather are will come on- and off-line. As a result, a dedicated table/resource
# to track the movement of sites is not required. However, the position of sites
# and the timing of their existence throughout the project is required, and
# those details can simply be added to the sites table. As with other programs,
# spatial information can be stored in the database and pulled as needed rather
# than managing separate geospatial files. This workflow draws on spatial
# information and some details about the start and end dates of select sites
# from the PO10_AllSites.shp shapefile.

# libraries ----
library(RMySQL)
library(tidyverse)

# DB connections ----
con <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='lter10_arthropods_production',
                 host='stegosaurus.gios.asu.edu')

local <- dbConnect(MySQL(),
                   user='srearl',
                   password=.rs.askForPassword("Enter password:"),
                   dbname='lter10_arthropods_production',
                   host='127.0.0.1')

# data acquisition ----

# current data in the sites table for assessment only
sites <- dbGetQuery(con, "
SELECT *
FROM lter10_arthropods_production.sites;")

# site details were pulled from the PO10_AllSites.shp shapefile that reflects a
# combination of GPS data collected in 2007 and historic location information.
# Included also were some start and end dates. Here forward, spatial information
# can be pulled from the database and the maintenance of geospatial shapefiles
# should no longer be required.
site_details <- read_csv('PO10_AllSites.csv')

# database modifications from shapefile data ----

# general processing steps
# 1. write site_details to a temporary table
# 2. update sites table structure to receive additional details
# 3. update sites table with locational and chronology details from site_details

if (dbExistsTable(con, 'temp_site_details')) dbRemoveTable(con, 'temp_site_details') # make sure tbl does not exist
dbWriteTable(con, 'temp_site_details', value = site_details, row.names = F)

dbSendQuery(con, "
ALTER TABLE temp_site_details
  MODIFY COLUMN GPS_Date DATE,
  MODIFY COLUMN Start_date DATE,
  MODIFY COLUMN End_date DATE;
")

dbSendQuery(con, "
ALTER TABLE sites
  ADD COLUMN `lat` DOUBLE AFTER `location`,
  ADD COLUMN `long` DOUBLE AFTER `lat`,
  ADD COLUMN `gps_date` DATE AFTER `long`,
  ADD COLUMN `start_date` DATE AFTER `gps_date`,
  ADD COLUMN `end_date` DATE AFTER `start_date`;
")
  
dbSendQuery(con, "
UPDATE sites s
JOIN temp_site_details tsd ON (s.site_code = tsd.SITE_ID)
  SET s.lat = tsd.Y;
")

dbSendQuery(con, "
UPDATE sites s
JOIN temp_site_details tsd ON (s.site_code = tsd.SITE_ID)
  SET s.long = tsd.X;
")

dbSendQuery(con, "
UPDATE sites s
JOIN temp_site_details tsd ON (s.site_code = tsd.SITE_ID)
  SET s.gps_date = tsd.GPS_Date;
")

dbSendQuery(con, "
UPDATE sites s
JOIN temp_site_details tsd ON (s.site_code = tsd.SITE_ID)
  SET s.start_date = tsd.Start_date;
")

dbSendQuery(con, "
UPDATE sites s
JOIN temp_site_details tsd ON (s.site_code = tsd.SITE_ID)
  SET s.end_date = tsd.End_date;
")

dbRemoveTable(con, 'temp_site_details')

# subsequent manual changes ----