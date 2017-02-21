
# README ----

# version 13 

# when addressing the McDowell arthropods for the first time, shortly after this
# version 12 was published, it became apparant that not including the trap count
# was a glaring ommission that, though leaving it out was not technically
# incorrect, could compromise the interpretation of these data. The McDowell
# arthropods data has a complicated SQL statement that has a critical LEFT join
# of trap_trap_sampling_events onto the specimen data (taxa, counts, etc.). That
# worked fine for McDowell with ~5K results, but was overwhelming for the core
# data with >100K results. As a workaround, I created a View
# (lter10_arthropods_production.specimens_data) to include with the Core data
# query that makes the load more manageable. Will likely have to go this route
# for the McDowell data as well if that project continues. The only change from
# version 12 to this version 13 is the updated query that includes the trap
# count data. As a result, only the XML corresponding to the core_arthropods_DT
# was generated and that was simply copied and pasted into the bulk of the
# version 12 XML file.

# version 12 

# is the first update to these data using REML and the first update addressed by
# SRE (version 11 by D. Julian). Additions include a sites table to pull out and
# feature additional information about the sampling locations that were not
# included in earlier publications - a trend to normalization but, hopefully, an
# appropriate one.

# Specifically regarding the spatial data, unlike birds and herpetofauna, 
# arthropod sampling locations will not move but rather will come on- and 
# off-line. As such, a dedicated table/resource to track the movement of sites
# is not required. However, the position of sites and the timing of their 
# existence throughout the project is required, and those details can simply be 
# added to the sites table. As with other programs, spatial information can be 
# stored in the database and pulled as needed rather than managing separate 
# geospatial files. This workflow draws on spatial information and some details 
# about the start and end dates of select sites from the PO10_AllSites.shp 
# shapefile (see arthropods_locationDetails_database.R in this directory for the
# workflow). To sum, the lat/long data in the sites table draws on the best
# available spatial information, any gis files can generally be dismissed, and
# the locations of new sites can simply be added to the sites table, which is
# now the authoritative source of information for the location of sampling
# sites.

# Eyal Schohat, Mark Hostetler, Nancy McIntyre, and Stan Faeth added as
# associated parties.

# reml slots ----
getSlots("dataset")
  getSlots("distribution")
  getSlots("keywordSet")
    getSlots("keyword")
getSlots("dataTable")
getSlots("physical")
  getSlots("dataFormat")
    getSlots("textFormat")
  getSlots("size")
  getSlots("distribution")
    getSlots("online")
      getSlots("url")
getSlots("additionalInfo")
  getSlots("section")
  getSlots("para")
getSlots("metadataProvider")
  getSlots("individualName")
  getSlots("userId")
getSlots("creator")
  getSlots("individualName")
  getSlots("userId")

# libraries ----
library("EML")
library('RPostgreSQL')
library('RMySQL')
library('tidyverse')
library("tools")
library("readr")
library("readxl")

# reml-helper-functions ----
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createKMLFn.R')
# source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createdataTableFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createOtherEntityFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createPeople.R')

# DB connections ----
con <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='lter10_arthropods_production',
                 host='stegosaurus.gios.asu.edu')

prod <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='gios2_production',
                 host='mysql.prod.aws.gios.asu.edu')

# pg <- dbConnect(dbDriver("PostgreSQL"),
#                  user="srearl",
#                  dbname="working",
#                  host="localhost",
#                  password=.rs.askForPassword("Enter password:"))
# 
# pg <- dbConnect(dbDriver("PostgreSQL"),
#                  user="srearl",
#                  dbname="caplter",
#                  host="stegosaurus.gios.asu.edu",
#                  password=.rs.askForPassword("Enter password:"))

# dataset details to set first ----
projectid <- 41
packageIdent <- 'knb-lter-cap.41.13'
pubDate <- '2017-02-20'

# data entity ----

# CORE_ARTHROPODS

core_arthropods <- dbGetQuery(con, "
SELECT  
  s.site_code,
  se.sample_date,
  coalesce(specimens_data.observer, outer_join_people.observer) AS observer,
  t.trap_name,
  tse.comments AS trap_sampling_events_comments,
  tse.flags AS trap_sampling_events_flags,
  count_data.trap_count,
  specimens_data.arth_class,
  specimens_data.arth_order,
  specimens_data.arth_family,
  specimens_data.arth_genus_subgenus,
  specimens_data.display_name,
  specimens_data.lt2mm,
  specimens_data._2_5mm,
  specimens_data._5_10mm,
  specimens_data.gt10mm,
  specimens_data.unsized
FROM lter10_arthropods_production.trap_sampling_events tse
JOIN lter10_arthropods_production.sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
JOIN lter10_arthropods_production.sites s ON (s.site_id = se.site_id)
JOIN lter10_arthropods_production.traps t ON (tse.trap_id = t.trap_id)
LEFT JOIN lter10_arthropods_production.people outer_join_people ON (outer_join_people.person_id = se.default_person_for_trap_samples)
LEFT JOIN lter10_arthropods_production.specimens_data ON (specimens_data.trap_sampling_event_id = tse.trap_sampling_event_id)
JOIN
(
  SELECT
  trap_sampling_events.sampling_event_id,
  COUNT(DISTINCT trap_sampling_events.trap_sampling_event_id) AS trap_count
  FROM lter10_arthropods_production.trap_sampling_events
  WHERE NOT 
  (
    trap_sampling_events.flags LIKE 'trap not collected' OR 
    trap_sampling_events.flags LIKE 'NotCollected' OR
    trap_sampling_events.flags LIKE 'missing'
  )  OR 
    trap_sampling_events.flags IS NULL OR
    trap_sampling_events.flags = ''
  GROUP BY sampling_event_id
) AS count_data ON (count_data.sampling_event_id = tse.sampling_event_id)
ORDER BY se.sample_date, s.site_code;")

core_arthropods[core_arthropods == ''] <- NA

core_arthropods <- core_arthropods %>% 
  mutate(sample_date = as.Date(sample_date)) %>% 
  mutate(lt2mm = as.numeric(lt2mm)) %>% 
  mutate(`_2_5mm` = as.numeric(`_2_5mm`)) %>% 
  mutate(`_5_10mm` = as.numeric(`_5_10mm`)) %>% 
  mutate(gt10mm = as.numeric(gt10mm)) %>% 
  mutate(unsized = as.numeric(unsized))

writeAttributes(core_arthropods) # write data frame attributes to a csv in current dir to edit metadata
core_arthropods_desc <- "Tabular file detailing key data from the CAP LTER's long-term monitoring of ground-dwelling arthropods. This data entity includes sample event details, taxonomic details and the number of collected organisms, sizes of organisms where noted in the early stages of the project, and general notes and details regarding the gathering of specimen data."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
core_arthropods_DT <- createDTFF(dfname = core_arthropods,
                                 # factors = meter_factors,
                                 description = core_arthropods_desc)


# CORE_ARTHROPOD_SITES 
core_arthropod_sites <- dbGetQuery(con, "
SELECT
  site_code,
  location,
  lat,
  `long`,
  gps_date,
  start_date,
  end_date,
  num_traps,
  trap_arrange,
  comments
FROM lter10_arthropods_production.sites;")

core_arthropod_sites <- core_arthropod_sites %>% 
  mutate(num_traps = as.numeric(num_traps)) %>% 
  mutate(start_date = as.Date(start_date)) %>% 
  mutate(end_date = as.Date(end_date)) %>% 
  mutate(gps_date = as.Date(gps_date))

writeAttributes(core_arthropod_sites) # write data frame attributes to a csv in current dir to edit metadata
core_arthropod_sites_desc <- "Tabular file providing detailed characteristics of arthropod sampling locations, including a general description of the location (typically nearest cross streets), latitude and longitude, sampling start and end dates (if applicable), specifics as to the number and arrangment of pitfall traps, and general comments regarding the sampling location."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
core_arthropod_sites_DT <- createDTFF(dfname = core_arthropod_sites,
                          # factors = meter_factors,
                          description = core_arthropod_sites_desc)


# CORE_ARTHROPOD_KML

# convert tabular data to kml
library("sp")
library("rgdal")

core_arthropod_locations <- core_arthropod_sites %>%  
  filter(!is.na(lat))
  
coordinates(core_arthropod_locations) <- c("long", "lat")
proj4string(core_arthropod_locations) <- CRS("+init=epsg:4326")
# core_bird_locations <- spTransform(core_bird_locations, CRS("+proj=longlat +datum=WGS84")) 
# spTransform not required here as already in WGS 84
writeOGR(core_arthropod_locations, "core_arthropod_locations.kml", layer = "core_arthropod_locations", driver = "KML")

kml_desc <- "Geospatial file (KML) detailing the locations of CAP LTER ground-dwelling arthropod sampling. Points are approximately the center point of the set of traps at a given sampling location."
core_arthropod_locations <- createKML(kmlobject = 'core_arthropod_locations.kml',
                                      description = kml_desc)


# title and abstract ----
title <- 'Long-term monitoring of ground-dwelling arthropods in central Arizona–Phoenix, ongoing since 1998'
abstract <- "The Central Arizona–Phoenix Long-Term Ecological Research (CAP LTER) program has been monitoring ground-dwelling arthropods (e.g., insects, ararchnids) at locations throughout the greater Phoenix metropolitan area (GPMA) and surrounding Sonoran desert region since 1998. Monitoring locations span a diversity of habitat types, including mesic and xeric residential yards, commercial areas, agricultural fields, desert locations within the GPMA (desert remnant), and undisturbed desert locations. Organisms are collected quarterly using unbaited pitfall traps, typically ten per location but with some variation, exposed for approximately seventy-two hours. Organisms are identified to the lowest practical taxonomic level and enumerated. Many of the sampling locations established at the beginning of the monitoring project were relocated in 2001-2002 to overlap with the CAP LTER's Ecological Survey of Central Arizona (ESCA; formerly named Survey200) long-term monitoring sites, although within the same general landscape categories."


# people ----

nancyGrimm <- addCreator('n', 'grimm')
danChilders <- addCreator('d', 'childers')
stanFaeth <- addAssocParty('s', 'faeth', 'Former Associate of Study')
markHostetler <- addAssocParty('m', 'hostetler', 'Former Associate of Study')
nancyMcintyre <- addAssocParty('n', 'mcintyre', 'Former Associate of Study')
eyalShochat <- addAssocParty('e', 'shochat', 'Former Associate of Study')
stevanEarl <- addMetadataProvider('s', 'earl')

creators <- c(as(nancyGrimm, 'creator'),
              as(danChilders, 'creator'))

metadataProvider <-c(as(stevanEarl, 'metadataProvider'))

associatedParty <- c(as(stanFaeth, 'associatedParty'),
                     as(markHostetler, 'associatedParty'),
                     as(nancyMcintyre, 'associatedParty'),
                     as(eyalShochat, 'associatedParty'))


# keywords ----
keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("urban",
                     "arthropods",
                     "insects",
                     "invertebrates",
                     "pitfall traps",
                     "long term monitoring",
                     "agriculture",
                     "community composition")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("disturbance patterns",
                     "populations studies",
                     "land use and land cover change",
                     "adapting to city life")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("sonoran desert",
                     "residential yards")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "arid land"))
    )

# methods and coverages ----
methods <- set_methods("./pitfall_trap_methods/pitfall_trapping_protocol_modified_from_v_sept2016.md")

begindate <- "1998-04-23"
enddate <- "2016-10-15"
geographicDescription <- "CAP LTER study area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         # sci_names = c("Salix spp",
                         #               "Ambrosia deltoidea"),
                         geographicDescription = geographicDescription,
                         west = -112.577118210717, east = -111.61479515834,
                         north = +33.8204250130355, south = +33.3028279145498)


# construct the dataset ----

# address, publisher, contact, and rights come from a sourced file

# XML DISTRUBUTION
  xml_url <- new("online",
                 onlineDescription = "CAPLTER Metadata URL",
                 url = paste0("https://sustainability.asu.edu/caplter/data/data-catalog/view/", packageIdent, "/xml/"))
metadata_dist <- new("distribution",
                 online = xml_url)

# DATASET
dataset <- new("dataset",
               title = title,
               pubDate = pubDate,
               creator = creators,
               metadataProvider = metadataProvider,
               associatedParty = associatedParty,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(core_arthropods_DT,
                             core_arthropod_sites_DT),
               otherEntity = c(core_arthropod_locations))

# construct the eml ----

# ACCESS
allow_cap <- new("allow",
                 principal = "uid=CAP,o=LTER,dc=ecoinformatics,dc=org",
                 permission = "all")
allow_public <- new("allow",
                    principal = "public",
                    permission = "read")
lter_access <- new("access",
                   authSystem = "knb",
                   order = "allowFirst",
                   scope = "document",
                   allow = c(allow_cap,
                             allow_public))

eml <- new("eml",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset)

# write the xml to file ----
# write_eml(eml, "knb-lter-cap.41.12.xml")
write_eml(core_arthropods_DT, "core_arthropods.xml")
