
# README ----

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

# functions and working dir ----
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createKMLFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createdataTableFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createOtherEntityFn.R')

# DB connections ----
con <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='',
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
packageIdent <- 'knb-lter-cap.41.12'
pubDate <- '2017-01-27'

# data entity ----

# CORE_ARTHROPODS

core_arthropods <- dbGetQuery(con, "
SELECT  
  s.site_code,
  se.sample_date,
  people.observer,
  t.trap_name,
  tse.comments AS trap_sampling_events_comments,
  tax.arth_Class,
  tax.arth_order,
  tax.arth_family,
  tax.arth_genus_subgenus,
  tax.display_name,
  ts.lt2mm,
  ts._2_5mm,
  ts._5_10mm,
  ts.gt10mm,
  ts.unsized
FROM lter10_arthropods_production.trap_specimens ts
JOIN lter10_arthropods_production.arthropod_taxonomies tax ON (tax.arthropod_taxon_id = ts.arthropod_taxon_id)
JOIN lter10_arthropods_production.trap_sampling_events tse ON (tse.trap_sampling_event_id = ts.trap_sampling_event_id)
JOIN lter10_arthropods_production.sampling_events se ON (se.sampling_event_id = tse.sampling_event_id)
JOIN lter10_arthropods_production.people ON (people.person_id = ts.person_id) # se.default_person_for_trap_samples not required
JOIN lter10_arthropods_production.sites s ON (s.site_id = se.site_id)
JOIN lter10_arthropods_production.traps t ON (tse.trap_id = t.trap_id)
ORDER BY sample_date, site_code, trap_name, arth_class, arth_order, arth_family, arth_genus_subgenus, display_name, unsized
LIMIT 1000000;")

core_arthropods[core_arthropods == ''] <- NA

core_arthropods <- core_arthropods %>% 
  mutate(sample_date = as.Date(sample_date)) %>% 
  mutate(lt2mm = as.numeric(lt2mm)) %>% 
  mutate(`_2_5mm` = as.numeric(`_2_5mm`)) %>% 
  mutate(`_5_10mm` = as.numeric(`_5_10mm`)) %>% 
  mutate(gt10mm = as.numeric(gt10mm)) %>% 
  mutate(unsized = as.numeric(unsized))

writeAttributes(core_arthropods) # write data frame attributes to a csv in current dir to edit metadata
core_arthropods_desc <- "Denormalized tabular file detailing key data from the CAP LTER's long-term monitoring of ground-dwelling arthropods. This data entity includes sample event details, taxonomic details and the number of collected organisms, sizes of organisms where noted in the early stages of the project, and general notes and details regarding the gathering of specimen data."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
core_arthropods_DT <- createDTFF(dfname = core_arthropods,
                                 # factors = meter_factors,
                                 description = core_arthropods_desc)

# CORE_ARTHROPOD_SITES 




# address factors if needed
reach <- c(Tonto = "Salt River, Tonto National Forest, near Usery Road",
           Priest = "Salt River flood channel, east of Priest Drive and west of Tempe Town Lake dam",
           Price = "Salt River, by Price Drain, northeast of the loop 101 and loop 202 intersection",
           Rio = "Salt River at Rio Salado; Central Ave, north of Broadway Rd",
           Ave35 = "Salt River at 35th Ave north of Broadway Rd",
           Ave67 = "Salt River at 67th Ave north of Southern Ave",
           BM = "Baseline and Meridian Wildlife Area; Salt River at 115th Ave northeast of Phoenix International Raceway")
urbanized <- c(urban = "in urban area",
               NonUrban = "outside urban area")
restored <- c(Restored = "site received active restoration",
              NotRestored = "site has not been restored")

meter_factors <- rbind(
  data.frame(
    attributeName = "reach",
    code = names(reach),
    definition = unname(reach)
  ),
  data.frame(
    attributeName = "urbanized",
    code = names(urbanized),
    definition = unname(urbanized)
  )
)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
dataframe_DT <- createDTFF(dfname = dataframe,
                          # factors = meter_factors,
                          description = dataframe_desc)


# title and abstract ----
title <- 'Tempe Town Lake water-quality monitoring, ongoing since 2005'
abstract <- 'Constructed in 1997, the Tempe Town Lake is a small man-made reservoir that transforms a section of the typically-dry Salt River bed into a 224-acre lake in the heart of Tempe, Arizona. To accommodate the river when it flows, the lake features hydraulically-operated steel gates that allow water to pass through the system unimpeded. The lake has been a remarkable success as a community amenity and as a driver of economic growth in the area around the lake. The lake provides an ideal model system for the many artificial lakes constructed in arid-land cities owing to management decisions, such as draining, that affect their operation and ecology. At the same time, dramatic shifts in hydrology and chemistry when the lake is transformed to a flowing river and back into a lake during and after floods, provide opportunities to study the system’s dynamic evolution to new limnological steady states. The CAP LTER has been measuring water quality, including temperature, pH, conductivity, and dissolved oxygen, dissolved organic carbon (DOC), and total dissolved nitrogen (TDN), in the lake since 2005.'


# people ----

ASU <- "Arizona State University"

heh_name <- new('individualName',
                givenName = 'Hilairy',
                surName = 'Hartnett')

heh_orcid <- new('userId',
                 'http://orcid.org/0000-0003-0736-7844',
                 directory = 'orcid.org')

hilairyHartnett <- new('creator',
                       individualName = heh_name,
                       organizationName = ASU,
                       electronicMailAddress = "h.hartnett@asu.edu",
                       userId = heh_orcid)

nbg_name <- new('individualName',
                givenName = "Nancy",
                surName = "Grimm")

nbg_orcid <- new("userId",
                 "http://orcid.org/0000-0001-9374-660X",
                 directory = "orcid.org")

nancyGrimm  <- new('creator',
                   individualName = nbg_name,
                   organizationName = ASU,
                   electronicMailAddress = "nbgrimm@asu.edu",
                   userId = nbg_orcid)

creators <- c(as(hilairyHartnett, 'creator'),
              as(nancyGrimm, 'creator'))

hilairyHartnett <- new('metadataProvider',
                       individualName = heh_name,
                       organizationName = ASU,
                       electronicMailAddress = "h.hartnett@asu.edu",
                       userId = heh_orcid)

metadataProvider <-c(as(hilairyHartnett, 'metadataProvider'))


# keywords ----
keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("urban",
                     "dissolved organic carbon",
                     "total dissolved nitrogen")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("disturbance patterns",
                     "movement of inorganic matter")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("unlisted stuff",
                     "unlisted stuff")),
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
methods <- set_methods("ref a file")

begindate <- "2005-11-05"
enddate <- "2015-12-15"
geographicDescription <- "CAP LTER study area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         sci_names = c("Salix spp",
                                       "Ambrosia deltoidea"),
                         geographicDescription = geographicDescription,
                         west = -111.949, east = -111.910,
                         north = +33.437, south = +33.430)

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
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(first_DT,
                             second_DT))

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

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

custom_units <- rbind(
  data.frame(id = "microsiemenPerCentimeter",
             unitType = "conductance",
             parentSI = "siemen",
             multiplierToSI = 0.000001,
             description = "electric conductance of lake water in the units of microsiemenPerCentimeter"),
data.frame(id = "nephelometricTurbidityUnit",
           unitType = "unknown",
           parentSI = "unknown",
           multiplierToSI = 1,
           description = "(NTU) ratio of the amount of light transmitted straight through a water sample with the amount scattered at an angle of 90 degrees to one side"))
unitList <- set_unitList(custom_units)

eml <- new("eml",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))

# write the xml to file ----
write_eml(eml, "out.xml")
