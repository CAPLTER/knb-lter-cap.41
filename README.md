# knb-lter-cap.41

## dataset publishing: core arthropod monitoring

### knb-lter-cap.41.17 2025-06-xx

- reflects the first data refresh since the database, taxonomy, and data-entry
[application](https://gitlab.com/caplter/arthropods-shiny) were revised
- documented schema design issue (see reference
[here](https://github.com/CAPLTER/knb-lter-cap.643/blob/knb-lter-cap.643.5/sampling_events_traps_site_id_conflict.md)
in knb-lter-cap.643)

### knb-lter-cap.41.16 2020-03-04

- template was updated to use newer capeml functions
- Rmd is still not sourceable as I had to manually add roles for associated
parties (need to fix in gioseml) - not tested this version
- Greatly simplified the methods to remove lists and formatting (revise when
markdown support is available) - not changed
- Taxonomy not yet addressed with annotations
- added sampling event id
- updated discontinued sites end dates
- removed reference to cap count from sites since this is not tracked through
time and thus could be misleading

### knb-lter-cap.41.15 2019-12-13

Version was primarily a data refresh. Notes:
- template was updated to use newer capeml functions
- Rmd is still not sourceable as I had to manually add roles for associated
parties (need to fix in gioseml)
- Greatly simplified the methods to remove lists and formatting (revise when
markdown support is available)
- Taxonomy not yet addressed with annotations

### knb-lter-cap.41.14

Merely updated data, and transitioning to Rmd from R.

### knb-lter-cap.41.13

When addressing the McDowell arthropods for the first time, shortly after this
version 12 was published, it became apparant that not including the trap count
was a glaring ommission that, though leaving it out was not technically
incorrect, could compromise the interpretation of these data. The McDowell
arthropods data has a complicated SQL statement that has a critical LEFT join
of trap_trap_sampling_events onto the specimen data (taxa, counts, etc.). That
worked fine for McDowell with ~5K results, but was overwhelming for the core
data with >100K results. As a workaround, I created a View
(lter10_arthropods_production.specimens_data) to include with the Core data
query that makes the load more manageable. Will likely have to go this route
for the McDowell data as well if that project continues. The only change from
version 12 to this version 13 is the updated query that includes the trap count
data. As a result, only the XML corresponding to the core_arthropods_DT was
generated and that was simply copied and pasted into the bulk of the version 12
XML file.

### knb-lter-cap.41.12

The first update to these data using REML and the first update addressed by SRE
(version 11 by D. Julian). Additions include a sites table to pull out and
feature additional information about the sampling locations that were not
included in earlier publications - a trend to normalization but, hopefully, an
appropriate one.

Specifically regarding the spatial data, unlike birds and herpetofauna,
arthropod sampling locations will not move but rather will come on- and
off-line. As such, a dedicated table/resource to track the movement of sites is
not required. However, the position of sites and the timing of their existence
throughout the project is required, and those details can simply be added to
the sites table. As with other programs, spatial information can be stored in
the database and pulled as needed rather than managing separate geospatial
files. This workflow draws on spatial information and some details about the
start and end dates of select sites from the PO10_AllSites.shp shapefile (see
arthropods_locationDetails_database.R in this directory for the workflow). To
sum, the lat/long data in the sites table draws on the best available spatial
information, any gis files can generally be dismissed, and the locations of new
sites can simply be added to the sites table, which is now the authoritative
source of information for the location of sampling sites.

Eyal Schohat, Mark Hostetler, Nancy McIntyre, and Stan Faeth added as
associated parties.
