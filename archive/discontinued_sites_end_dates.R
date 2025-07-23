
# README ------------------------------------------------------------------

# Tools to set the end date of sites (that are discontinued)


# libraries ---------------------------------------------------------------

library(tidyverse)
library(RMySQL)


# connetions --------------------------------------------------------------

source('~/Documents/localSettings/mysql_prod.R')
mysql <- mysql_prod_connect('lter10_arthropods_production')


# set end date of discontinued sites --------------------------------------

# update discontinued sites based on the last collection

UPDATE lter10_arthropods_production.sites
JOIN 
(
	SELECT
	  sites.site_id,
	  sites.site_code,
	  maxdates.enddate
	FROM lter10_arthropods_production.sites
	INNER JOIN 
	(
	SELECT 
	  site_id,
	  max(sample_date) AS enddate
	FROM lter10_arthropods_production.sampling_events
	GROUP BY site_id
	) AS maxdates ON (sites.site_id = maxdates.site_id)
	WHERE 
	sites.site_code REGEXP 'W.17|ADODAM|V.16|AA.17|AC.16|M.16|P.18|Q.16|S.17|L.7|Q.7|W.6|AC.20|ALFPOW|NDV'
) AS subquery ON (sites.site_id = subquery.site_id)
SET sites.end_date = subquery.enddate;

# however sites S17, AC20, ALFPOW, and V16 had notes about collections after
# actual sampling had stopped given the impression that there was a collection
# date (per the aforementioned) after the last sample had been collected. We
# will update those sites manually per input from Sally
# (./archive/Pitfall_End.date.deleted.sites.xlsx)

UPDATE lter10_arthropods_production.sites SET sites.end_date = '2012-07-03' WHERE sites.site_code REGEXP 'S.17';
UPDATE lter10_arthropods_production.sites SET sites.end_date = '2014-11-06' WHERE sites.site_code REGEXP 'AC.20';
UPDATE lter10_arthropods_production.sites SET sites.end_date = '2011-10-13' WHERE sites.site_code REGEXP 'ALFPOW';
UPDATE lter10_arthropods_production.sites SET sites.end_date = '2016-07-29' WHERE sites.site_code REGEXP 'V.16';
