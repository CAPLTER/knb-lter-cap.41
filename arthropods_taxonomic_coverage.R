
# pass vector of sci names to set_taxonomicCoverage with the optional elements
# expand (set to TRUE) and a database to use. Note that set_taxonomicCoverage in
# the package did not seem to work, and I had to copy the code from the repo and
# overwrite the function (and embedded functions) in the current session. Note
# that you will get an error for any taxonomic names that are not identified so
# be sure to to a pre-cleaning step. set_taxonomicCoverage(binomial, expand = T,
# db = 'itis')

# libraries
library(taxize)

# setTaxonomicCoverage() ----
set_taxonomicCoverage <- function(sci_names, expand=FALSE, db = 'itis') {
  # Expand using taxize and ITIS if the user passes in just scientific names
  if (is(sci_names, "character") && expand) {
    if (!requireNamespace("taxize", quietly = TRUE)) {
      stop(call. = FALSE,
           "Expansion of scientific names requires the 'taxize' package to be installed. Install taxize or set expand to FALSE.")
    }
    
    classifications <- taxize::classification(sci_names, db = db)
    
    # Remove any NAs and warn for each
    if (any(is.na(classifications))) {
      warning(call. = FALSE,
              paste0("Some scientific names were not found in the taxonomic database and were not expanded: ", paste0(sci_names[which(is.na(classifications))], collapse = ","), "."))
    }
    
    # Turn result into a list of named lists where names are the rank name and
    # values are the rank value
    sci_names <- mapply(function(cls, sci_name) {
      # If the species name isn't found in the database, NA is returned
      if (is.list(cls)) {
        x <- as.list(cls[["name"]])
        names(x) <- cls[["rank"]]
        x
      } else {
        x <- list(list("species" = as.character(sci_name)))
        names(x) <- sci_name
        x
      }
    }, classifications, names(classifications), SIMPLIFY = FALSE)
  }
  
  if (class(sci_names) == "character") {
    taxa <- lapply(sci_names, function(sci_name) {
      s <- strsplit(sci_name, " ")[[1]]
      new(
        "taxonomicClassification",
        taxonRankName = "genus",
        taxonRankValue = s[[1]],
        taxonomicClassification = c(
          new(
            "taxonomicClassification",
            taxonRankName = "species",
            taxonRankValue = sci_name
          )
        )
      )
    })
    new("taxonomicCoverage",
        taxonomicClassification = do.call(c, taxa))
  } else if (class(sci_names) == "data.frame") {
    taxon_classification <- colnames(sci_names)
    new <- as.data.frame(t(sci_names))
    colnames(new) <- NULL
    taxa <- lapply(new, function(sci_name) {
      tc <- lapply(taxon_classification, function(name) {
        new(
          "taxonomicClassification",
          taxonRankName = name,
          taxonRankValue = as.character(sci_name[name])
        )
      })
      tc <- formRecursiveTree(tc)[[1]]
    })
    new("taxonomicCoverage",
        taxonomicClassification = do.call(c, taxa))
  } else if (class(sci_names) == "list") {
    # Warn if not a list of lists
    if (!all(vapply(sci_names, class, "") == "list")) {
      message("sci_names should be a list of lists. Your input was automatically wrapped up in a list.")
      sci_names <- list(sci_names)
    }
    
    taxa <- lapply(sci_names, function(sci_name) {
      taxonRankNames <- as.list(names(sci_name))
      
      taxa <- lapply(taxonRankNames, function(name) {
        new(
          "taxonomicClassification",
          taxonRankName = as.character(name),
          taxonRankValue = as.character(sci_name[[name]])
        )
      })
      formRecursiveTree(taxa)
    })
    
    new("taxonomicCoverage",
        taxonomicClassification = new("ListOftaxonomicClassification", do.call(c, taxa)))
  } else {
    stop("Incorrect format: sci_names can only be character string, data.frame or list")
  }
}

# helper function: form a nested tree recursively
formRecursiveTree <- function(listOfelements) {
  if (length(listOfelements) == 1 ||
      length(listOfelements) == 2 && is.null(listOfelements[[2]])) {
    return(do.call(c, listOfelements[1]))
  } else if (is.null(listOfelements[[1]])) {
    formRecursiveTree(listOfelements[2:length(listOfelements)])
  } else {
    listOfelements[[1]]@taxonomicClassification <- formRecursiveTree(listOfelements[2:length(listOfelements)])
    return(do.call(c, listOfelements[1]))
  }
}

# get arthropod taxa from core_arthropods object
arthropod_taxa <- core_arthropods %>% 
  select(display_name) %>%
  filter(!is.na(display_name)) %>% 
  mutate(
    display_name = gsub("\\(winged\\)", "", display_name),
    display_name = gsub("\\(adult\\)", "", display_name),
    display_name = gsub("\\(immature\\)", "", display_name),
    display_name = gsub("\\(nymph\\)", "", display_name),
    display_name = gsub("\\(larvae\\)", "", display_name),
    display_name = gsub("\\(imm.\\)", "", display_name),
    display_name = gsub("\\(sp.\\)", "", display_name),
    display_name = trimws(display_name, which = c("both"))
  ) %>% 
  distinct(display_name) %>% 
  pull(display_name)


# identify those taxa for which we can resolve a tsn
identify_resolvable_taxa <- function(taxa_list) {
  
  resolved_taxa <- data.frame(taxon = NA, resolve = NA)
  
  for (i in 1:length(taxa_list)){
    info <- suppressWarnings(get_tsn(searchterm = taxa_list[i],
                                     searchtype = "scientific",
                                     accepted = T,
                                     ask = F))
    resolved_taxa[i,"taxon"] <- taxa_list[i]
    resolved_taxa[i,"resolve"] <- info[1]
    
  }
  
  return(resolved_taxa)
  
}


# run list of taxa through function to identify those taxa for which we can
# resolve a tsn
all_taxa <- identify_resolvable_taxa(arthropod_taxa)


# filter out those taxa for which we could resolve a tsn; of course this means
# that we are not generating a coverage that covers all the taxa but a complete
# coverage would require expert input on all taxa
resolved_taxa <- c(all_taxa[!is.na(all_taxa$resolve),]$taxon)


# set the tax coverage based on those taxa for which we could resolve a tsn. The
# idea here is that because we were able to resolve a tsn, that all of those
# should work in the set_taxonomicCoverage function but in fact I wound up
# having to do a lot of manual, interactive selection so not sure the issue.
taxaCoverage <- set_taxonomicCoverage(resolved_taxa, expand = T, db = 'itis')

# set taxonomicCoverage element
coverage@taxonomicCoverage <- c(taxaCoverage)
