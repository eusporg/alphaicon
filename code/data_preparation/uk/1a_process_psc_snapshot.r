# This code reads in the JSONL with daily
# Persons with Significant Control snapshot
# and saves the data on control in long format
# to a cleaned .rdata object
library(data.table)
library(ndjson)
library(lubridate)
library(stringi)
library(stringr)

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

# Stream in the JSONL with daily Persons with Significant Control
# information from http://download.companieshouse.gov.uk/en_pscdata.html
# (takes about 2 hours to load)
psc_snapshot <- ndjson::stream_in("data/uk/persons-with-significant-control-snapshot-2021-08-02.txt", cls = "dt")

# Unify the names
names(psc_snapshot) <- gsub("^data\\.", "",  names(psc_snapshot), perl = T)

# Coerce variables to relevant types
## To dates
to_ymd_dates <- unique(c("ceased_on", "notified_on", names(psc_snapshot)[grepl("exempt_from|exempt_to", names(psc_snapshot))]))

psc_snapshot[, c(to_ymd_dates) := lapply(.SD, ymd), .SDcols = to_ymd_dates ]

# Remove last row with overall statistics
psc_snapshot[, c("exemptions_count", "generated_at", "persons_of_significant_control_count", "statements_count") := NULL ]
psc_snapshot <- psc_snapshot[!is.na(company_number)]

# Remove exemptions
psc_snapshot <- psc_snapshot[ kind != "exemptions"]
psc_snapshot[, names(psc_snapshot)[grepl("exempt", names(psc_snapshot))] := NULL ]

# Remove statements
psc_snapshot <- psc_snapshot[ kind != "persons-with-significant-control-statement" ]
psc_snapshot[, statement := NULL ]

# Remove variables with all NAs
na_count <- sapply(psc_snapshot, function(y) sum(length(which(is.na(y)))))
empty_vars <- names(na_count[ which( nrow(psc_snapshot) - na_count == 0) ])
psc_snapshot[, c(empty_vars) := NULL ]

# Kind to factor
psc_snapshot[, kind := as.factor(kind)]

# Save point
save(psc_snapshot, file = "data/uk/psc_snapshot_2021-08-02.rdata", compress = "gzip")
