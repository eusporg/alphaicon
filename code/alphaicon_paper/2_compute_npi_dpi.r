# This code computes Direct Power Index and
# Network Power Index Mizuno, Doi, and Kurizaki (2020, p. 7)
# (https://doi.org/10.1371/journal.pone.0237862)
# for the UK entities in PSC data
library(data.table)

# NB: there seems to be a bug in multi-threaded data.table group
# processing in Windows: https://github.com/Rdatatable/data.table/issues/4294
setDTthreads(1)

set.seed(42)

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

# Load the function to compute power index
source("code/helper_functions/compute_power_index.r")

# Load British entities data
uk_entities <- fread("output/uk/uk_organisations_participants_2021_long_2aug21.csv", colClasses = c("character", "character", "factor", "numeric"), encoding = "UTF-8", na.strings = "")
uk_entities[, kind := NULL ]
gc()

# Remove self-loops and missing equity shares
uk_entities <- uk_entities[ participant_id != company_number & !is.na(equity_share) ]

# Normalize equity to one
uk_entities[, equity_share := 100*equity_share/sum(equity_share), by = "company_number" ]

# Run DPI computation on 10000 iterations
tic <- Sys.time()
uk_entities_dpi_10000iter <- compute_power_index(uk_entities,
					participant_var = "participant_id",
					entity_var = "company_number",
					weight_var = "equity_share",
					iterations = 10000,
					powerindex = "dpi",
					quota = 50,
					save_labels = FALSE
				)
toc <- Sys.time()
toc - tic # Takes 2.353837 days on Intel Xeon E5-2620
fwrite(uk_entities_dpi_10000iter, file = paste0("output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_dpi_10000iter.csv"), eol = "\n")

# Run NPI computation on 10000 iterations
tic <- Sys.time()
uk_entities_npi_10000iter <- compute_power_index(uk_entities,
					participant_var = "participant_id",
					entity_var = "company_number",
					weight_var = "equity_share",
					iterations = 10000,
					powerindex = "npi",
					quota = 50,
					epsilon = 0.01,
					iterations_to_discard = 1000,
					save_labels = FALSE
				)
toc <- Sys.time()
toc - tic # Takes 3.755192 days on Intel Xeon E5-2620
fwrite(uk_entities_npi_10000iter, file = paste0("output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_npi_10000iter.csv"), eol = "\n")
