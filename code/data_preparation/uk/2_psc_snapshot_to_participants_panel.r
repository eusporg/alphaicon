# This code converts PSC data into
# a long CSV with entity-participant
# mapping that we will in the study
library(data.table)
library(lubridate)
library(stringi)
library(stringr)

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

# Load the PSC snapshot (prepared by 1a_process_psc_snapshot.r)
load("data/uk/psc_snapshot_2021-08-02.rdata")

# Load the companies data as well (prepared by 1b_process_companies_data.r)
load("data/uk/uk_basic_companies_data_2021-08-01.rdata")

# Keep only active companies and exclude data on secret participants
participants <- psc_snapshot[ is.na(ceased) & is.na(ceased_on) & company_number %in% uk_basic_companies_data[ CompanyStatus == "Active"]$CompanyNumber & kind != "super-secure-person-with-significant-control", c("company_number", "date_of_birth.month", "date_of_birth.year", "kind", "name", "name_elements.forename", "name_elements.middle_name", "name_elements.surname", "identification.country_registered", "identification.registration_number", names(psc_snapshot)[grepl("natures_of_control", names(psc_snapshot))] ), with = F ]

# Create participant name
## For individuals it is a concatenation of name and DOB
participants[kind == "individual-person-with-significant-control", participant_name := paste(toupper(name_elements.forename), toupper(name_elements.middle_name), toupper(name_elements.surname), date_of_birth.year, date_of_birth.month, sep = "$") ]
## For entities it is a concatenation of name, ID and country of incorporation
participants[kind != "individual-person-with-significant-control", participant_name := paste(toupper(name), toupper(identification.country_registered), toupper(identification.registration_number), sep = "$") ]
## Add NAs
participants[is.na(name) & is.na(name_elements.forename) & is.na(name_elements.middle_name) & is.na(name_elements.surname), participant_name := NA ]

# Participant ID is either domestic company number or individual name
## Consider company number as identifier
participants[ grepl("England|scotland|wales|^uk$|uk\\s+|United Kingdom|Great Britain|london|Cardiff|Engalnd|E\\&W|britain|Gb-Sct|N\\.Ireland|N\\. Ireland|Englan|Englad|Engand|Companies House|United Kindom|U\\.K|Gb-Nir|Unitied Kingdom|United Kingom|Endland|Great Britian|United Kigdom|Northern Ireland|Belfast|Englang|Gb-Wls|Kent|Enland|Sotland|Rngland|Englane|Scotlan|Englanf|Enagland|Staffordshire|Northen Ireland|Enagland|Englang|Englamd|Northamptonshire|Engkand|Emgland|England|United Kingtom|English|Devon|Durham|Stirlingshire|Register Of Companies|United Kingdo|Unitedd Kingdom|United Kingd|Scotlnd|Cumbria|Renfrewshire|Uunited Kingdom|Noerthern Ireland|Shropshire|Englabnd|Uk Company|United Kingdown|United King|U\\. K\\.|Ukl", identification.country_registered, perl = T, ignore.case = T) & identification.registration_number %in% unique(c(uk_basic_companies_data$CompanyNumber, psc_snapshot$company_number)), participant_id := identification.registration_number ]

## Create unique company name-company number mapping
## to restore company numbers
uk_company_name_number_mapping <- uk_basic_companies_data[, list(count_samename = .N), by = c("CompanyName") ]
uk_company_name_number_mapping <- uk_company_name_number_mapping[  count_samename == 1 ]
uk_company_name_number_mapping[, company_number := uk_basic_companies_data[ match(uk_company_name_number_mapping$CompanyName, uk_basic_companies_data$CompanyName)]$CompanyNumber ]
uk_company_name_number_mapping[, count_samename := NULL ]

## Add this to participant IDs
participants[ kind == "corporate-entity-person-with-significant-control", participant_id_namebased := uk_company_name_number_mapping[ match(toupper(participants[kind == "corporate-entity-person-with-significant-control"]$name), toupper(uk_company_name_number_mapping$CompanyName))]$company_number]

## Perform the replacement (about 95 thousand companies more now have proper IDs)
participants[ kind == "corporate-entity-person-with-significant-control" & is.na(participant_id) & !is.na(participant_id_namebased), participant_id := participant_id_namebased ]
participants[, participant_id_namebased := NULL ]

## If match on company ID fails, use the name-dob combination
participants[is.na(participant_id), participant_id := participant_name ]

# Remove irrelevant fields (except for "kind" as it is crucial in determining the participant_id)
participants[, c("date_of_birth.month", "date_of_birth.year", "name", "name_elements.forename", "name_elements.middle_name", "name_elements.surname", "identification.country_registered", "identification.registration_number", "participant_name") := NULL ]

# Remove possible duplicates
nrow(participants) # 5502058
participants <- unique(participants, by = c("company_number", "participant_id"))
nrow(participants) # 5482200

# To long data to obtain equity shares
participants_long <- melt(participants, id.vars = c("company_number", "participant_id"), value.name = "nature_of_control", na.rm = T)
participants_long[, variable := NULL ]

participants_long[, equity_share := NA_real_]
participants_long[grepl("25-to-50", nature_of_control, perl = T), equity_share := 37.5]
participants_long[grepl("50-to-75", nature_of_control, perl = T), equity_share := 62.5]
participants_long[grepl("75-to-100", nature_of_control, perl = T), equity_share := 87.5]

# Back to participants data, but now with equity share
temp <- participants_long[, list(equity_share = max(equity_share, na.rm = T)), by = c("company_number", "participant_id")]
temp[!is.finite(equity_share), equity_share := NA]
participants <- merge(participants[, c("company_number", "participant_id", "kind"), with = F], temp, by = c("company_number", "participant_id"), all = F, sort = F)

# Replace NAs with proper value of full ownership if we have one person per entity
participants[, persons_per_entity := .N, by = "company_number" ]
participants[persons_per_entity == 1 & is.na(equity_share), equity_share := 87.5 ]
participants[, persons_per_entity := NULL ]

# Shorter kinds
participants[ kind == "individual-person-with-significant-control", kind := "individual" ]
participants[ kind == "corporate-entity-person-with-significant-control", kind := "entity" ]
participants[ kind == "legal-person-person-with-significant-control", kind := "legal_person" ]

# Proper order and save point
setorderv(participants, c("company_number", "participant_id"))

# Save point
fwrite(participants, file = "output/uk/uk_organisations_participants_2021_long_2aug21.csv", eol = "\n")
