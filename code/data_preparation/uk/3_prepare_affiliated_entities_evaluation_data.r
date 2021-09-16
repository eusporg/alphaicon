# This code processes the dump of
# CorpWatch EDGAR data to create
# a CSV with entity-participant
# mapping from SEC 10-K Exhibit 21
# that we will use in the study
library(data.table)
library(lubridate)
library(stringi)
library(stringr)

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

# Load the data from http://api.corpwatch.org/ dump
# (http://api.corpwatch.org/documentation/db_dump/)

## Company information (name, id, location)
## This information includes data on parent-subsidiary relationship
company_info <- fread("data/corpwatch_api_tables_csv_14aug21/company_info.csv", encoding = "UTF-8", na.strings = c("", "NULL"))

## Company locations
company_locations <- fread("data/corpwatch_api_tables_csv_14aug21/company_locations.csv", encoding = "UTF-8", na.strings = c("", "NULL"))

## Company name-id mapping
cik_name_lookup <- fread("data/corpwatch_api_tables_csv_14aug21/cik_name_lookup.csv", encoding = "UTF-8", na.strings = c("", "NULL"))

## Add locations to the company information
company_info[, country := company_locations[ match(company_info$best_location_id, company_locations$location_id) ]$country_code ]

### Extract subsidiaries of UK companies active in 2020--2021 (whichever comes latest)
uk_subsidiary_companies_filers <- company_info[ year >= 2020 & country == "GB" & top_parent_id != cw_id, c("year", "company_name", "cw_id", "num_parents", "num_children", "top_parent_id"), with = F]
### Get the latest available year
setorderv(uk_subsidiary_companies_filers, c("cw_id", "year"), c(1, -1))
uk_subsidiary_companies_filers <- unique(uk_subsidiary_companies_filers, by = c("cw_id"))
uk_subsidiary_companies_filers[, year := NULL ]

## Parent-subsidiary ID mapping
uk_parent_subsidiary_mapping <- unique(uk_subsidiary_companies_filers[, c("cw_id", "top_parent_id", "company_name")])
setnames(uk_parent_subsidiary_mapping, c("subsidiary_cw_id", "parent_cw_id", "subsidiary_name"))

# Add namesakes
## For subsidiaries
uk_parent_subsidiary_mapping <- merge(uk_parent_subsidiary_mapping, cik_name_lookup[, c("cw_id", "edgar_name")], by.x = "subsidiary_cw_id", by.y = "cw_id", all.x = T, all.y = F)
uk_parent_subsidiary_mapping[!is.na(edgar_name) & subsidiary_name != edgar_name, subsidiary_name := edgar_name ]
uk_parent_subsidiary_mapping[, edgar_name := NULL ]
## For parents
uk_parent_subsidiary_mapping[, parent_name := company_info[year >= 2020][ match(uk_parent_subsidiary_mapping$parent_cw_id, company_info[year >= 2020]$cw_id) ]$company_name ]
uk_parent_subsidiary_mapping <- merge(uk_parent_subsidiary_mapping, cik_name_lookup[, c("cw_id", "edgar_name")], by.x = "parent_cw_id", by.y = "cw_id", all.x = T, all.y = F)
uk_parent_subsidiary_mapping[ !is.na(edgar_name) & parent_name != edgar_name, parent_name := edgar_name ]
uk_parent_subsidiary_mapping[, edgar_name := NULL ]

uk_parent_subsidiary_mapping <- unique(uk_parent_subsidiary_mapping)

# Remove self-loops
uk_parent_subsidiary_mapping <- uk_parent_subsidiary_mapping[ subsidiary_cw_id != parent_cw_id ]

uniqueN(uk_parent_subsidiary_mapping$parent_cw_id) # 1071
uniqueN(uk_parent_subsidiary_mapping$subsidiary_cw_id) # 7634

# Match the parent-subsidiart data from SEC filings with PSC register on company names

## Load the companies data prepared by 1b_process_companies_data.r
load("data/uk/uk_basic_companies_data_2021-08-01.rdata")

## Load the participants panel prepared by 2_psc_snapshot_to_participants_panel.r
participants <- fread("output/uk/uk_organisations_participants_2021_long_2aug21.csv", encoding = "UTF-8", na.strings = "")

## Keep only entities-persons with significant control
participants <- participants[ kind %in% c("entity", "legal_person") ]

## Add the names to the participants panel
participants[, company_name := uk_basic_companies_data[ match(participants$company_number, uk_basic_companies_data$CompanyNumber) ]$CompanyName ]
participants[, participant_name := uk_basic_companies_data[ match(participants$participant_id, uk_basic_companies_data$CompanyNumber) ]$CompanyName ]
participants[, participant_name_bak := stri_split_fixed(participant_id, "$", n = 2, simplify = T)[,1] ]
participants[ is.na(participant_name) & !is.na(participant_name_bak), participant_name := participant_name_bak ]
participants[, participant_name_bak := NULL ]

## Reduce the names of companies
### Name cleaner function
name_cleaner <- function(x) {

	out <- gsub("(['’\n  ]|[[:punct:]]|[[:space:]]|[[:cntrl:]])+", " ", x, perl = T)

	out <- gsub(" LIMITED", " LTD", out, perl = T, ignore.case = T)

	out <- str_trim(out)

	return(out)

}

### Remove punctuation, stopwords, strip whitespaces from company_names
participants[, company_name_reduced := name_cleaner(toupper(company_name))]
participants[, participant_name_reduced := name_cleaner(toupper(participant_name))]

uk_parent_subsidiary_mapping[, subsidiary_name_reduced := name_cleaner(toupper(subsidiary_name)) ]
uk_parent_subsidiary_mapping[, parent_name_reduced := name_cleaner(toupper(parent_name)) ]

# UK company number - name mapping excluding namesakes
uk_company_name_number_mapping <- unique(participants[, c("company_number", "company_name_reduced"), with = F])
uk_company_name_number_mapping[, count_namesakes := .N, by = "company_name_reduced" ]
uk_company_name_number_mapping <- uk_company_name_number_mapping[ count_namesakes == 1]
uk_company_name_number_mapping[, count_namesakes := NULL ]

# Participant id - participant name mapping excluding namesakes
uk_participant_name_id_mapping <- unique(participants[, c("participant_id", "participant_name_reduced"), with = F])
uk_participant_name_id_mapping[, count_namesakes := .N, by = "participant_name_reduced" ]
uk_participant_name_id_mapping <- uk_participant_name_id_mapping[ count_namesakes == 1]
uk_participant_name_id_mapping[, count_namesakes := NULL ]

# Add company numbers to UK subsidiary companies from SEC 10-K Exhibit 21
uk_parent_subsidiary_mapping[, company_number := uk_company_name_number_mapping[ match(uk_parent_subsidiary_mapping$subsidiary_name_reduced, uk_company_name_number_mapping$company_name_reduced) ]$company_number ]

# Add participant_ids to parent companies from SEC 10-K Exhibit 21
uk_parent_subsidiary_mapping[, participant_id := uk_participant_name_id_mapping[ match(uk_parent_subsidiary_mapping$parent_name_reduced, uk_participant_name_id_mapping$participant_name_reduced) ]$participant_id ]

# Final list with only matched entries, excluding
# namesakes
uk_parent_subsidiary_mapping <- unique(uk_parent_subsidiary_mapping[ !is.na(company_number) & !is.na(participant_id), c("company_number", "participant_id", "subsidiary_cw_id", "parent_cw_id") ], by = c("company_number", "participant_id"))

# Export point
fwrite(uk_parent_subsidiary_mapping, file = "data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv", eol = "\n")
