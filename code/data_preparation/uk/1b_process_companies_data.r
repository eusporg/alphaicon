# This code reads in the CSV with data
# on live UK companies and saves the data
# to a cleaned .rdata object
library(data.table)
library(lubridate)
library(stringi)
library(stringr)

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

# Read in the CSV with Free Company Data Product snapshot
# from http://download.companieshouse.gov.uk/en_output.html
uk_basic_companies_data <- fread("data/uk/BasicCompanyDataAsOneFile-2021-08-01.csv", encoding = "UTF-8", na.strings = "")

# Remove certain columns
cols_to_remove <- names(uk_basic_companies_data)[ grepl("PreviousName|Mortgages|ConfStmt", names(uk_basic_companies_data))]
uk_basic_companies_data[, c(cols_to_remove) := NULL ]

# Empty to NA
for(j in seq_along(uk_basic_companies_data)) {
	set(uk_basic_companies_data, i = which(uk_basic_companies_data[[j]] == ""), j=j, value = NA)
}

# Convert dates
cols_to_date <- names(uk_basic_companies_data)[ grepl("Date", names(uk_basic_companies_data))]

uk_basic_companies_data[, c(cols_to_date) := lapply(.SD, dmy), .SDcols = cols_to_date ]

# Vars to factor
cols_to_factor <- c("RegAddress.Country", "CompanyCategory", "CompanyStatus", "CountryOfOrigin", "Accounts.AccountCategory")

uk_basic_companies_data[, c(cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor ]

# Split into industry code and industry name
uk_basic_companies_data[, SICCode.SicText_1 := as.character(SICCode.SicText_1) ]
uk_basic_companies_data[, SICCode.SicText_2 := as.character(SICCode.SicText_2) ]
uk_basic_companies_data[, SICCode.SicText_3 := as.character(SICCode.SicText_3) ]
uk_basic_companies_data[, SICCode.SicText_4 := as.character(SICCode.SicText_4) ]

uk_basic_companies_data[, c("industrycode_1", "industryname_1") := as.data.table(stri_split_fixed(SICCode.SicText_1, " - ", n = 2, simplify = T))]
uk_basic_companies_data[, c("industrycode_2", "industryname_2") := as.data.table(stri_split_fixed(SICCode.SicText_2, " - ", n = 2, simplify = T))]
uk_basic_companies_data[, c("industrycode_3", "industryname_3") := as.data.table(stri_split_fixed(SICCode.SicText_3, " - ", n = 2, simplify = T))]
uk_basic_companies_data[, c("industrycode_4", "industryname_4") := as.data.table(stri_split_fixed(SICCode.SicText_4, " - ", n = 2, simplify = T))]

# Fix empty entries
uk_basic_companies_data[ nchar(industryname_1) < 2, industrycode_1 := NA_character_]
uk_basic_companies_data[ nchar(industryname_1) < 2, industryname_1 := NA_character_]

uk_basic_companies_data[ nchar(industryname_2) < 2, industrycode_2 := NA_character_]
uk_basic_companies_data[ nchar(industryname_2) < 2, industryname_2 := NA_character_]

uk_basic_companies_data[ nchar(industryname_3) < 2, industrycode_3 := NA_character_]
uk_basic_companies_data[ nchar(industryname_3) < 2, industryname_3 := NA_character_]

uk_basic_companies_data[ nchar(industryname_4) < 2, industrycode_4 := NA_character_]
uk_basic_companies_data[ nchar(industryname_4) < 2, industryname_4 := NA_character_]

uk_basic_companies_data[, c("SICCode.SicText_1", "SICCode.SicText_2", "SICCode.SicText_3", "SICCode.SicText_4") := NULL ]

# Load SIC 2007 numeric coding of industries
sic_codes <- fread("data/uk/sic_2007_code_list.csv", encoding = "UTF-8", na.strings = "", colClasses = "character")

# Add leading zeros to industry codes
sic_codes[ nchar(sic_code) == 4, sic_code := paste0("0", sic_code) ]

# Debug
#uk_basic_companies_data[ industryname_1 %in% sic_codes[ substr(sic_code, 1, 1) == "0" ]$sic_description & substr(industrycode_1, 1, 1) != "0" ]
#uk_basic_companies_data[ industryname_2 %in% sic_codes[ substr(sic_code, 1, 1) == "0" ]$sic_description & substr(industrycode_2, 1, 1) != "0" ]

# Add industry section
uk_basic_companies_data[, industrysection_1 := sic_codes[ match(substr(uk_basic_companies_data$industrycode_1, 1, 2), substr(sic_codes$sic_code, 1, 2)) ]$section_description ]
uk_basic_companies_data[ industrycode_1 == "99999", industrysection_1 := NA ]

uk_basic_companies_data[, industrysection_2 := sic_codes[ match(substr(uk_basic_companies_data$industrycode_2, 1, 2), substr(sic_codes$sic_code, 1, 2)) ]$section_description ]
uk_basic_companies_data[ industrycode_2 == "99999", industrysection_2 := NA ]

uk_basic_companies_data[, industrysection_3 := sic_codes[ match(substr(uk_basic_companies_data$industrycode_3, 1, 2), substr(sic_codes$sic_code, 1, 2)) ]$section_description ]
uk_basic_companies_data[ industrycode_3 == "99999", industrysection_3 := NA ]

uk_basic_companies_data[, industrysection_4 := sic_codes[ match(substr(uk_basic_companies_data$industrycode_4, 1, 2), substr(sic_codes$sic_code, 1, 2)) ]$section_description ]
uk_basic_companies_data[ industrycode_4 == "99999", industrysection_4 := NA ]

# Factor coercion for industry codes
industry_names <- unique(c(uk_basic_companies_data$industryname_1, uk_basic_companies_data$industryname_2, uk_basic_companies_data$industryname_3, uk_basic_companies_data$industryname_4))
industry_names <- industry_names[!is.na(industry_names)]
industry_names <- industry_names[order(industry_names)]

industry_sections <- unique(c(uk_basic_companies_data$industrysection_1, uk_basic_companies_data$industrysection_2, uk_basic_companies_data$industrysection_3, uk_basic_companies_data$industrysection_4))
industry_sections <- industry_sections[!is.na(industry_sections)]
industry_sections <- industry_sections[order(industry_sections)]

uk_basic_companies_data[, industryname_1 := factor(industryname_1, levels = industry_names) ]
uk_basic_companies_data[, industryname_2 := factor(industryname_2, levels = industry_names) ]
uk_basic_companies_data[, industryname_3 := factor(industryname_3, levels = industry_names) ]
uk_basic_companies_data[, industryname_4 := factor(industryname_4, levels = industry_names) ]

uk_basic_companies_data[, industrysection_1 := factor(industrysection_1, levels = industry_sections) ]
uk_basic_companies_data[, industrysection_2 := factor(industrysection_2, levels = industry_sections) ]
uk_basic_companies_data[, industrysection_3 := factor(industrysection_3, levels = industry_sections) ]
uk_basic_companies_data[, industrysection_4 := factor(industrysection_4, levels = industry_sections) ]

# Save point
save(uk_basic_companies_data, file = "data/uk/uk_basic_companies_data_2021-08-01.rdata", compress = "gzip")
