# This code creates summary statistics
# table by node type for the α-ICON paper
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(igraph)
library(stargazer)

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

############
# Data loading

# Load the active participants snapshot of PSC (prepared by data_preparation/uk/2_psc_snapshot_to_participants_panel.r)
psc <- fread("output/uk/uk_organisations_participants_2021_long_2aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8")
setnames(psc, "equity_share", "share")
psc[, share := share/100]
psc <- psc[ company_number != participant_id & !is.na(share) ]

# Load the companies data (prepared by data_preparation/uk/1b_process_companies_data.r)
load("data/uk/uk_basic_companies_data_2021-08-01.rdata")

# Shorter industry names
uk_basic_companies_data[ industrysection_1 == "Financial and insurance activities", industrysection_1 := "Finance/Insurance"]
uk_basic_companies_data[ industrysection_1 == "Information and communication", industrysection_1 := "IT"]
uk_basic_companies_data[ industrysection_1 == "Other service activities", industrysection_1 := "Other service"]
uk_basic_companies_data[ industrysection_1 == "Professional, scientific and technical activities", industrysection_1 := "Profess./Science activ"]
uk_basic_companies_data[ industrysection_1 == "Mining and Quarrying", industrysection_1 := "Mining/Quarrying"]
uk_basic_companies_data[ industrysection_1 == "Mining and Quarrying", industrysection_1 := "Mining/Quarrying"]
uk_basic_companies_data[ industrysection_1 == "Real estate activities", industrysection_1 := "Real estate"]
uk_basic_companies_data[ industrysection_1 == "Arts, entertainment and recreation", industrysection_1 := "Arts/Entertainment"]
uk_basic_companies_data[ industrysection_1 == "Administrative and support service activities", industrysection_1 := "Admin./Support activ"]
uk_basic_companies_data[ industrysection_1 == "Accommodation and food service activities", industrysection_1 := "Hospitality/Food"]
uk_basic_companies_data[ industrysection_1 == "Human health and social work activities", industrysection_1 := "Health/Social work"]
uk_basic_companies_data[ industrysection_1 == "Wholesale and retail trade; repair of motor vehicles and", industrysection_1 := "Trade"]
uk_basic_companies_data[ industrysection_1 == "Transportation and storage", industrysection_1 := "Transportation/Storage"]
uk_basic_companies_data[ industrysection_1 == "Agriculture, Forestry and Fishing", industrysection_1 := "Agriculture"]
uk_basic_companies_data[ industrysection_1 == "Activities of households as employers; undifferentiated", industrysection_1 := "Households as employers"]
uk_basic_companies_data[ industrysection_1 == "Water supply, sewerage, waste management and", industrysection_1 := "Water/Sewerage/Waste"]
uk_basic_companies_data[ industrysection_1 == "Public administration and defence; compulsory social", industrysection_1 := "Public admin/Defence"]
uk_basic_companies_data[ industrysection_1 == "Electricity, gas, steam and air conditioning supply", industrysection_1 := "Electricity/Gas/Steam"]
uk_basic_companies_data[ industrysection_1 == "Activities of extraterritorial organisations and", industrysection_1 := "Extraterritorial orgs"]

# Company age
uk_basic_companies_data[, age := as.numeric(ymd("2021-08-01") - IncorporationDate)/31/12 ]

# Load the transitive ownership of each participant at alpha = 0.999
# (prepared by alphaicon_paper/1_compute_alphaicon.ipynb)
holders <- fread("output/uk/transitive/uk_organisations_transitive_ownership_alpha0.999_2021_long_2aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8", colClasses = c("character", "character", "numeric"))

# Load the data on graph membership
# (prepared by alphaicon_paper/1_compute_alphaicon.ipynb)
graph_membership <- fread("output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8", header = T, colClasses = c("numeric", "character", "factor"))
graph_membership[, V1 := NULL ]
setnames(graph_membership, "company_number/id", "participant_id")
gc()

# Load the data for evaluation of the algorithms (prepared by data_preparation/uk/3_prepare_affiliated_entities_evaluation_data.r)
affiliated_entities <- fread("data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8")
# Remove the CorpWatch entity identifiers
affiliated_entities[, c("subsidiary_cw_id", "parent_cw_id") := NULL ]

# Add the data on the shortest paths from parent to subsidiary 
psc_graph <- graph_from_data_frame(psc[participant_id != company_number & !is.na(share), c("participant_id", "company_number", "share"), with = F], directed = T)

# Compute all the distances on the graph
# from parent companies in the gold standard
# data to the subsidiates
affiliated_distance_mat <- distances(psc_graph, v = which(V(psc_graph)$name %in% unique(affiliated_entities$participant_id)), to = which(V(psc_graph)$name %in% affiliated_entities$company_number), mode = "out")

# Arrange them so that the distance is on the main diagonal
affiliated_distance <- as.data.table(as.table(affiliated_distance_mat))
names(affiliated_distance) <- c("participant_id", "company_number", "path_length")
# Remove non-reachable entries
affiliated_distance <- affiliated_distance[ is.finite(path_length) ]

# Add the path lengths to the affiliated entities data
affiliated_entities <- merge(affiliated_entities, affiliated_distance, by = c("company_number", "participant_id"), all.x = T, all.y = F, sort = F)
#nrow(affiliated_entities[!is.na(path_length)] # 1007

# Keep only matched orgs
affiliated_entities <- affiliated_entities[!is.na(path_length)]

############
# Graph membership data preparation

# Add the selected characteristics to the graph membership data
## Is entity or individual dummy
graph_membership[, kind := psc[ match(graph_membership$participant_id, psc$participant_id)]$kind ]
graph_membership[is.na(kind) & participant_id %in% psc$company_number, kind := "entity" ]
graph_membership <- graph_membership[ !is.na(kind) ]

graph_membership[, is_person := NA_real_ ]
graph_membership[ !is.na(kind), is_person := 0]
graph_membership[ kind == "individual", is_person := 1]

## Industry
graph_membership[, industrysection := uk_basic_companies_data[ match(graph_membership$participant_id, uk_basic_companies_data$CompanyNumber)]$industrysection_1 ]
## Add individuals to the said list
graph_membership[ is_person == 1, industrysection := "Individual"]

## Company age
graph_membership[, age := uk_basic_companies_data[ match(graph_membership$participant_id, uk_basic_companies_data$CompanyNumber)]$age ]

## Is public company
graph_membership[, companytype := uk_basic_companies_data[ match(graph_membership$participant_id, uk_basic_companies_data$CompanyNumber)]$CompanyCategory ]
graph_membership[, is_public := NA_real_ ]
graph_membership[!is.na(companytype) | is_person == 1, is_public := 0 ]
graph_membership[ companytype %in% c("Public Limited Company"), is_public := 1]

uk_basic_companies_data[, is_public := NA_real_ ]
uk_basic_companies_data[!is.na(CompanyCategory), is_public := 0 ]
uk_basic_companies_data[ CompanyCategory %in% c("Public Limited Company"), is_public := 1]

## Is in validation data
graph_membership[, in_validation := 0 ]
graph_membership[ participant_id %in% affiliated_entities$company_number | participant_id %in% affiliated_entities$participant_id, in_validation := 1 ]

uk_basic_companies_data[, in_validation := 0 ]
uk_basic_companies_data[ CompanyNumber %in% c(affiliated_entities$company_number, affiliated_entities$participant_id), in_validation := 1 ]

## In- and outdegree 
psc_graph_degree <- data.table(participant_id = V(psc_graph)$name, indegree = degree(psc_graph, mode = "in"), outdegree = degree(psc_graph, mode = "out"))
graph_membership <- merge(graph_membership, psc_graph_degree, by = "participant_id", all.x = T, all.y = F, sort = T)

uk_basic_companies_data <- merge(uk_basic_companies_data, psc_graph_degree, by.x = "CompanyNumber", by.y = "participant_id", all.x = T, all.y = F, sort = F)

############
# Compute summary stat by graph membership

## Industry shares by type in PSC graph
industry_by_graphtype <- graph_membership[, list(N = .N), by = c("type", "industrysection") ]
industry_by_graphtype[ is.na(N), N := 0 ]
industry_by_graphtype[, percent := round(100*N/sum(N), 2), by = "type"]
# Add the total
industry_by_graphtype <- rbind(industry_by_graphtype, data.table(industrysection = "Total", industry_by_graphtype[, list( N = sum(N)), by = "type"], percent = 100))

## Industry shares in the company universe
industry_universe <- uk_basic_companies_data[, list(N_universe = .N), by = c("industrysection_1") ]
setnames(industry_universe, "industrysection_1", "industrysection")
industry_universe[ is.na(N_universe), N_universe := 0 ]
industry_universe[, percent_universe := round(100*N_universe/sum(N_universe), 2)]
# Add the total
industry_universe <- rbind(industry_universe, data.table( industrysection = "Total", N_universe = sum(industry_universe$N), percent_universe = 100), fill = T)

## Industry shares in the validation data
industry_validation <- graph_membership[ in_validation == 1, list(N_validation = .N), by = c("industrysection") ]
industry_validation[ is.na(N_validation), N_validation := 0 ]
industry_validation[, percent_validation := round(100*N_validation/sum(N_validation), 2)]
# Add the total
industry_validation <- rbind(industry_validation, data.table( industrysection = "Total", N_validation = sum(industry_validation$N), percent_validation = 100), fill = T)

# Compute means of in/outdegree by type, for the entire universe, and for validation data
means_by_graphtype <- graph_membership[, lapply(.SD, mean, na.rm = T), .SDcols = c("indegree", "outdegree", "age", "is_public"), by = "type" ]
means_by_graphtype[, industrysection := "tempname" ]
means_by_graphtype[, c("indegree", "outdegree", "age") := lapply(.SD, round, 2), .SDcols = c("indegree", "outdegree", "age") ]
means_by_graphtype[, is_public := round(100*is_public, 2)]
temp <- rbind(dcast(means_by_graphtype, industrysection ~ type, value.var = "indegree"), dcast(means_by_graphtype, industrysection ~ type, value.var = "outdegree"))
temp <- rbind(temp, dcast(means_by_graphtype, industrysection ~ type, value.var = "age"))
temp <- rbind(temp, dcast(means_by_graphtype, industrysection ~ type, value.var = "is_public"))
temp$industrysection <- c("Average indegree", "Average outdegree", "Entity age", "Public limited company")
names(temp)[2:ncol(temp)] <- paste0("percent_", names(temp)[2:ncol(temp)])
means_by_graphtype <- temp

means_universe <- uk_basic_companies_data[, lapply(.SD, mean, na.rm = T), .SDcols = c("indegree", "outdegree", "age", "is_public")]
means_universe[, industrysection := "tempname" ]
means_universe[, c("indegree", "outdegree", "age") := lapply(.SD, round, 2), .SDcols = c("indegree", "outdegree", "age") ]
means_universe[, is_public := round(100*is_public, 2)]
temp <- rbind(dcast(means_universe, industrysection ~ ., value.var = "indegree"), dcast(means_universe, industrysection ~ ., value.var = "outdegree"))
temp <- rbind(temp, dcast(means_universe, industrysection ~ ., value.var = "age"))
temp <- rbind(temp, dcast(means_universe, industrysection ~ ., value.var = "is_public"))
temp$industrysection <- c("Average indegree", "Average outdegree", "Entity age", "Public limited company")
setnames(temp, ".", "percent_universe")
means_universe <- temp

means_validation <- graph_membership[ in_validation == 1, lapply(.SD, mean, na.rm = T), .SDcols = c("indegree", "outdegree", "age", "is_public")]
means_validation[, industrysection := "tempname" ]
means_validation[, c("indegree", "outdegree", "age") := lapply(.SD, round, 2), .SDcols = c("indegree", "outdegree", "age") ]
means_validation[, is_public := round(100*is_public, 2)]
temp <- rbind(dcast(means_validation, industrysection ~ ., value.var = "indegree"), dcast(means_validation, industrysection ~ ., value.var = "outdegree"))
temp <- rbind(temp, dcast(means_validation, industrysection ~ ., value.var = "age"))
temp <- rbind(temp, dcast(means_validation, industrysection ~ ., value.var = "is_public"))
temp$industrysection <- c("Average indegree", "Average outdegree", "Entity age", "Public limited company")
setnames(temp, ".", "percent_validation")
means_validation <- temp

# Long per industry data to wide summary table
summary_table <- dcast(industry_by_graphtype, industrysection ~ type, value.var = c("N", "percent"), fill = 0)
# Add the data on universe
summary_table <- merge(summary_table, industry_universe, by = "industrysection", all = T)
summary_table[ is.na(N_universe) & industrysection != "Individual", N_universe := 0]
summary_table[ is.na(percent_universe) & industrysection != "Individual", percent_universe := 0]

# Add the data on validation set
summary_table <- merge(summary_table, industry_validation, by = "industrysection", all = T)
summary_table[ is.na(N_validation) & industrysection != "Individual", N_validation := 0]
summary_table[ is.na(percent_validation) & industrysection != "Individual", percent_validation := 0]

# Proper order, so that total goes first
setorderv(summary_table, "percent_universe", -1)

# Add the data on degree and other variables to the bottom of the table
summary_table <- rbind(summary_table, means_by_graphtype, fill = T)
summary_table <- rbind(summary_table, means_universe, fill = T)
summary_table <- rbind(summary_table, means_validation, fill = T)

# Export to .tex table for future editing
#stargazer(summary_table, type = "latex", summary = F, out = "output/alphaicon_paper/summary_stat_by_type.tex", float = F, multicolumn = T, digits = 2)
