# This code sums the computed control shares
# of the super-holders in the PSC data and
# produces the rankings under different algorithms
# for the α-ICON paper
library(data.table)
library(lubridate)
library(stringi)
library(stringr)
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

# Load the companies data (prepared by data_preparation/uk/1b_process_companies_data.r)
load("data/uk/uk_basic_companies_data_2021-08-01.rdata")

# Load the DPI of each participant after 10000 iterations (computed by alphaicon_paper/2_compute_npi_dpi.r)
holders_dpi <- fread("output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_dpi_10000iter.csv", integer64 = "character", na.strings = "", encoding = "UTF-8")
setnames(holders_dpi, c("entity", "participant", "dpi"), c("company_number", "participant_id", "share"))
holders_dpi[, times_pivotal := NULL ]

# Load the NPI of each participant after 10000 iterations (computed by alphaicon_paper/2_compute_npi_dpi.r)
holders_npi <- fread("output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_npi_10000iter.csv", integer64 = "character", na.strings = "", encoding = "UTF-8")
setnames(holders_npi, c("entity", "participant", "npi"), c("company_number", "participant_id", "share"))
holders_npi[, times_pivotal := NULL ]

# Load the transitive ownership data at various alpha
# (prepared by alphaicon_paper/1_compute_alphaicon.ipynb)
alphas <- c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "0.999")

for( a in alphas) {

	temp <- fread(paste0("output/uk/transitive/uk_organisations_transitive_ownership_alpha", a, "_2021_long_2aug21.csv"), integer64 = "character", na.strings = "", encoding = "UTF-8", colClasses = c("character", "character", "numeric"))

	assign(paste0("holders_transitive_alpha", a), temp)

	rm(temp)

}

# Load the data on graph membership
# (prepared by alphaicon_paper/1_compute_alphaicon.ipynb)
graph_membership <- fread("output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8", header = T, colClasses = c("numeric", "character", "factor"))
graph_membership[, V1 := NULL ]
setnames(graph_membership, "company_number/id", "participant_id")

# All holder objects that are loaded in the memory
holder_objects <- paste0("holders_", c("dpi", "npi", paste0("transitive_alpha", alphas)))

# Add the graph membership and remove self-loops
for(obj in holder_objects) {

	get(obj)[, participant_type := graph_membership[ match(get(obj)$participant_id, graph_membership$participant_id) ]$type ]
	get(obj)[, company_type := graph_membership[ match(get(obj)$company_number, graph_membership$participant_id) ]$type ]

	assign(obj, get(obj)[ company_number != participant_id ])

}

psc[, participant_type := graph_membership[ match(psc$participant_id, graph_membership$participant_id) ]$type ]
psc[, company_type := graph_membership[ match(psc$company_number, graph_membership$participant_id) ]$type ]


############
# Identify top holders

# Compute total holdings (weighted and unweighted by assets)
holdings_dpi <- holders_dpi[, list( sumdpi = sum(share)), by = "participant_id"]
holdings_dpi[, rankdpi := frankv(sumdpi, order = -1, ties.method = "first")]

holdings_npi <- holders_npi[, list( sumnpi = sum(share)), by = "participant_id"]
holdings_npi[, ranknpi := frankv(sumnpi, order = -1, ties.method = "first")]

# Normalize shares by type to one for our transitive data
holders_transitive_alpha0.999[, share_typenormalized := share/sum(share), by = c("company_number", "participant_type")]

# Keep only super-holders and sum up their shares
holdings_transitive <- holders_transitive_alpha0.999[participant_type == "SH", list( sumtransitive = sum(share_typenormalized)), by = "participant_id"]

# Rank the holders by their share
holdings_transitive[, ranktransitive := frankv(sumtransitive, order = -1, ties.method = "first")]

# Top-100 participants by our metric compared to other metrics
top_holders <- setorderv(holdings_transitive, "ranktransitive", 1)[1:100]

# Add ranks from other algorithms
top_holders <- merge(top_holders, holdings_dpi, by = "participant_id", all.x = T, all.y = F, sort = F)
top_holders <- merge(top_holders, holdings_npi, by = "participant_id", all.x = T, all.y = F, sort = F)

# Proper names
## First, match on company number with companies data
top_holders[, participant_name := uk_basic_companies_data[ match(top_holders$participant_id, uk_basic_companies_data$CompanyNumber)]$CompanyName ]
## Second, extract the company name
top_holders[ str_count(participant_id, fixed("$")) == 2, participant_name := stri_split_fixed(participant_id, "$", n = 3, simplify = T)[,1] ]
## Third, add the first and last names of the persons
top_holders[ str_count(participant_id, fixed("$")) == 4, participant_name := paste0(stri_split_fixed(participant_id, "$", n = 4, simplify = T)[, 1], " ", stri_split_fixed(participant_id, "$", n = 4, simplify = T)[, 3]) ]

## We no longer need participant id
#top_holders[, participant_id := NULL ]

# Proper column order
setcolorder(top_holders, c("participant_name", "participant_id", "ranktransitive", "ranknpi", "rankdpi", "sumtransitive", "sumnpi", "sumdpi"))

# Round the score
top_holders[, c("sumtransitive", "sumnpi", "sumdpi") := lapply(.SD, round, 1), .SDcols = c("sumtransitive", "sumnpi", "sumdpi") ]

# Add the size of the network of holdings and the core-shell distribution
top_holders_network_chars <- data.table()

for(superholder_id in top_holders$participant_id) {

	# Debug: superholder_id <- "SC095000"

	# Ultimate companies held by this organization
	holders_subset_ultimate <- holders_transitive_alpha0.999[ participant_id %in% superholder_id ]
	
	# Consider their total interest in all entities
	holders_subset <- psc[ participant_id %in% unique(holders_subset_ultimate$company_number) | company_number %in% unique(holders_subset_ultimate$company_number) ]
	holders_subset <- holders_subset[ !is.na(equity_share)]
	holders_subset <- holders_subset[ participant_id != company_number]

	# Share to 0-1 range
	setnames(holders_subset, "equity_share", "share")
	holders_subset[, share := share/100]

	## Create an object with vertex metadata
	vertex_metadata <- unique(rbind(unique(data.table( name = holders_subset$company_number, type = holders_subset$company_type)),
								data.table( name = holders_subset$participant_id, type = holders_subset$participant_type)
							), by = "name")
	
	# Count all vertex types
	out <- vertex_metadata[, .N, by = "type"]
	# Transpose
	out <- dcast(melt(out, id.vars = "type"), variable ~ type)
	out[, variable := NULL ]
	
	# Keep only the columns of interest in the order of interest
	types_of_interest <- c("SH", "ST", "I", "C")
	names_outside_types_of_interest <- types_of_interest[!(types_of_interest %in% names(out))]
	if( length(names_outside_types_of_interest) > 0) {
		out[, c(names_outside_types_of_interest) := 0]
	}
	out <- out[, types_of_interest, with = F]
	
	# Add the network size
	out <- data.table(participant_id = superholder_id, nodes = nrow(vertex_metadata), out)

	top_holders_network_chars <- rbind(top_holders_network_chars, out, fill = T)

	message(superholder_id)

}

# Add to the top holders data
top_holders <- merge(top_holders, top_holders_network_chars, by = "participant_id", sort = F)

# Write to CSV
fwrite(top_holders, file = "output/alphaicon_paper/uk_organisations_top100_holders_2021_long_2aug21.csv")

# Produce a TeX table
#stargazer(top_holders[1:25, -"participant_id"], type = "latex", summary = F, out = "output/alphaicon_paper/uk_organisations_top25_holders_2021_long_2aug21.tex", float = F, multicolumn = T, digits = 1)

# Export the information on top-25 entities with the largest deviation between ranks
combined_holdings <- merge(holdings_dpi, holdings_npi, by = "participant_id", all = T)
combined_holdings <- merge(combined_holdings, holdings_transitive, by = "participant_id", all = T)

combined_holdings[, diff_npi_dpi := sumnpi - sumdpi ]
combined_holdings[, diff_transitive_dpi := sumtransitive - sumdpi ]
combined_holdings[, diff_transitive_npi := sumtransitive - sumnpi ]

top100_holders_diff_npi_dpi <- setorderv(combined_holdings, "diff_npi_dpi", -1, na.last = T)[1:100]
top100_holders_diff_transitive_dpi <- setorderv(combined_holdings, "diff_transitive_dpi", -1, na.last = T)[1:100]
top100_holders_diff_transitive_npi <- setorderv(combined_holdings, "diff_transitive_npi", -1, na.last = T)[1:100]

# Export point
#fwrite(top100_holders_diff_npi_dpi, file = "output/alphaicon_paper/uk_organisations_top100_holders_diff_npi_dpi_2021_long_2aug21.csv")
#fwrite(top100_holders_diff_transitive_dpi, file = "output/alphaicon_paper/uk_organisations_top100_holders_diff_transitive_dpi_2021_long_2aug21.csv")
#fwrite(top100_holders_diff_transitive_npi, file = "output/alphaicon_paper/uk_organisations_top100_holders_diff_transitive_npi_2021_long_2aug21.csv")

############
# Agreement between rankings

# Add the ranks (takes time)
for(obj in holder_objects) {

	# Create temporary variable prioritising super-holders
	if( grepl("transitive", obj) )  {

		get(obj)[, superholder := 0]
		get(obj)[participant_type == "SH", superholder := 1]

	}
	# Sort by descending share within companies
	if( grepl("transitive", obj) )  {

		setorderv(get(obj), c("company_number", "superholder", "share"), c(1, -1, -1))

	} else {

		setorderv(get(obj), c("company_number", "share"), c(1, -1))

	}

	# Add the rank that prioritises super-holders
	get(obj)[, rank := 1:.N, by = c("company_number")]

	message(obj)

}

# Combine the ranks from all the methods
vars_to_keep <- c("company_number", "participant_id", "rank")

## DPI and NPI
holders_rankings <- merge(holders_dpi[, c(vars_to_keep), with = F], holders_npi[, c(vars_to_keep), with = F], by = c("company_number", "participant_id"), all = T)
setnames(holders_rankings, c("rank.x", "rank.y"), c("rank_dpi", "rank_npi"))

## Add transitive holder hankings (only super-holders or top-ranking non-super-holders)
for( a in alphas) {

	holders_rankings <- merge(holders_rankings, get(paste0("holders_transitive_alpha", a))[ participant_type == "SH" | (participant_type != "SH" & rank == 1), c(vars_to_keep), with = F], by = c("company_number", "participant_id"), all = T)
	setnames(holders_rankings, c("rank"), paste0("rank_transitive", a))

}

# Count holders by company from each method
holders_rankings[, countholders_dpi := sum(!is.na(rank_dpi)), by = "company_number"]

holders_rankings[, countholders_npi := sum(!is.na(rank_npi)), by = "company_number"]

for( a in alphas) {

	holders_rankings[, paste0("countholders_transitive", a) := sum(!is.na(get(paste0("rank_transitive", a)))), by = "company_number"]

	message(a)

}

# Pairs of counts of holders by company
holders_rankings[, countholders_dpi_npi := sum(!is.na(rank_dpi) & !is.na(rank_npi)), by = "company_number"]

for( a in alphas) {

	holders_rankings[, paste0("countholders_dpi_transitive", a) := sum(!is.na(rank_dpi) & !is.na(get(paste0("rank_transitive", a)))), by = "company_number"]
	
	holders_rankings[, paste0("countholders_npi_transitive", a) := sum(!is.na(rank_npi) & !is.na(get(paste0("rank_transitive", a)))), by = "company_number"]

	message(a)

}

## Compute company-level Kendal's tau-b rank correlation coefficient
### (takes about 15 minutes per pair)
tau_dpi_npi <- holders_rankings[ countholders_dpi_npi > 1 & !is.na(rank_dpi) & !is.na(rank_npi), list(kendalltau_dpi_npi = cor(rank_dpi, rank_npi, method = "kendall", use = "pairwise.complete.obs")), by = "company_number"]

for( a in alphas) {

	eval(parse(text = paste0("tau_dpi_transitive", a, " <- holders_rankings[ countholders_dpi_transitive", a, " > 1 & !is.na(rank_dpi) & !is.na(rank_transitive", a, "), list(kendalltau_dpi_transitive", a, " = cor(rank_dpi, rank_transitive", a, ", method = 'kendall', use = 'pairwise.complete.obs')), by = 'company_number']")))

	eval(parse(text = paste0("tau_npi_transitive", a, " <- holders_rankings[ countholders_npi_transitive", a, " > 1 & !is.na(rank_npi) & !is.na(rank_transitive", a, "), list(kendalltau_npi_transitive", a, " = cor(rank_npi, rank_transitive", a, ", method = 'kendall', use = 'pairwise.complete.obs')), by = 'company_number']")))

	message(a)

}

# Examples of companies with complex rankings
#holders_rankings[ company_number == "00026306"]
#holders_rankings[ company_number == "00000529"]
#holders_rankings[ company_number == "00018751"]

# Debug: these should be zeros
#holders_rankings[ countholders_dpi == 1 & countholders_npi == 1 & rank_dpi != rank_npi]
#holders_rankings[ countholders_transitive0.999 == 1 & countholders_npi == 1 & rank_transitive0.999 != rank_npi]

# Add the tau-b = 1 for companies with one participant (both direct and indirect)
tau_dpi_npi <- rbind(tau_dpi_npi, data.table(company_number = unique(holders_rankings[ countholders_dpi == 1 & countholders_npi == 1 & !is.na(rank_dpi) & !is.na(rank_npi) ]$company_number), kendalltau_dpi_npi = 1))

for( a in alphas) {

	eval(parse(text = paste0("tau_dpi_transitive", a, " <- rbind(tau_dpi_transitive", a, ", data.table( company_number = unique(holders_rankings[ countholders_dpi == 1 & countholders_transitive", a, " == 1 & !is.na(rank_dpi) & !is.na(rank_transitive", a, ")]$company_number), kendalltau_dpi_transitive", a, " = 1))")))

	eval(parse(text = paste0("tau_npi_transitive", a, " <- rbind(tau_npi_transitive", a, ", data.table( company_number = unique(holders_rankings[ countholders_npi == 1 & countholders_transitive", a, " == 1 & !is.na(rank_npi) & !is.na(rank_transitive", a, ")]$company_number), kendalltau_npi_transitive", a, " = 1))")))

}

# Add the tau-b = -1 for companies with differing number of participants
# where ranks do not match. Example: holders_rankings[ company_number == "00026306"]
tau_dpi_npi <- rbind(tau_dpi_npi, data.table(company_number = unique(holders_rankings[ !(company_number %in% tau_dpi_npi$company_number)]$company_number), kendalltau_dpi_npi = -1))

for( a in alphas) {

	eval(parse(text = paste0("tau_dpi_transitive", a, " <- rbind(tau_dpi_transitive", a, ", data.table( company_number = unique(holders_rankings[ !(company_number %in% tau_dpi_transitive", a, "$company_number) ]$company_number), kendalltau_dpi_transitive", a, " = -1))")))

	eval(parse(text = paste0("tau_npi_transitive", a, " <- rbind(tau_npi_transitive", a, ", data.table( company_number = unique(holders_rankings[ !(company_number %in% tau_npi_transitive", a, "$company_number) ]$company_number), kendalltau_npi_transitive", a, " = -1))")))


}

# Finally, we need to fill in NAs like tau_dpi_transitive0.999[company_number == "00001615"]
# holders_rankings[ company_number == "00001615" ]
# holders_rankings[ company_number == "SO307326" ]
# holders_rankings[ company_number == "00000529" ]

# Merge the taus
for( a in alphas) {

	if( which(alphas == a) == 1 ) {

		kendall_taus_participant_ranks <- merge(tau_dpi_npi, get(paste0("tau_dpi_transitive", a)), by = "company_number", all = T)
		kendall_taus_participant_ranks <- merge(kendall_taus_participant_ranks, get(paste0("tau_npi_transitive", a)), by = "company_number", all = T)


	} else {

		kendall_taus_participant_ranks <- merge(kendall_taus_participant_ranks, get(paste0("tau_dpi_transitive", a)), by = "company_number", all = T)
		kendall_taus_participant_ranks <- merge(kendall_taus_participant_ranks, get(paste0("tau_npi_transitive", a)), by = "company_number", all = T)

	}

	message(a)
}

# Add the information on the count of holders
kendall_taus_participant_ranks <- merge(kendall_taus_participant_ranks, unique(holders_rankings[, c("company_number", "countholders_dpi", "countholders_npi", paste0("countholders_transitive", alphas)), with = F], by = "company_number"), by = "company_number", all.x = T, all.y = F)

# Save point
fwrite(kendall_taus_participant_ranks, file = "output/alphaicon_paper/kendall_taus_participant_ranks_dpi_npi_transitive_uk_organisations_participants_2021_7sep21.csv")

############
# Analyse the agreement between rankings
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(stringi)
library(stringr)
library(stargazer)
library(showtext)

setwd(Sys.getenv('ALPHAICON_PATH'))

# Add the font to use
font_add_google("Open Sans", "Open Sans")
showtext_auto()

# Load the produced CSV with Kendall's tau-b
taus_longs <- fread("output/alphaicon_paper/kendall_taus_participant_ranks_dpi_npi_transitive_uk_organisations_participants_2021_7sep21.csv", encoding = "UTF-8")

# To long form (each line is method and alpha)
taus <- melt(taus_longs, id.vars = "company_number", measure.vars = names(taus_longs)[grepl("kendalltau", names(taus_longs))], variable.name = "pair", value.name = "tau", na.rm = T, variable.factor = T)

countholders <- melt(taus_longs, id.vars = "company_number", measure.vars = names(taus_longs)[grepl("countholders", names(taus_longs))], variable.name = "algorithm", value.name = "countholders", na.rm = T, variable.factor = T)

# Convert pairs to extract useful information
alg_pairs <- data.table(pair = levels(taus$pair))
alg_pairs[, pair_modif := gsub("kendalltau_", "", pair, fixed = T)]
alg_pairs[, c("algorithm1", "algorithm2") := as.data.table(stri_split_fixed(pair_modif, "_", n = 2, simplify = T))]
alg_pairs[, pair_modif := NULL]
alg_pairs[, pair := factor(pair, levels = levels(taus$pair))]
alg_pairs[, alpha := NA_real_ ]
alg_pairs[ grepl("transitive", algorithm2), alpha := as.numeric(gsub("transitive", "", algorithm2))]
alg_pairs[ grepl("transitive", algorithm2), algorithm2 := gsub("[^transitive]", "", algorithm2)]
alg_pairs[, algorithm1 := factor(algorithm1, levels = c("dpi", "npi", "transitive"))]
alg_pairs[, algorithm2 := factor(algorithm2, levels = c("dpi", "npi", "transitive"))]

taus <- merge(taus, alg_pairs, by = "pair")
taus[, pair := NULL]
gc()

# Same for countholders
alg_counts <- data.table(algorithm = levels(countholders$algorithm))
alg_counts[, alg_modif := gsub("countholders_", "", algorithm)]
alg_counts[ grepl("transitive", alg_modif), alpha := as.numeric(gsub("transitive", "", alg_modif))]
alg_counts[ grepl("transitive", alg_modif), alg_modif := gsub("[^transitive]", "", alg_modif)]
alg_counts[, alg_modif := factor(alg_modif, levels = c("dpi", "npi", "transitive"))]

countholders <- merge(countholders, alg_counts, by = "algorithm")
countholders[, algorithm := NULL]
setnames(countholders, "alg_modif", "algorithm")
gc()

# Add count holders to tau
#taus <- merge(taus, countholders, by.x = c("algorithm1", "alpha"), by.y = c("algorithm", "alpha"))
#setnames(taus, "countholders", "countholders1")

#taus <- merge(taus, countholders, by.x = c("algorithm2", "alpha"), by.y = c("algorithm", "alpha"))
#setnames(taus, "countholders", "countholders2")

# Add entity industry to the data (peak use of 40 GB)
taus[, industrysection := uk_basic_companies_data[ match(taus$company_number, uk_basic_companies_data$CompanyNumber)]$industrysection_1 ]
gc()

# Summary statistics of Kendall's tau-b

## Mean, overall
taus_overall_mean <- taus[!is.na(tau), list(tau = mean(tau), tausemean = sd(tau)/sqrt(.N)), by = c("algorithm1", "algorithm2", "alpha")]

## Mean, by industry
taus_industry_mean <- taus[!is.na(tau) & !is.na(industrysection), list( tau = mean(tau), tausemean = sd(tau)/sqrt(.N)), by = c("algorithm1", "algorithm2", "alpha", "industrysection")]

# Export point
#fwrite(taus_overall_mean, file = "output/alphaicon_paper/mean_kendall_taus_participant_ranks_dpi_npi_transitive_uk_organisations_participants_2021_7sep21.csv")
#fwrite(taus_industry_mean, file = "output/alphaicon_paper/mean_by_industrysection_kendall_taus_participant_ranks_dpi_npi_transitive_uk_organisations_participants_2021_7sep21.csv")

# Produce the plot with overall mean
taus_overall_mean[ grepl("dpi|npi", algorithm1), algorithm1 := toupper(algorithm1)]

## Other algorithms vs best and worst-performing transitive algorithm
taus_mean_plot <- ggplot(aes(x = alpha, y = tau, group = algorithm1, color = algorithm1), data = taus_overall_mean[ algorithm1 == "NPI" & algorithm2 == "transitive"]) +
				geom_line(size = 1.5, alpha = 1) +
				geom_point(size = 2) +
				geom_text(aes(label = round(tau, 4)), size = 2.5, nudge_y = 0.0005) +
				scale_x_continuous( breaks = c(seq(0, 0.9, 0.1), 0.999), labels = c(seq(0, 0.9, 0.1), 0.999), guide = guide_axis(n.dodge = 1)) +
				#facet_wrap(. ~ algorithm1, scales = "free_y")+
				labs(y = expression("Kendall's "~tau~"-b betw. NPI\nand "~alpha~"-ICON SH ranks"), x = expression(alpha)) +
				scale_colour_brewer(type = "qual", palette = 2) +
				theme_minimal() + theme(legend.position = "none", text = element_text(size = 14, family = "Open Sans"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(taus_mean_plot, file = "output/alphaicon_paper/taus_mean_plot.pdf", width = 8.5, height = 4, device = cairo_pdf, scale = 1)

# Export to a table
#stargazer(taus_overall_mean[ algorithm1 == "NPI" & algorithm2 == "transitive"], type = "latex", summary = F, out = "output/alphaicon_paper/taus_overall_mean.tex", float = F, multicolumn = T, digits = 5)