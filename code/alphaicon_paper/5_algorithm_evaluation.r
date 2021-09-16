# This code evaluates various algorithms
# for ultimate owner identification

library(data.table)
library(stringi)
library(stringr)
library(igraph)

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

############
# Load and prepare the data 

# Load the active participants snapshot of PSC (prepared by data_preparation/uk/2_psc_snapshot_to_participants_panel.r)
psc <- fread("output/uk/uk_organisations_participants_2021_long_2aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8")

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

	# Transitive ownership on equity shares
	temp1 <- fread(paste0("output/uk/transitive/uk_organisations_transitive_ownership_alpha", a, "_2021_long_2aug21.csv"), integer64 = "character", na.strings = "", encoding = "UTF-8", colClasses = c("character", "character", "numeric"))

	assign(paste0("holders_transitive_alpha", a), temp1)

	# Transitive ownership on DPI weights
	temp2 <- fread(paste0("output/uk/transitive/uk_organisations_transitive_ownership_alpha", a, "_2021_long_7sep21_dpi_10000iter.csv"), integer64 = "character", na.strings = "", encoding = "UTF-8", colClasses = c("character", "character", "numeric"))

	assign(paste0("holders_transitivedpi_alpha", a), temp2)

	rm(temp1); rm(temp2)

}

# Load the data on graph membership
# (prepared by alphaicon_paper/1_compute_alphaicon.ipynb)
graph_membership <- fread("output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8", header = T, colClasses = c("numeric", "character", "factor"))
graph_membership[, V1 := NULL ]
setnames(graph_membership, "company_number/id", "participant_id")

# Load the data for evaluation of the algorithms (prepared by data_preparation/uk/3_prepare_affiliated_entities_evaluation_data.r)
affiliated_entities <- fread("data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8")
# Remove the CorpWatch entity identifiers
affiliated_entities[, c("subsidiary_cw_id", "parent_cw_id") := NULL ]

# Add the data on the shortest paths from parent to subsidiary 
psc_graph <- graph_from_data_frame(psc[participant_id != company_number & !is.na(equity_share), c("participant_id", "company_number", "equity_share"), with = F], directed = T)

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
#affiliated_entities[, .N, by = "path_length"]
#nrow(affiliated_entities[!is.na(path_length)] # 1007

# Keep only affiliated entities reachable on the PSC graph
affiliated_entities <- affiliated_entities[ !is.na(path_length) ]

# List all holder objects that are loaded in the memory
holder_objects <- paste0("holders_", c("dpi", "npi", paste0("transitive_alpha", alphas), paste0("transitivedpi_alpha", alphas)))

############
# Subset holder objects

# Keep only the affiliated entities in the holder objects (to speed up the computation time)
for(obj in holder_objects) {

	assign(obj, get(obj)[ company_number %in% affiliated_entities$company_number | company_number %in% affiliated_entities$participant_id | participant_id %in% affiliated_entities$company_number | participant_id %in% affiliated_entities$participant_id])

	# Add the entity type for objects	
	assign(obj, merge(get(obj), graph_membership, by = "participant_id", all.x = T, all.y = F, sort = F))

}

# Add the ranks
for(obj in holder_objects) {

	# Sort by descending share within companies
	setorderv(get(obj), c("company_number", "share"), c(1, -1))

	# Add the rank
	get(obj)[, rank := 1:.N, by = "company_number"]

	# Devise another rank
	# that ranks participants within types
	get(obj)[, rankbygraphtype := 1:.N, by = c("company_number", "type")]

}

# Debug: holders_transitive_alpha0.5[company_number == "SL033942"]

# Create a single object with ranks under different algorithms
algorithms <- c("dpi", "npi", paste0("transitive_alpha", alphas), paste0("transitivedpi_alpha", alphas))

# Start with the actual participants data (for the baseline algorithm)
holders_algorithm_ranks <- psc[ company_number %in% affiliated_entities$company_number, c("company_number", "participant_id", "equity_share"), with = F]
holders_algorithm_ranks <- holders_algorithm_ranks[ !is.na(equity_share) ]

# For the baseline algorithm simply take the equity share
setorderv(holders_algorithm_ranks, c("company_number", "equity_share"), c(1, -1))
holders_algorithm_ranks[, rank_baseline := 1:.N, by = "company_number" ]
holders_algorithm_ranks[, equity_share := NULL ]

# For the remaining ranks query the holders object
for(alg in algorithms) {

	cols_to_keep <- intersect(names(get(paste0("holders_", alg))), c("company_number", "participant_id", "rank", "rankbygraphtype"))

	holders_algorithm_ranks <- merge(holders_algorithm_ranks, get(paste0("holders_", alg))[, c(cols_to_keep), with = F], by = c("company_number", "participant_id"), all.x = T, all.y = T, sort = F)
	setnames(holders_algorithm_ranks, "rank", paste0("rank_", alg)) 
	setnames(holders_algorithm_ranks, "rankbygraphtype", paste0("rankbygraphtype_", alg), skip_absent = T) 

	message(alg)

}

# Add the participant type in graph
holders_algorithm_ranks[, type := graph_membership[ match(holders_algorithm_ranks$participant_id, graph_membership$participant_id)]$type ] 

# What rank do the actual super-holders from the affiliated entities evaluation data
# take under each algorithm
holders_algorithm_ranks_affiliated <- holders_algorithm_ranks[ company_number %in% affiliated_entities$company_number]

# Add the ground truth participant id
holders_algorithm_ranks_affiliated <- merge(holders_algorithm_ranks_affiliated, affiliated_entities[, -"path_length"], by = c("company_number"), all = T, sort = F)

# NB: We have duplicates:
# holders_algorithm_ranks_affiliated[ company_number == "00906355"]

# Debug:
#holders_algorithm_ranks[ company_number == "00027883"]
#holders_algorithm_ranks_affiliated[ company_number == "00027883"]

# Add the path lengths to the data
holders_algorithm_ranks_affiliated <- merge(holders_algorithm_ranks_affiliated, affiliated_entities, by.x = c("company_number", "participant_id.y"), by.y = c("company_number", "participant_id"), sort = F, all.x = T, all.y = F)

# What rank does the truth take under each algorithm
algorithms <- unique(c("baseline", algorithms))

for(alg in algorithms) {

	holders_algorithm_ranks_affiliated[, paste0("rank_true_", alg) := NA_real_ ]

	holders_algorithm_ranks_affiliated[participant_id.x == participant_id.y, paste0("rank_true_", alg) := get(paste0("rank_", alg)) ]

	# Add the rank when we consider only super-holders
	if( !grepl("baseline", alg) ) {

		holders_algorithm_ranks_affiliated[ participant_id.x == participant_id.y & type == "SH", paste0("rankonlysuperholder_true_", alg) := get(paste0("rankbygraphtype_", alg))]

		# Restore the original value if no rank for super-holder
		holders_algorithm_ranks_affiliated[ is.na(get(paste0("rankonlysuperholder_true_", alg))) , paste0("rankonlysuperholder_true_", alg) := get(paste0("rank_true_", alg)) ]


	}

}

# Aggregate at company level
cols_of_interest <- names(holders_algorithm_ranks_affiliated)[ grepl("rank.*_true_", names(holders_algorithm_ranks_affiliated))]
cols_of_interest <- c(cols_of_interest, "path_length")

affiliated_evaluation_results <- holders_algorithm_ranks_affiliated[, lapply(.SD, min, na.rm = T), .SDcols = cols_of_interest, by = "company_number"]
affiliated_evaluation_results[, c(cols_of_interest) := lapply(.SD, function(x) ifelse(is.finite(x), x, NA)), .SDcols = cols_of_interest]

# With the above min-aggregation we resolve the issue with duplicates
# affiliated_evaluation_results[ company_number == "00906355"]

# Compute recall @ different k for algorithms
## Init an object to store the recall
algorithm_recall <- data.table()

## Init an object to store the recall at different path lengths
algorithm_recall_by_pathlength <- data.table()

path_lengths <- unique(holders_algorithm_ranks_affiliated$path_length)
path_lengths <- path_lengths[ order(path_lengths) ]
path_lengths <- path_lengths[ !is.na(path_lengths) ]

for(k in 1:10) {

	for( alg in algorithms ) {

		# How many organisations have the true participant out of all k participants
		recall <- nrow(affiliated_evaluation_results[ get(paste0("rank_true_", alg)) <= k ])/nrow(affiliated_evaluation_results)

		algorithm_recall <- rbind(algorithm_recall, data.table(algorithm = alg, k = k, superholderpriority = 0, recall = recall), fill = T)	

		# Special recall considering only super-holders
		if( !grepl("baseline", alg) ) {

			recallsh <- nrow(affiliated_evaluation_results[ get(paste0("rankonlysuperholder_true_", alg)) <= k ])/nrow(affiliated_evaluation_results)

			algorithm_recall <- rbind(algorithm_recall, data.table(algorithm = alg, k = k, superholderpriority = 1, recall = recallsh), fill = T)	

		}

		# Same for each path lengths
		for( p in path_lengths ) {

			recallpath <- nrow(affiliated_evaluation_results[ get(paste0("rank_true_", alg)) <= k & path_length == p ])/nrow(affiliated_evaluation_results[ path_length == p | is.na(path_length) ])


			algorithm_recall_by_pathlength <- rbind(algorithm_recall_by_pathlength, data.table(algorithm = alg, k = k, superholderpriority = 0, path = p, recall = recallpath), fill = T)

			if( !grepl("baseline", alg) ) {

				recallpathsh <- nrow(affiliated_evaluation_results[ get(paste0("rankonlysuperholder_true_", alg)) <= k & path_length == p ])/nrow(affiliated_evaluation_results[ path_length == p | is.na(path_length) ] )

				algorithm_recall_by_pathlength <- rbind(algorithm_recall_by_pathlength, data.table(algorithm = alg, k = k, superholderpriority = 1, path = p, recall = recallpathsh), fill = T)

			}

		}

	}

}

# Save point
fwrite(algorithm_recall, file = "output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall.csv")
fwrite(algorithm_recall_by_pathlength, file = "output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall_by_pathlength.csv")

# Manual inspection for tables
#algorithm_recall[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 ]
#algorithm_recall[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 3 ]
#algorithm_recall[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 5 ]
#algorithm_recall[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 10 ]

#algorithm_recall_by_pathlength[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 & path == 1 ]
#algorithm_recall_by_pathlength[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 & path == 2 ]
#algorithm_recall_by_pathlength[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 & path == 3 ]
#algorithm_recall_by_pathlength[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 & path == 4 ]
#algorithm_recall_by_pathlength[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 & path == 5 ]
#algorithm_recall_by_pathlength[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 & path == 10 ]
#algorithm_recall_by_pathlength[ algorithm %in% c("baseline", "dpi", "npi", "transitive_alpha0.0", "transitive_alpha0.999", "transitivedpi_alpha0.999") & k == 1 & path == 12 ]


############
# Plot the results

library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(stringi)
library(showtext)

setwd(Sys.getenv('ALPHAICON_PATH'))

# Add the font to use
font_add_google("Open Sans", "Open Sans")
showtext_auto()

# Load the files with evaluation results
algorithm_recall <- fread("output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall.csv", encoding = "UTF-8")
algorithm_recall_by_pathlength <- fread("output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall_by_pathlength.csv", encoding = "UTF-8")

for(obj in c("algorithm_recall", "algorithm_recall_by_pathlength") ) {

	# Column marking whether we use DPI weights
	get(obj)[, dpi_weights := 0]
	get(obj)[grepl("dpi", algorithm, ignore.case = T), dpi_weights := 1]
	
	# Shorter names 
	get(obj)[, algorithm := gsub("transitive.*_alpha", "α=", algorithm)]
	get(obj)[, algorithm := gsub("transitivedpi_alpha", "α=", algorithm)]
	
	# Extract alpha values from the name
	get(obj)[, alpha := as.numeric(stri_split_fixed(algorithm, "α=", n = 2, simplify = T)[,2]) ]
	
	# Add the superholder mark to corresponding algorithm names
	get(obj)[ superholderpriority == 1, algorithm := paste0(algorithm, " SH")]
	
	# Add the DPI weight mark to corresponding algorithm names
	get(obj)[ dpi_weights == 1 & grepl("α", algorithm), algorithm := paste0(algorithm, " DPI")]
	
	# Certain names to upper case
	get(obj)[, algorithm := gsub("dpi", "DPI", algorithm) ]
	get(obj)[, algorithm := gsub("npi", "NPI", algorithm) ]

}

########
# Produce the evaluation plots (overall)

# One subset for display: only the best performing α-ICON versus other algorithms
algorithm_recall_otheralgs <- algorithm_recall[ algorithm %in% c("DPI", "NPI", "α=0.999", "α=0.999 SH")]

## Other algorithms vs best and worst-performing α-ICON algorithm
recall_plot <- ggplot(aes(x = k, y = recall, group = algorithm, color = algorithm), data = algorithm_recall_otheralgs) +
				geom_line(size = 1.5, alpha = 1) +
				geom_point(size = 2) +
				scale_x_continuous(breaks = 1:10) +
  				geom_text(aes(label = algorithm, colour = algorithm, x = k-1, y = recall), data = algorithm_recall_otheralgs[k == 1 & algorithm == "DPI"], size = 8, hjust = 0.3) +
  				geom_text(aes(label = algorithm, colour = algorithm, x = k-1, y = recall), data = algorithm_recall_otheralgs[k == 1 & algorithm == "α=0.999"], size = 8, hjust = 0.7) +
  				geom_text(aes(label = algorithm, colour = algorithm, x = k-1, y = recall), data = algorithm_recall_otheralgs[k == 1 & algorithm == "NPI"], size = 8, hjust = 0.3) +
  				geom_text(aes(label = algorithm, colour = algorithm, x = k-1, y = recall), data = algorithm_recall_otheralgs[k == 1 & algorithm == "α=0.999 SH"], size = 8, hjust = 0.5, vjust = -0.6) +
  				#geom_text_repel(aes(label = algorithm, colour = algorithm, x = k-1, y = recall), data = algorithm_recall_otheralgs[k == 1], size = 8, hjust = 0.5) +
				labs(y = expression("Recall @ "~italic("k")), x = expression(italic("k"))) +
				#scale_colour_brewer(type = "qual", palette = 2) +
				scale_color_manual(name = "", values = c("#b2df8a", "#33a02c", "#a6cee3", "#1f78b4")) +
				expand_limits(x = c(-1.5, 10)) +
				theme_minimal() + theme(legend.position = "none", text = element_text(size = 18, family = "Open Sans"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(recall_plot, file = "output/alphaicon_paper/recall_plot.pdf", width = 12, height = 9, device = cairo_pdf, scale = 0.6)

# Another subset for display: only α-ICON under different alphas
algorithm_recall_onlyalphas <- algorithm_recall[ alpha > 0 & superholderpriority == 1 & dpi_weights == 0]

# Group the duplicated values of recall at alphas
alpha_group <- algorithm_recall_onlyalphas[ k == 1, list(alphagroup = .GRP), by = "recall"]
alpha_group <- merge(alpha_group, algorithm_recall_onlyalphas[ k == 1, c("alpha", "recall")], by = "recall")
alpha_group[, recall := NULL]
# Generate the label text for alpha group
alpha_group[, minalpha := min(alpha), by = "alphagroup"]
alpha_group[, maxalpha := max(alpha), by = "alphagroup"]

alpha_group[ minalpha == maxalpha, alphagrouplabel := as.character(round(alpha, 1))]
alpha_group[ minalpha != maxalpha, alphagrouplabel := as.character(paste0(round(minalpha, 1), "-", round(maxalpha, 3)))]
alpha_group[, alphagrouplabel := paste0("α=", alphagrouplabel)]

algorithm_recall_onlyalphas[, alphagroup := alpha_group[match(algorithm_recall_onlyalphas$alpha, alpha_group$alpha)]$alphagrouplabel]

# Make alpha groups unique
algorithm_recall_onlyalphas <- unique(algorithm_recall_onlyalphas, by = c("alphagroup", "k"))

## Other algorithms vs best and worst-performing transitive algorithm
recall_plot_alphas <- ggplot(aes(x = k, y = recall, group = alphagroup, color = as.factor(alpha)), data = unique(algorithm_recall_onlyalphas)) +
				geom_line(size = 1.5, alpha = 1) +
				geom_point(size = 2) +
				scale_x_continuous(breaks = 1:10) +
  				geom_text(aes(label = alphagroup, colour = as.factor(alpha), group = alphagroup, x = k-2.5, y = recall), size = 8, data = algorithm_recall_onlyalphas[k == 4 & alpha == 0.1 ], hjust = 0.3) +
  				geom_text(aes(label = alphagroup, colour = as.factor(alpha), group = alphagroup, x = k-2.5, y = recall), size = 8, data = algorithm_recall_onlyalphas[k == 2 & alpha == 0.5 ], vjust = -4, hjust = 0.1) +
  				geom_text(aes(label = alphagroup, colour = as.factor(alpha), group = alphagroup, x = k-2.5, y = recall), size = 8, data = algorithm_recall_onlyalphas[k == 3 & alpha == 0.7 ], hjust = -0.1) +
  				geom_text(aes(label = alphagroup, colour = as.factor(alpha), group = alphagroup, x = k-2.5, y = recall), size = 8, data = algorithm_recall_onlyalphas[k == 1 & alpha == 0.8 ], vjust = 1, hjust = -0.1) +
  				geom_text(aes(label = alphagroup, colour = as.factor(alpha), group = alphagroup, x = k-2.5, y = recall), size = 8, data = algorithm_recall_onlyalphas[k == 2 & alpha == 0.9 ], hjust = 0.4) +
  				#geom_text_repel(aes(label = alphagroup, colour = as.factor(alpha), group = alphagroup, x = k-2.5, y = recall), size = 8, data = algorithm_recall_onlyalphas[k == 1], hjust = 0, min.segment.length = 10) +
				scale_colour_brewer(type = "qual", palette = 2) +
				labs(y = expression("Recall @ "~italic("k")), x = expression(italic("k"))) +
				expand_limits(x = c(-2, 10)) +
				theme_minimal() + theme(legend.position = "none", text = element_text(size = 18, family = "Open Sans"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(recall_plot_alphas, file = "output/alphaicon_paper/recall_plot_alphas.pdf", width = 12, height = 9, device = cairo_pdf, scale = 0.6)

# For reporting: table with recall
algorithms_to_report <- c("baseline", "DPI", "NPI", "NPI SH", "α=0.0", "α=0.999", "α=0.999 SH", "α=0.999 SH DPI")

algorithm_recall[, recall := round(recall, 3) ]
algorithm_recall[ k == 1 & algorithm %in% algorithms_to_report, c("algorithm", "recall")]
algorithm_recall[ k == 3 & algorithm %in% algorithms_to_report, c("algorithm", "recall")]
algorithm_recall[ k == 5 & algorithm %in% algorithms_to_report, c("algorithm", "recall")]
algorithm_recall[ k == 10 & algorithm %in% algorithms_to_report, c("algorithm", "recall")]


########
# Produce the evaluation plots (by path length)

# One subset for display: only the best performing α-ICON versus other algorithms
algorithm_recall_by_pathlength_otheralgs <- algorithm_recall_by_pathlength[ algorithm %in% c("NPI", "α=0.999", "α=0.999 SH") ]

## Other algorithms vs best and worst-performing α-ICON algorithm
recall_path_plot <- ggplot(aes(x = path, y = recall, group = algorithm, color = algorithm), data = algorithm_recall_by_pathlength_otheralgs[ k == 1 ]) +
				geom_line(size = 1.5, alpha = 1) +
				geom_point(size = 2) +
				scale_x_continuous(breaks = 1:12) +
				scale_y_continuous(limits = c(0, 1)) +
				labs(y = "Recall @ 1 for parents within path", x = expression(italic("l")~", path length from subsidiary to parent")) +
				#scale_colour_brewer(type = "qual", palette = 3) +
				scale_color_manual(name = "", values = c("#b2df8a", "#33a02c", "#1f78b4")) +
				expand_limits(x = c(1, 12)) +
				theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 18, family = "Open Sans"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(recall_path_plot, file = "output/alphaicon_paper/recall_path_plot_at_1.pdf", width = 12, height = 9, device = cairo_pdf, scale = 0.6)

# For reporting: table with recall
algorithm_recall_by_pathlength[, recall := round(recall, 3) ]

algorithms_path_length_to_report <- c("baseline", "DPI", "NPI", "NPI SH", "α=0.0", "α=0.999", "α=0.999 SH", "α=0.999 SH DPI")

algorithm_recall_by_pathlength[ path == 1 & algorithm %in% algorithms_path_length_to_report & k == 1, c("algorithm", "recall") ]
algorithm_recall_by_pathlength[ path == 2 & algorithm %in% algorithms_path_length_to_report & k == 1, c("algorithm", "recall") ]
algorithm_recall_by_pathlength[ path == 3 & algorithm %in% algorithms_path_length_to_report & k == 1, c("algorithm", "recall") ]
algorithm_recall_by_pathlength[ path == 4 & algorithm %in% algorithms_path_length_to_report & k == 1, c("algorithm", "recall") ]
algorithm_recall_by_pathlength[ path == 5 & algorithm %in% algorithms_path_length_to_report & k == 1, c("algorithm", "recall") ]
algorithm_recall_by_pathlength[ path == 10 & algorithm %in% algorithms_path_length_to_report & k == 1, c("algorithm", "recall") ]
algorithm_recall_by_pathlength[ path == 12 & algorithm %in% algorithms_path_length_to_report & k == 1, c("algorithm", "recall") ]
