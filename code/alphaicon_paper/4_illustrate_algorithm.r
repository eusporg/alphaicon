# This code plots the selected sub-networks 
# in the PSC data and produces the figures
# for the α-ICON paper
library(data.table)
library(igraph)
library(stringi)
library(stringr)
library(ggplot2)
library(ggnetwork)
library(ggthemes)
library(showtext)

# Add the font to use
font_add_google("Open Sans", "Open Sans")
showtext_auto()

# Declare working directory beforehand in an environment variable
# ALPHAICON_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('ALPHAICON_PATH'))

# Load the active participants snapshot of PSC (prepared by data_preparation/uk/2_psc_snapshot_to_participants_panel.r)
psc <- fread("output/uk/uk_organisations_participants_2021_long_2aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8")

# Load the companies data (prepared by data_preparation/uk/1b_process_companies_data.r)
load("data/uk/uk_basic_companies_data_2021-08-01.rdata")

# Load the transitive ownership of each participant at alpha = 0.999
# (prepared by alphaicon_paper/1_compute_alphaicon.ipynb)
holders <- fread("output/uk/transitive/uk_organisations_transitive_ownership_alpha0.999_2021_long_2aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8", colClasses = c("character", "character", "numeric"))

# Load the data on graph membership
# (prepared by alphaicon_paper/1_compute_alphaicon.ipynb)
graph_membership <- fread("output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv", integer64 = "character", na.strings = "", encoding = "UTF-8", header = T, colClasses = c("numeric", "character", "factor"))
graph_membership[, V1 := NULL ]
setnames(graph_membership, "company_number/id", "participant_id")
gc()

# We need to pick up an illustrative organization

# Ideally this organization should have a small network size
# and have at least one entity in graph core

# Orgs in the core with all other types in the subgraph
# and manageable network size
temp <- holders[ participant_id %in% graph_membership[ type == "C"]$participant_id ]
temp[, company_type :=  graph_membership[ match(temp$company_number, graph_membership$participant_id) ]$type  ]
temp[, participant_type :=  graph_membership[ match(temp$participant_id, graph_membership$participant_id) ]$type  ]

# Devise a set of candidate orgs for illustration
# where all organization types are present in the company network
candidate_orgs <- temp[, list( company_types = uniqueN(company_type), network_size = uniqueN(company_number)), by = "participant_id"]

# Consider moderately sized networks with all types of orgs there
candidate_ids <- unique(candidate_orgs[ company_types > 2 & network_size <= 30 & network_size > 10]$participant_id)

# Additionally, consider the largest structure in terms
# of graph path from participant to company
# This is the result of igraph::farthest_vertices
# applied to the psc graph
farthest_components <- c("11443992")

# Also consider participant SEAN DINNEN
# he has DPI and NPI of 0 but very large transitivity score
sean_dinnen <- "SEAN$MICHAEL$DINNEN$1968$3"

# Example of long path
heidelbergcement <- "HEIDELBERGCEMENT AG$GERMANY$HRB 33082"

# Finally, consider the organizations with large difference between
# the super-holder scores (computed in alphaicon_paper/6_rank_top_holders.r)
top100_holders_diff_npi_dpi <- fread("output/alphaicon_paper/uk_organisations_top100_holders_diff_npi_dpi_2021_long_2aug21.csv", encoding = "UTF-8")
top100_holders_diff_transitive_dpi <- fread("output/alphaicon_paper/uk_organisations_top100_holders_diff_transitive_dpi_2021_long_2aug21.csv", encoding = "UTF-8")
top100_holders_diff_transitive_npi <- fread("output/alphaicon_paper/uk_organisations_top100_holders_diff_transitive_npi_2021_long_2aug21.csv", encoding = "UTF-8")

# Define the candidates to consider (from affiliated_entities
# and other inspection)
organizations_of_interest <- unique(c("IHS MARKIT LTD$BERMUDA$48610", "06647317", "11594795", candidate_ids, farthest_components, sean_dinnen))

# Or specify one organization for the graph in the paper:
#organizations_of_interest <- "03782947" # OPUS

organizations_of_interest <- c(top100_holders_diff_transitive_dpi[1]$participant_id, # SPECSAVERS OPTICAL SUPERSTORES LTD
								top100_holders_diff_npi_dpi[82]$participant_id, # LENDLEASE INTERNATIONAL PTY LIMITED
								top100_holders_diff_transitive_dpi[99]$participant_id, # THE BERKELEY GROUP PLC
								top100_holders_diff_transitive_npi[17]$participant_id) # BAJLINDER$KAUR$BOPARAN$1968$1. NB: her husband RANJIT$SINGH$BOPARAN$1966$8 has alpha-ICON sum 73

# Iterate over multiple graph candidates
for( org_interest in organizations_of_interest ) {
	
	# Ultimate companies held by this organization
	holders_subset_ultimate <- holders[ participant_id %in% org_interest ]
	
	# Consider their total interest in all entities
	holders_subset <- psc[ participant_id %in% unique(holders_subset_ultimate$company_number) | company_number %in% unique(holders_subset_ultimate$company_number) ]
	holders_subset <- holders_subset[ !is.na(equity_share)]
	setnames(holders_subset, "equity_share", "share")

	# Network size
	#uniqueN(holders_subset$company_number)
	#uniqueN(holders_subset$participant_id)
	
	# Add company and participant names and countries of origin
	holders_subset[, company_name := uk_basic_companies_data[ match(holders_subset$company_number, uk_basic_companies_data$CompanyNumber)]$CompanyName ]
	#holders_subset[, company_country := uk_basic_companies_data[ match(holders_subset$company_number, uk_basic_companies_data$CompanyNumber)]$CountryOfOrigin ]
	
	holders_subset[, participant_name := uk_basic_companies_data[ match(holders_subset$participant_id, uk_basic_companies_data$CompanyNumber)]$CompanyName ]
	#holders_subset[, participant_country := uk_basic_companies_data[ match(holders_subset$participant_id, uk_basic_companies_data$CompanyNumber)]$CountryOfOrigin ]
	
	# Proper name handling for individuals: first name-surname
	holders_subset[grepl("$", participant_id, fixed = T) & kind != "individual", participant_name := as.data.table(stri_split_fixed(participant_id, "$", n = 3, simplify = T)[,1])]
	holders_subset[kind == "individual", participant_name := paste0(stri_split_fixed(participant_id, "$", n = 4, simplify = T)[, 1], " ", stri_split_fixed(participant_id, "$", n = 4, simplify = T)[, 3])]
	
	#holders_subset[, company_country := gsub("United Kingdom", "UK", company_country) ]
	#holders_subset[, participant_country := gsub("United Kingdom", "UK", participant_country) ]
	
	# Are the names unique?
	#uniqueN(holders_subset$company_name)
	#uniqueN(holders_subset$participant_name)
	
	# Add the graph types of organizations
	holders_subset[, company_type := graph_membership[ match(holders_subset$company_number, graph_membership$participant_id) ]$type ]
	holders_subset[, participant_type := graph_membership[ match(holders_subset$participant_id, graph_membership$participant_id) ]$type ]
	holders_subset[, share_range := NA_character_ ]
	holders_subset[ share == 87.5, share_range := "75-100%"]
	holders_subset[ share == 62.5, share_range := "50-75%"]
	holders_subset[ share == 37.5, share_range := "25-50%"]

	## Total number of holdings
	holdings_count <- holders_subset[, list(count_holdings_total = .N), by = "participant_name"]

	## Total number of holders
	holders_count <- holders_subset[, list(count_holders_total = .N), by = "company_name"]

	# To igraph object
	## Create an object with vertex metadata
	vertex_metadata <- unique(rbind(unique(data.table( name = holders_subset$company_name, type = holders_subset$company_type)),
								data.table( name = holders_subset$participant_name, type = holders_subset$participant_type)
							), by = "name")
	vertex_metadata <- merge(vertex_metadata, holdings_count, by.x = "name", by.y = "participant_name", all.x = T, all.y = F, sort = F)
	vertex_metadata <- merge(vertex_metadata, holders_count, by.x = "name", by.y = "company_name", all.x = T, all.y = F, sort = F)
	vertex_metadata[is.na(count_holdings_total), count_holdings_total := 0]
	vertex_metadata[is.na(count_holders_total), count_holders_total := 0]
	# NA to mock 0
	vertex_metadata[count_holdings_total == 0, count_holdings_total := 1]
	# Remove intermediaries with no holders
	vertex_metadata <- vertex_metadata[ !(count_holders_total == 0 & type == "I") ]

	## Convert to igraph
	holders_example_graph <- graph_from_data_frame(holders_subset[participant_name %in% vertex_metadata$name & company_name %in% vertex_metadata$name, c("participant_name", "company_name", "share", "share_range")], directed = T, vertices = vertex_metadata)
	
	# Line-wrap the name
	V(holders_example_graph)$name_label <- str_wrap(V(holders_example_graph)$name, 15)

	# Graph to ggnetwork
	holders_example_net <- ggnetwork(holders_example_graph, layout = with_kk(), arrow.gap = 0.01)

	# Empty name for certain types if network is large	
	if(nrow(holders_example_net) > 150) {

		holders_example_net[!(holders_example_net$type %in% c("SH", "C") | holders_example_net$name %in% c("SPECSAVERS UK HOLDINGS LIMITED", "HANSON PACKED PRODUCTS LIMITED", "HOUSERATE LIMITED", "HANSON LIMITED", "HEIDELBERGCEMENT UK HOLDING LIMITED") ), "name_label"] <- ""

	}

	# Plot the graph
	holders_example_plot <- ggplot(holders_example_net, aes(x = x, y = y, xend = xend, yend = yend)) +
		geom_edges(color = "#ebecf0", arrow = arrow(angle = 45, length = unit(5, "pt"), type = "closed"), show.legend = F) +
		geom_nodes(aes(color = type, size = log(count_holdings_total)), show.legend = T) +
		{ if( org_interest %in% c("03782947", "PHILIP$AULD$MACTAGGART$1956$2", "BAJLINDER$KAUR$BOPARAN$1968$1")) geom_edgetext(aes(label = share_range), color = "grey25", size = 1.5) } +
		geom_nodetext_repel(aes(label = name_label, color = type), fontface = "bold", size = 2, show.legend = F, point.padding = 5, max.overlaps = 300) +
		#{ if(nrow(holders_example_net) > 150) geom_nodetext_repel(aes(color = type), label = "", fontface = "bold", size = 2, show.legend = F, point.padding = 5) } + 
		#{ if(nrow(holders_example_net) <= 150) geom_nodetext_repel(aes(label = name, color = type), fontface = "bold", size = 2, show.legend = F, point.padding = 5) } + 
		scale_size(guide = "none") +
		scale_color_manual(name = "node type", labels = c("super-holder", "super-target", "intermediary", "core"), values = c("SH" = "#66c2a5", "ST" = "#8da0cb", "I" = "grey", "C" = "#fc8d62")) +
		guides(color = guide_legend(override.aes = list(size = 5))) + 
		theme_blank() + theme(text = element_text(family = "Open Sans"), legend.position = "bottom", plot.margin = unit(c(0, 0, 0, 0), "cm"))
	
	# Export to an image
	graph_file_name <- gsub("[[:punct:]]", "", stri_split_fixed(org_interest, "$", simplify = T)[1,1])

	if( org_interest %in% c("SEAN$MICHAEL$DINNEN$1968$3", "SPECSAVERS OPTICAL SUPERSTORES LTD$UNITED KINGDOM$1721624") ) {

		ggsave(paste0("output/alphaicon_paper/network_examples/uk_example ", graph_file_name, ".pdf"), height = 20, width = 20, scale = 1.2, device = cairo_pdf)

	} else {

		ggsave(paste0("output/alphaicon_paper/network_examples/uk_example ", graph_file_name, ".pdf"), height = 10, width = 10, device = cairo_pdf)

	}

	message(graph_file_name)

}

########
# Plot the graph with the core components only
core <- psc[ participant_id %in% unique(graph_membership[type == "C"]$participant_id) & company_number %in% unique(graph_membership[type == "C"]$participant_id) ]
core <- core[ !is.na(equity_share)]
setnames(core, "equity_share", "share")

# Add company and participant names and countries of origin
core[, company_name := uk_basic_companies_data[ match(core$company_number, uk_basic_companies_data$CompanyNumber)]$CompanyName ]
core[, participant_name := uk_basic_companies_data[ match(core$participant_id, uk_basic_companies_data$CompanyNumber)]$CompanyName ]

# Proper name handling for individuals: first name-surname
core[grepl("$", participant_id, fixed = T) & kind != "individual", participant_name := as.data.table(stri_split_fixed(participant_id, "$", n = 3, simplify = T)[,1])]
core[kind == "individual", participant_name := paste0(stri_split_fixed(participant_id, "$", n = 4, simplify = T)[, 1], " ", stri_split_fixed(participant_id, "$", n = 4, simplify = T)[, 3])]

# Add the graph types of organizations
core[, company_type := graph_membership[ match(core$company_number, graph_membership$participant_id) ]$type ]
core[, participant_type := graph_membership[ match(core$participant_id, graph_membership$participant_id) ]$type ]
core[, share_range := NA_character_ ]
core[ share == 87.5, share_range := "75-100%"]
core[ share == 62.5, share_range := "50-75%"]
core[ share == 37.5, share_range := "25-50%"]

# Add the total number of companies where core participants
## Total number of holdings
core_holdings <- holders[participant_id %in% graph_membership[type == "C"]$participant_id, list(count_holdings_total = .N), by = "participant_id"]
## Holdings in core
core_holdings <- merge(core_holdings, holders[participant_id %in% graph_membership[type == "C"]$participant_id & company_number %in% graph_membership[type == "C"]$participant_id, list(count_holdings_core = .N), by = "participant_id"], by = "participant_id", all = T)

# To igraph object
## Create an object with vertex metadata
vertex_metadata <- unique(rbind(unique(data.table(name = core$company_name, type = core$company_type, id = core$company_number)),
							data.table( name = core$participant_name, type = core$participant_type, id = core$participant_id)
						), by = "name")

# Add the counts of holdings
vertex_metadata <- merge(vertex_metadata, core_holdings, by.x = "id", by.y = "participant_id", all = T)
vertex_metadata[, id := NULL]

# Keep only non-trivial components
vertex_metadata <- vertex_metadata[ count_holdings_core > 2 ]

## Convert to igraph
core_graph <- graph_from_data_frame(core[ participant_name %in% vertex_metadata$name | company_name %in% vertex_metadata$name, c("participant_name", "company_name", "share", "share_range")], directed = T, vertices = vertex_metadata)

# Line-wrap the name
V(core_graph)$name_label <- str_wrap(V(core_graph)$name, 15)

# Graph to ggnetwork
core_net <- ggnetwork(core_graph, layout = with_fr(), arrow.gap = 0.01)

# Plot the graph
core_plot <- ggplot(core_net, aes(x = x, y = y, xend = xend, yend = yend)) +
	geom_edges(color = "grey", arrow = arrow(angle = 45, length = unit(3, "pt"), type = "closed"), show.legend = F) +
	geom_nodes(aes(color = type, size = count_holdings_total), show.legend = T) +
	geom_edgetext(aes(label = share_range), color = "grey25", size = 1) +
	geom_nodetext_repel(aes(label = name_label, color = type), fontface = "bold", size = 2, show.legend = F, point.padding = 5) +
	scale_size(guide = "none") +
	scale_color_manual(name = "node type", labels = c("super-holder", "super-target", "intermediary", "core"), values = c("SH" = "#66c2a5", "ST" = "#8da0cb", "I" = "grey", "C" = "#fc8d62")) +
	guides(color = guide_legend(override.aes = list(size = 5))) + 
	theme_blank() + theme(text = element_text(family = "Open Sans"), legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"))

ggsave(paste0("output/network_examples/core_plot.pdf"), height = 10, width = 10, scale = 1.1, device = cairo_pdf)
