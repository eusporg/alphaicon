# List of R dependencies for the project
DEPENDENCIES:
	# Do nothing, the file is created outside the repo
	noop

# Helper function to install the dependencies
code/helper_functions/install_dependencies.r:
	# Do nothing, the file is created outside the repo
	noop

# People with Significant Control snapshot from Companies House
data/uk/persons-with-significant-control-snapshot-2021-08-02.txt:
	# Do nothing, the file is created outside the repo
	noop

# Company Data Product snapshot from Companies House
data/uk/BasicCompanyDataAsOneFile-2021-08-01.csv:
	# Do nothing, the file is created outside the repo
	noop

# Industry sector names
data/uk/sic_2007_code_list.csv:
	# Do nothing, the file is created outside the repo
	noop

# CorpWatch SEC 10-K filings data: company name-id mapping
data/corpwatch_api_tables_csv_14aug21/cik_name_lookup.csv:
	# Do nothing, the file is created outside the repo
	noop

# CorpWatch SEC 10-K filings data: basic company information
data/corpwatch_api_tables_csv_14aug21/company_info.csv:
	# Do nothing, the file is created outside the repo
	noop

# CorpWatch SEC 10-K filings data: company locations
data/corpwatch_api_tables_csv_14aug21/company_locations.csv:
	# Do nothing, the file is created outside the repo
	noop

# Process the PSC snapshot
data/uk/psc_snapshot_2021-08-02.rdata: data/uk/persons-with-significant-control-snapshot-2021-08-02.txt
	Rscript code/data_preparation/uk/1a_process_psc_snapshot.r

# Process the live snapshot of companies data
data/uk/uk_basic_companies_data_2021-08-01.rdata: data/uk/BasicCompanyDataAsOneFile-2021-08-01.csv data/uk/sic_2007_code_list.csv
	Rscript code/data_preparation/uk/1b_process_companies_data.r

# Convert the PSC snapshot to a company-participant clean data
output/uk/uk_organisations_participants_2021_long_2aug21.csv: data/uk/psc_snapshot_2021-08-02.rdata data/uk/uk_basic_companies_data_2021-08-01.rdata
	Rscript code/data_preparation/uk/2_psc_snapshot_to_participants_panel

# Create SEC 10-K Exhibit 21 company-participant evaluation set matched to PSC and live companies
data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv: data/corpwatch_api_tables_csv_14aug21/company_info.csv data/corpwatch_api_tables_csv_14aug21/cik_name_lookup.csv data/corpwatch_api_tables_csv_14aug21/company_locations.csv data/uk/psc_snapshot_2021-08-02.rdata data/uk/uk_basic_companies_data_2021-08-01.rdata
	Rscript code/data_preparation/uk/3_prepare_affiliated_entities_evaluation_data.r

# Classify the network into SH, ST, C, and I entities
output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv: output/uk/uk_organisations_participants_2021_long_2aug21.csv
	jupyter nbconvert --ExecutePreprocessor.timeout=-1 --execute code/alphaicon_paper/1_compute_alphaicon.ipynb

# Compute the shares by transitivity
transitiveshares := $(wildcard output/uk/transitive/uk_organisations_transitive_ownership_alpha*.csv)
$(transitiveshares): output/uk/uk_organisations_participants_2021_long_2aug21.csv output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv
	jupyter nbconvert --ExecutePreprocessor.timeout=-1 --execute code/alphaicon_paper/1_compute_alphaicon.ipynb

# Helper function implementing NPI/DPI computation
code/helper_functions/compute_power_index.r:
	# Do nothing, the file is created outside the repo
	noop

# Compute the DPI shares
output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_dpi_10000iter.csv: code/helper_functions/compute_power_index.r
	Rscript code/alphaicon_paper/2_compute_npi_dpi.r

# Compute the NPI shares
output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_npi_10000iter.csv: code/helper_functions/compute_power_index.r
	Rscript code/alphaicon_paper/2_compute_npi_dpi.r

# Perform the evaluation of algorithms at different k
output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall.csv: output/uk/uk_organisations_participants_2021_long_2aug21.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_dpi_10000iter.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_npi_10000iter.csv $(transitiveshares) output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv
	Rscript code/alphaicon_paper/5_algorithm_evaluation.r

# Perform the evaluation of algorithms at different path length
output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall_by_pathlength.csv: output/uk/uk_organisations_participants_2021_long_2aug21.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_dpi_10000iter.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_npi_10000iter.csv $(transitiveshares) output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv
	Rscript code/alphaicon_paper/5_algorithm_evaluation.r

# Create the ranking of top-100 holders by each method
output/alphaicon_paper/uk_organisations_top100_holders_2021_long_2aug21.csv: output/uk/uk_organisations_participants_2021_long_2aug21.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_dpi_10000iter.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_npi_10000iter.csv $(transitiveshares) output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv
	Rscript code/alphaicon_paper/6_rank_top_holders.r

# Compute Kendall's tau-b rank correlation of per-company participants for different methods
output/alphaicon_paper/kendall_taus_participant_ranks_dpi_npi_transitive_uk_organisations_participants_2021_7sep21.csv: output/uk/uk_organisations_participants_2021_long_2aug21.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_dpi_10000iter.csv output/uk/npi_dpi/10000iter/uk_organisations_participants_2021_long_7sep21_npi_10000iter.csv $(transitiveshares) output/uk/uk_organisations_participation_graph_core_periphery_membership_6aug21.csv data/uk/uk_parent_subsidiary_mapping_2020_2021_sec_filers_exhibit21.csv
	Rscript code/alphaicon_paper/6_rank_top_holders.r

# α-ICON paper
alphaicon_paper: output/alphaicon_paper/uk_organisations_top100_holders_2021_long_2aug21.csv output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall.csv output/alphaicon_paper/uk_orgs_algorithm_evaluation_recall_by_pathlength.csv DEPENDENCIES
	Rscript code/helper_functions/install_dependencies.r 
	Rscript code/helper_functions/compute_power_index.r
	Rscript code/alphaicon_paper/3_summary_stat_by_node_type.r
	Rscript code/alphaicon_paper/4_illustrate_algorithm.r
	Rscript code/alphaicon_paper/5_algorithm_evaluation.r
	Rscript code/alphaicon_paper/6_rank_top_holders.r
