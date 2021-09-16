# This code implements Algorithm 1
# "Label propagation for NPI"
# from Mizuno, Doi, and Kurizaki (2020, p. 7)
# (https://doi.org/10.1371/journal.pone.0237862)
library(data.table)

# Function to compute Network or Direct power index
# INPUTS: a data.table with participant-entity relations
# in long form or an igraph object with edge weights
# OUTPUTS: data.table with power index for each participant
#	or named list with such table and labels from each iteration
# 	(if save_labels = T).

compute_power_index <- function(relations_object,
								participant_var = "participant",
								entity_var = "entity",
								weight_var = "weight",
								powerindex = "dpi", # which power index to compute
								iterations = 100, # how many iterations to run
								quota = 50, # majority rule, % 
								epsilon = 0.01, # probability to restore the original label when computing the NPI
								iterations_to_discard = 20, # how many iterations to discard when computing the NPI
								save_labels = FALSE # whether to save labels from each iteration
								) {

	# Debug: powerindex <- "dpi"; iterations <- 100; quota <- 50; epsilon <- 0.01; iterations_to_discard <- 20; save_labels <- F; participant_var = "participant"; entity_var = "entity"; weight_var = "voting_share"

	# Input checks
	## Does the relations object exist and is it in correct form?
	stopifnot("relations_object is not specified" = !missing(relations_object))

	## If the relations object is in igraph transform to a data.frame
	if( "igraph" %in% class(relations_object) ) {
		
		relations_object <- igraph::as_data_frame(relations_object, what = "edges")

		# Check whether a weight column is present in the data (i.e. data.frame has at least 3 columns
		stopifnot("`relations_object` graph does not have edge weights" = ncol(relations_object) >= 3)

		# Proper names
		names(relations_object)[1:2] <- c(participant_var, entity_var)

	}

	## If the relations object is not a data.table try to coerce to it
	if( !("data.table" %in% class(relations_object)) ) {

		data.table::setDT(relations_object)

	}

	## Are the participant and entity variables in the data?
	stopifnot("`participant_var` variable not found in `relations_object`" = participant_var %in% names(relations_object))
	stopifnot("`entity_var` variable not found in `relations_object`" = entity_var %in% names(relations_object))
	stopifnot("`weight_var` variable not found in `relations_object`" = weight_var %in% names(relations_object))

	## Are the power index options correctly specified?
	stopifnot("`powerindex` should be either dpi or npi" = powerindex %in% c("npi", "dpi"))
	stopifnot("`save_labels` should be either TRUE or FALSE" = save_labels %in% c(TRUE, FALSE))

	## Are the parameters of the required type?
	stopifnot("`iterations should` be an integer" = all.equal(iterations, as.integer(iterations)) )
	stopifnot("`iterations_to_discard` should be an integer not exceeding `iterations`" = (all.equal(iterations_to_discard, as.integer(iterations_to_discard)) & iterations_to_discard < iterations) | powerindex == "dpi" )
	min_weight <- min(relations_object[[weight_var]], na.rm = T)
	max_weight <- max(relations_object[[weight_var]], na.rm = T)
	stopifnot("`weight_var` should take non-negative numeric values" = min_weight >= 0 & is.finite(max_weight) & is.finite(min_weight) )
	stopifnot("epsilon should be in [0,1] range" = (epsilon >= 0 & epsilon <= 1) | powerindex == "dpi" )

	## Disable the `iterations_to_discard` option
	## when computing DPI
	if( powerindex == "dpi" ) {

		iterations_to_discard <- 0
		
	}

	# Remove observations with missing data
	## Participant
	if( nrow(relations_object[ is.na(get(participant_var)) ]) > 0 ) {
		
		warning(paste0("Removing rows with missing ", participant_var, " before computation"))
		relations_object <- relations_object[ !is.na(get(participant_var)) ]

	}
	## Entity
	if( nrow(relations_object[ is.na(get(entity_var)) ]) > 0 ) {
		
		warning(paste0("Removing rows with missing ", entity_var, " before computation"))
		relations_object <- relations_object[ !is.na(get(entity_var)) ]

	}
	## Weight
	if( nrow(relations_object[ is.na(get(weight_var)) ]) > 0 ) {
		
		warning(paste0("Removing rows with missing ", weight_var, " before computation"))
		relations_object <- relations_object[ !is.na(get(weight_var)) ]

	}

	# Init an object to update labels in each iteration
	updating_relations <- copy(relations_object[, c(participant_var, entity_var, weight_var), with = F])
	setnames(updating_relations, c("participant", "entity", "weight"))

	# Init an object to store per-iteration
	# entity labels (i.e. who the pivotal
	# shareholder is)
	if( save_labels == T) {
	
		iteration_labels <- data.table()
	
	# Otherwise we store overall counts of each
	# label being named a pivot
	} else {
	
		pivot_counts <- updating_relations[, c("participant", "entity")]
		pivot_counts[, times_pivotal := 0 ]

	}

	# Create a progress bar to report on iteration progess
	message(paste0("Iterating through ", iterations, " permutations:"))
	iterations_progess_bar <- txtProgressBar(min = 0, max = iterations, style = 3)

	for(i in 1:iterations) {
	
		# Randomly order rows within entities
		updating_relations[, random_var := runif(.N)]
		setorderv(updating_relations, c("entity", "random_var"))
	
		# Breaking cycles when computing NPI: restore original
		# labels for the random set of rows with epsilon
		# probability
		if( powerindex == "npi" & epsilon > 0 ) {

			entities_to_restore <- unique(updating_relations[ random_var <= epsilon ]$entity)
	
			if( length(entities_to_restore) > 0 ) {

				restored_entities <- relations_object[ get(entity_var) %in% entities_to_restore, c(participant_var, entity_var, weight_var), with = F]
				setnames(restored_entities, c("participant", "entity", "weight"))

				# Randomly shuffle the labels to restore as well respect random ordering
				restored_entities[, random_var := runif(.N) ]
				setorderv(restored_entities, c("entity", "random_var"))

				updating_relations <- rbind(updating_relations[ !( entity %in% entities_to_restore) ], 
								restored_entities,
								fill = T)

			}

		}
		# Compute cumulative sums of voting rights by entity
		updating_relations[, cumulative_weight := cumsum(weight), by = "entity"]
	
		# Lagged cumulative voting share
		# shift() is not optimized with many groups
		# (https://github.com/Rdatatable/data.table/issues/1534)
		# so we cannot simply use `by` statement as below 
		# updating_relations[, cumulative_weight_l1 := shift(cumulative_weight, type = "lag", n = 1), by = "entity"]
		#
		# Instead, consider a two-step hack that uses our entity ordering
		# to mark as missing lagged weights for the first participant per entity
		updating_relations[, cumulative_weight_l1 := shift(cumulative_weight, type = "lag", n = 1)]
		updating_relations[, entity_l1 := shift(entity, type = "lag", n = 1)]
		updating_relations[entity != entity_l1, cumulative_weight_l1 := NA_real_ ]

		# Identify pivots
		pivots <- updating_relations[ cumulative_weight >= quota & (cumulative_weight_l1 < quota | is.na(cumulative_weight_l1) ), c("participant", "entity") ]
	
		# Store pivots from each iteration in a designated object
		if( save_labels == T) {
	
			iteration_labels <- rbind(iteration_labels, data.table(iteration = i, pivots), fill = T)
	
			# NB: ^ this is the most problematic step in terms of scaling.
			# Where to store and access an N * number of iterations table
			# with labels when N is huge and iterations = 20 000? We need
			# to save pivots for ~3 million entities 20 thousand times.
			#
			# Alternatively you can write pivots from each iteration to a separate CSV
			# but you will still end up with 20 thousand files, each having ~3 million lines.
		
		}
	
		# We can avoid storing the pivots altogether and just
		# add to the total count of times a given partipant is
		# named pivotal in a given entity. We also discard
		# the first `iterations_to_discard` iterations
		if( save_labels == F & i > iterations_to_discard ) {
	
			# We row bind the current pivots to the existing counts
			# and then sum. This might be faster than merging on
			# entity and participant.
			# In fact, it is very slow
			pivot_counts <- rbind(pivot_counts, data.table(pivots, times_pivotal = 1), fill = T)
			pivot_counts <- pivot_counts[, list( times_pivotal = sum(times_pivotal) ), by = c("entity", "participant")]
	
		}
	
		# When computing NPI: update the participant
		# labels with pivots identified during this
		# iteration step
		if( powerindex == "npi" ) {

			updating_relations[, participant_update := pivots[ match(updating_relations$participant, pivots$entity) ]$participant ]
			updating_relations[ !is.na(participant_update), participant := participant_update ]
			updating_relations[, participant_update := NA ]

			# Sum up the updated shares for the next iteration
			updating_relations <- updating_relations[, list(weight = sum(weight)), by = c("entity", "participant")]
	
		}

		# Update the progress bar
		setTxtProgressBar(iterations_progess_bar, i)
	
	}

	# Compute the index itself
	if( save_labels == T ) {
	
		# Count the number of times this
		# participant was named the pivotal player,
		# discarding first `iterations_to_discard` iterations
		index_values <- iteration_labels[iteration > iterations_to_discard, list(times_pivotal = .N), by = c("entity", "participant")]
	
	} else {
	
		index_values <- pivot_counts
	
	}

	# Compute the index
	index_values[, index := times_pivotal/(iterations - iterations_to_discard) ]
	setnames(index_values, "index", powerindex)

	if( save_labels == T ) {

		return(list(index_values = index_values, iteration_labels = iteration_labels))

	} else {

		return(index_values)

	}

}
