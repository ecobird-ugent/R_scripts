# Reinoud Allaert - 10/03/2023
# see colour_combination_generator.md on Github for explanation

generate_colour_combinations <- function(colours, num_combinations, rings_left, rings_right, num_groups = 1, set_seed = sample(1:100000, 1)) {
  
  # Install dependencies
  if (!require("dplyr")) install.packages("dplyr")  
  if (!require("stringdist")) install.packages("stringdist")
  if (!require("stringr")) install.packages("stringr")
  if (!require("tidyr")) install.packages("tidyr")
    require(dplyr)
    require(stringdist)
    require(stringr)
    require(tidyr)

  # Check inputs
  if (!is.vector(colours) || length(colours) < 2) {
    stop("Error: 'colors' must be a vector of at least two colors.")
  }
  
  if (!is.numeric(num_combinations) || num_combinations < 1) {
    stop("Error: 'num_combinations' must be a positive integer.")
  }
  
  if (!is.numeric(rings_left) || rings_left < 1) {
    stop("Error: 'rings_left' must be a positive integer.")
  }
  
  if (!is.numeric(rings_right) || rings_right < 1) {
    stop("Error: 'rings_right' must be a positive integer.")
  }
  
  if (!is.numeric(num_groups) || num_groups < 1) {
    stop("Error: 'num_groups' must be a positive integer.")
  }
  if (!is.numeric(num_groups) || num_groups < 1) {
    stop("Error: 'num_groups' must be a positive integer.")
  }
  if (!is.numeric(set_seed) || num_groups < 1) {
    stop("Error: 'set_seed' must be a positive integer.")
  }
  
  # Set seed if seed was defined
  set.seed(set_seed)
  
  # Define function to sort characters in a string
  sort_string <- function(s) {
    str_sort(strsplit(s, "")[[1]], numeric = FALSE, decreasing = FALSE) %>%
      str_c(collapse = "")
  }
  
  # Generate all combinations of n colours for left leg
  colour_combinations_a <- expand.grid(replicate(rings_left, colours, simplify = FALSE))
  colour_combinations_a <- apply(colour_combinations_a, 1, paste, collapse = "")
  # Apply sort_string() to each element of colour_combinations_a
  sorted_combinations_a <- sapply(colour_combinations_a, sort_string)
  unique_sorted_combinations_a <- unique(sorted_combinations_a)
  
  
  # Generate all combinations of n colours for right leg
  colour_combinations_b <- expand.grid(replicate(rings_right, colours, simplify = FALSE))
  colour_combinations_b <- apply(colour_combinations_b, 1, paste, collapse = "")
  # Apply sort_string() to each element of colour_combinations_a
  sorted_combinations_b <- sapply(colour_combinations_b, sort_string)
  unique_sorted_combinations_b <- unique(sorted_combinations_b)
  
  
  L_R <- crossing(unique_sorted_combinations_a, unique_sorted_combinations_b) %>% 
    unite('ring_combination', sep="_")
  
  # Randomise order of ring combinations
  ShuffledUniqueColour <- L_R[sample(nrow(L_R)), ]
  
  combinations <- ShuffledUniqueColour
  
  # Check if the number of requested combinations is greater than the number of unique combinations
  if (num_combinations > nrow(L_R)) {
    stop("Error: The number of requested combinations is greater than the number of unique color combinations.")
  }
  
  # Check if the number of groups is greater than or equal to the number of combinations
  if (num_groups >= num_combinations) {
    stop("Error: The number of requested groups is greater than or equal to the number of requested combinations.")
  }
  
  # Determine the number of combinations per group
  if (num_groups == 1) {
    num_per_group <- num_combinations
    remainder <- 0
  } else {
    num_per_group <- floor(num_combinations/num_groups)
    remainder <- num_combinations - num_groups * num_per_group
  }
  
  # Split combinations into groups
  groups <- list()
  for (i in 1:num_groups) {
    if (i == num_groups && remainder > 0) {
      # Last group gets any remaining combinations
      num_in_group <- num_per_group + remainder
    } else {
      num_in_group <- num_per_group
    }
    
    group_combinations <- ShuffledUniqueColour[((i-1)*num_per_group + 1):(i*num_per_group),]
    
    # Find the most different combination from previous groups
    if (i > 1) {
      previous_groups <- do.call(rbind, groups)
      dists <- stringdistmatrix(as.character(group_combinations$ring_combination), 
                                as.character(previous_groups$ring_combination))
      min_dists <- apply(dists, 1, min)
      most_different <- group_combinations[which.max(min_dists), ]
      group_combinations <- group_combinations[-which.max(min_dists), ]
    }
    
    # Add a new column specifying the group
    group_combinations$group <- i
    
    groups[[i]] <- group_combinations
  }
  
  # Combine all groups into a single data frame
  result <- do.call(rbind, groups)
  
  # Return the data frame
  return(result)
}

                                            
