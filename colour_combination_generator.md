
### Function:  `generate_colour_combinations`

This function generates a list of unique colour combinations for a given number of combinations, number of colours for each leg, 
and a set of colours. The list is split into three groups, with each group containing as diverse a set of combinations as possible.

Parameters:
-  `colours` (required): A character vector of colours to generate combinations from.
-  `num_combinations` (required): An integer specifying the number of unique colour combinations to generate.
- `rings_left` (required): A positive integer specifying the number of colours in the left leg of the combination.
- `rings_right` (required): A positive integer specifying the number of colours in the right leg of the combination.
- `num_groups ` (optional): A positive integer specifying the number of groups to split the combinations into. Default is 1.
 `set_seed ` (optional): A positive integer specifying the seed, makes the function reproducible. Default is a random number.
 
Output:
The function returns a list of data frames containing unique colour combinations, split into the specified number of groups. 
Each data frame has two columns:  `ring_combination which` represents the combination of colours for both legs, and  `group` which 
represents the group number the combination belongs to. If the number of requested combinations is greater than the number of 
unique colour combinations, an error will be thrown.

```
# Two examples:
# Generate 50 unique colour combinations with 3 colours in the left leg and 2 colours in the right leg,
# split into 3 groups.
source("https://raw.githubusercontent.com/cobe-lab/R_scripts/main/colour_combination_generator.R")
generate_colour_combinations(
  colours = c("R", "B", "G", "Y", "P"), num_combinations = 50, rings_left = 3, rings_right = 2, num_groups = 3)

# Generate 100 unique colour combinations with 2 colours in each leg, split into 2 groups.
generate_colour_combinations(
  colours = c("R", "B", "G", "Y", "P"), num_combinations = 100, rings_left = 2, rings_right = 2, num_groups = 2)

```

