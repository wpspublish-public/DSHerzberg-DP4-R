# Here is a simplified version of a problem that involves processing a large complex table. Here is the input table:
# 
# ```
# simplified input
library(tidyverse)
input <- tribble(
  ~group, ~score, ~label,
  1, 10, 'A',
  1, 20, 'B',
  1, 30, 'C',
  1, 40, 'D',
  2, 11, 'A',
  2, 21, 'B',
  2, 31, 'C',
  2, 41, 'D',
  3, 12, 'A',
  3, 22, 'B',
  4, 13, 'A',
  4, 23, 'B',
  4, 33, 'C',
  4, 43, 'D'
)
# ```
# The table has 14 rows. The data are grouped in numbered groups (1:4), each group is supposed to have four scores labeled A, B, C, D.
# The problem is group 3, which is missing the C and D rows.
# 
# I want R to do the following:
#   1. Find group 3 based on its lack of C and D rows.
# 2. Insert C and D rows for group 3, in proper alphabetical sequence.
# 3. Populate `score` in the new C and D rows with the value of of `score` (22) from group 3 row B.
# 
# Another way of describing the transformation is that I want two insert two copies of row 3B, changing the label
# of those copied rows from B to C and D, respectively.
# 
# The desired output table has 16 rows and looks like this:
# 
# 
# ```
#  desired output
output <- tribble(
  ~group, ~score, ~label,
  1, 10, 'A',
  1, 20, 'B',
  1, 30, 'C',
  1, 40, 'D',
  2, 11, 'A',
  2, 21, 'B',
  2, 31, 'C',
  2, 41, 'D',
  3, 12, 'A',
  3, 22, 'B',
  3, 22, 'C',
  3, 22, 'D',
  4, 13, 'A',
  4, 23, 'B',
  4, 33, 'C',
  4, 43, 'D'
)
# ```
# Thanks in advance for any help!

#  SOLUTION FROM STACK OVERFLOW.
solution <- complete(input, group, label) %>%
  fill(score) %>% select(group, score, label)

identical(output, solution)
  
# APPLIED TO DP4 DATA
solution <- complete(input, agestrat, dist_point) %>%
  fill(IRS_hi1)

input <- input %>% filter (dist_point != 'hi1')
input <- input %>% filter(dist_point != 'hi2')
input <- input %>% filter((dist_point != 'med') && (agestrat != 228))
input <- input %>% filter(!(dist_point == 'med') && (agestrat == 204))


library(tidyverse)
input <- tribble(
  ~group, ~score, ~label,
  1, 10, 'lo1',
  1, 20, 'lo2',
  1, 30, 'med',
  1, 40, 'hi1',
  2, 11, 'lo1',
  2, 21, 'lo2',
  2, 31, 'med',
  2, 41, 'hi1',
  3, 12, 'lo1',
  3, 22, 'lo2',
  4, 13, 'lo1',
  4, 23, 'lo2',
  4, 33, 'med',
  4, 43, 'hi1'
)

solution <- complete(input, group, label) %>%
  fill(score) %>% select(group, score, label) %>% arrange(group, score)

