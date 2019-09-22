library(tidyverse)
input <- tribble(
  ~ID, ~i1, ~i2, ~i3, ~i4, ~i5, ~i6, ~i7, ~i8, ~i9, ~i10, ~i11, ~i12, ~i13, ~i14, ~i15,
  "A", 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0,
  "B", 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
  "C", 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
  "D", 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0,
  "E", 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0,
  "F", 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  "G", 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0,
  "H", 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0,
  "I", 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  "J", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  "K", 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0,
  "L", 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0
)


input_tidy <- input %>%
  # gather columns to express each cell as a key-value pair: col_name-cell_value
  gather(col, val, -ID) %>%
  # group by ID, this allows a set of rows in the gathered table to be
  # identified with their origin row in the input table.
  group_by(ID) %>%
  # sort by ID
  arrange(ID) %>%
  # mutate creates new var, row_number() returns row number of input object, but
  # because object is grouped by ID, row_number() resets when ID changes to next
  # value; col_num adds 1 because in origin input table, the item columns start
  # on col 2
  mutate(col_num = row_number() + 1) 

# start this pipeline with the ID column only from the original input table.
output <- input[,1] %>% 
  # Combine these IDS with summary of each ID's first zero, filter reduces the
  # table so that it contains only the 0 cells from the original input rows
  left_join(input_tidy %>% filter(val == 0) %>%
              # In the input object there are multiple 0 rows per ID, and going
              # down the table they are arranged in aascending order of item
              # numbers going from left-to-right in the original input table.
              # first() returns only the first of those 0 rows, which, because
              # of the correspondence between the current row order and the
              # column order of the original input table, returns the column
              # name of the first item scored 0.
              summarize(first_0_name = first(col),
                        # analogously, this use of first() returns the column
                        # location of the first item scored 0.
                        first_0_loc = first(col_num))) %>%
  # Combine with length of each ID's first post-0 streak of 1's. First join
  # first_0_name and first_0_loc columns to gathered table from upstream. At
  # this point, the piped object has 180 rows, one row for each cell in the
  # crossing of cases(12) x items(15). The input object is grouped by ID, and
  # within each group of rows, the items are ordered ascending going down the
  # table
  left_join(input_tidy %>%
              # this filter pares rows out of each ID group, such that, for each
              # ID, the remaining rows begin with the row that holds the first 1
              # score after the first 0 score, and end with the row that holds
              # the last consecutive 1 score before the next 0 score (i.e., this
              # number of rows (for each ID) is now equal to the length of the string of
              # consecutive 1 scores above the first 0 score)
              filter(val == 1 & cumsum(val == 1 & lag(val, default = 1) == 0) == 1) %>% 
              # this summarize uses n() to create a new variable that is simple
              # the number of rows within each ID grouping, which as noted above
              # is equivalent to the length of the streak of 1 scores above the
              # first 0 score.
              summarize(streak_1 = n())) %>% 
              select(-ID) %>% 
              mutate(streak_1 = as.numeric(streak_1))



new_cols <- tribble(
  ~first_0_name, ~first_0_loc, ~streak_1,
  "i9", 10, 5,
  "i4", 5, 4,
  "i6", 7, 8,
  "i8", 9, 4,
  "i9", 10, 5,
  NA, NA, NA,
  "i1", 2, 5,
  "i3", 4, 8,
  "i2", 3, NA,
  "i1", 2, 1
)
