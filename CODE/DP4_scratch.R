suppressMessages(library(tidyverse))
library(runner)

# set.seed(11)
# x1 <- sample(c("a","b"),15,replace=TRUE)
# x2 <- sample(c(NA_character_,"a","b"),15,replace=TRUE)
# k <- sample(1:4,15,replace=TRUE)
# streak_run(x1) # simple streak run
# streak_run(x1, k=2) # streak run within 2-element window
# streak_run(x2, na_pad=TRUE, k=3) # streak run within k=3 with padding NA
# streak_run(x1, k=k) # streak run within varying window size specified by vector k
# test <- input_tidy %>% mutate(streak = streak_run(val))

input_repex <- tribble(
  ~ID, ~i1, ~i2, ~i3, ~i4, ~i5, ~i6, ~i7, ~i8, ~i9, ~i10, ~i11, ~i12, ~i13, ~i14, ~i15,
  "A", 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0,
  "B", 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
  "C", 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
  "D", 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0,
  "E", 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0,
  "F", 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  "G", 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0,
  "H", 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0,
  "I", 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  "J", 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1,
  "K", 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1,
  "L", 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1
)

input_repex_tidy <- input_repex %>%
  # transform rows on input table into columns, ows of input table expressed as
  # key-value pairs, key column = input col name, val column = cell value for
  # that column
  gather(col, val, -ID) %>%
  # grouping identifies new cells with their origin rows on input table.
  group_by(ID) %>%
  arrange(ID) %>%
  # create new col that gives origin col num rom input table for each cell
  mutate(col_num = row_number() + 1) 

test_repex <- input_repex_tidy %>%
  # create new col using runner::streak_run to count, for each cell in val col,
  # what is the current length of consecutive identical values
  mutate(streak = streak_run(val)) %>%
  # pare table so that it includes only streaks of a certain length, or longer,
  # for only certain values. If you want, you can make sure that streaks of x
  # are not simply the first section of longer streaks of length x + n, only
  # keep streaks where the leading value of streak is either 1 (meaning streak
  # has reset because previous streak ended at x) or NA (for streaks that end
  # with last row of input table) - append this logic statement: `& (lead(streak) == 1 | is.na(lead(streak))))`
  filter(val == 1 & streak >= 4) %>%
  # In the input object there are multiple rows per ID, and going
  # down the table they are arranged in ascending order of item
  # numbers going from left-to-right in the original input table.
  # last() returns only the last of those rows within each ID, which, because
  # of the correspondence between the current row order and the
  # column order of the original input table, returns the column
  # name of the last item in the highest run of a streak of 4 1s.
  summarize(last_streak1_name = last(col)) %>% 
  full_join(input_repex, by = "ID") %>% 
  select(ID, last_streak1_name) %>% 
  arrange(ID)


