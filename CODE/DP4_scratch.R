suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(runner)

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

streak_length <- 4

input_repex_tidy <- input_repex %>%
  # transform rows on input table into columns, ows of input table expressed as
  # key-value pairs, key column = input col name, val column = cell value for
  # that column
  gather(col, val, -ID) %>%
  # grouping identifies new cells with their origin rows on input table.
  group_by(ID) %>%
  arrange(ID) %>%
  # create new col that gives origin col num from input table for each cell
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
  # with last row of input table) - append this logic statement: `&
  # (lead(streak) == 1 | is.na(lead(streak))))`
  filter(val == 1 & streak >= streak_length) %>%
  # In the input object there are multiple rows per ID, and going down the table
  # they are arranged in ascending order of item numbers going from
  # left-to-right in the original input table. last() returns only the last of
  # those rows within each ID, which, because of the correspondence between the
  # current row order and the column order of the original input table, returns
  # the column name of the last item in the highest run of a streak of 1s that
  # is greater than or equal to your chosen streak length.
  summarize(last_streak1_name = last(col)) %>% 
  full_join(input_repex, by = "ID") %>% 
  select(ID, last_streak1_name) %>% 
  arrange(ID)

# Next sequence gets start item for start rule of streak_length + 1.
start_repex <- input_repex_tidy %>%
  group_by(ID) %>% 
  # From the tidy input, label rows that contain correct responses and are part
  # of a win streak equal to or greater than streak_length.
   mutate(
     streak_x = case_when(
       val == 1 & streak_run(val) >= streak_length ~ 1,
       TRUE ~ NA_real_
     ),
     # First step of isolating the start item: apply a label to only the last
     # row of streak_x, which you find by setting a logical T to the next row of
     # streak_x being NA. The label you apply is the column number that lags the
     # row you chose by streak_length. Start_item now holds the column number of
     # the desired start item.
     start_item = case_when(
       streak_x == 1 & is.na(lead(streak_x)) ~ col_num - streak_length,
       TRUE ~ NA_real_
     )
   ) %>% 
  # Now that you know the col num of desired start item per case, you have to
  # isolate the rows that actually contain those items, which you accomplish by
  # selecting (filter) rows whose value of col_num equals the value of
  # start_item in the row that leads (is ahead) of the row you want by
  # streak_length. To understand the operation of the filter, start with a row
  # that contains a value for start_item, lag this row by streak_length, and you
  # end up on the row that contains the col_num corresponding to the desired
  # start_item.
  filter(col_num == lead(start_item, streak_length)) %>% 
  # recode start_item so that it contains the actual name of the start item,
  # which is found in column `col` from the tidy input.
  mutate(start_item = col) %>% 
  # At this point the data object contains a start item for each streak >=
  # streak_length. We only want the last (highest) of these start items, because
  # that is the one that corresponds to the highest basal streak (so it will
  # yield a true basal). We use summarize() to return only the last start item
  # for each case (remember, data object is already grouped by ID, so it can be
  # summarized)
  summarize(start = last(start_item)) %>% 
  # need next full_join to bring back NA rows (e.g., rows that have no basal
  # streaks because they are a 1 follwed by all 0s, etc.)
  full_join(input_repex, by = "ID") %>% 
  select(ID, start) %>% 
  arrange(ID)


