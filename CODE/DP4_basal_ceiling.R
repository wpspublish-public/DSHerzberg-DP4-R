suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(runner)

input <- suppressMessages(
  read_csv(
    here(
      'INPUT-FILES/SCORED-ITEMS/DP4-PHY-interview-scored-items.csv'
    )
  )
) 

input_tidy <- input %>%
  # gather columns to express each cell as a key-value pair: col_name-cell_value
  gather(col, val, PHY06:PHY48) %>% 
  # group by IDnum, this allows a set of rows in the gathered table to be
  # identified with their origin row in the input table.
  group_by(IDnum) %>%
  # sort by IDnum
  arrange(IDnum) %>%
  # drop age column
  select(-age_in_months) %>% 
  # mutate creates new var, row_number() returns row number of input object, but
  # because object is grouped by IDnum, row_number() resets when IDnum changes to next
  # value; col_num adds 2 because in origin input table, the item columns start
  # on col 3
  mutate(col_num = row_number() + 2) 

# start this pipeline with the IDnum column only from the original input table.
output_basal <- input[,1] %>% 
  # Combine these IDnumS with summary of each IDnum's first zero, filter reduces the
  # table so that it contains only the 0 cells from the original input rows
  left_join(input_tidy %>% filter(val == 0) %>%
              # In the input object there are multiple 0 rows per IDnum, and going
              # down the table they are arranged in ascending order of item
              # numbers going from left-to-right in the original input table.
              # first() returns only the first of those 0 rows, which, because
              # of the correspondence between the current row order and the
              # column order of the original input table, returns the column
              # name of the first item scored 0.
              summarize(first_0_name = first(col),
                        # analogously, this use of first() returns the column
                        # location of the first item scored 0.
                        first_0_loc = first(col_num))) %>%
  # Combine with length of each IDnum's first post-0 streak of 1's. First join
  # first_0_name and first_0_loc columns to gathered table from upstream. At
  # this point, the piped object has one row for each cell in the
  # crossing of cases x items. The input object is grouped by IDnum, and
  # within each group of rows, the items are ordered ascending going down the
  # table
  left_join(input_tidy %>%
              # this filter pares rows out of each IDnum group, such that, for each
              # IDnum, the remaining rows begin with the row that holds the first 1
              # score after the first 0 score, and end with the row that holds
              # the last consecutive 1 score before the next 0 score (i.e., this
              # number of rows (for each IDnum) is now equal to the length of the
              # string of consecutive 1 scores above the first 0 score)
              #
              # More detail on how the filter works: it catches rows that 1)
              # represent a 1 score on an item (val = 1); 2) are part of the
              # string of consecutive 1 scores (val = 1) that appear obove a 0
              # response for that IDnum (lag(val, default = 1) == 0) -- lag gets
              # the value of val from the row preceding the current row, so when
              # that value is 0, lag will catch it; default = 1 deals with first
              # row, which has no lag row, and would return NA if the default
              # were not set to 1; 3) the cumsum() wrapper keeps a running count
              # of 1 streaks above a 0 (identified by the compound logic val = 1
              # & lag(val) = 0), and by setting the logical condition of
              # cumsum() = 1, it catches only rows that appear in the first such
              # streak, where the value of that running count would be 1.
              filter(val == 1 & cumsum(val == 1 & lag(val, default = 1) == 0) == 1) %>% 
              # this summarize uses n() to create a new variable that is simply
              # the number of rows within each IDnum grouping, which as noted above
              # is equivalent to the length of the streak of 1 scores above the
              # first 0 score.
              summarize(streak_1 = n())) %>% 
              # select(-IDnum) %>% 
              mutate(streak_1 = as.numeric(streak_1)) %>% 
              left_join(input, by = 'IDnum')

freq_1streak <- output_basal %>% 
  drop_na(streak_1) %>% 
  count(streak_1) %>% 
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
    )

highest_basal_streak <- input_tidy %>%
  # create new col using runner::streak_run to count, for each cell in val col,
  # what is the current length of consecutive identical values
  mutate(streak = streak_run(val)) %>%
  # pare table so that it includes only streaks of a certain length, for only
  # certain values. To make sure that streaks of x are not simply the first
  # section of longer streaks of length x + n, only keep streaks where the
  # leading value of streak is either 1 (meaning streak has reset because
  # previous streak ended at x) or NA (for streaks that end with last row of
  # input table)
  filter(val == 1 & (streak == 4 & (lead(streak) == 1 | is.na(lead(streak))))) %>%
  # In the input object there are multiple rows per IDnum, and going
  # down the table they are arranged in ascending order of item
  # numbers going from left-to-right in the original input table.
  # last() returns only the last of those rows within each IDnum, which, because
  # of the correspondence between the current row order and the
  # column order of the original input table, returns the column
  # name of the last item in the highest run of a streak of 4 1s.
  summarize(last_streak1_name = last(col)) %>% 
  full_join(input, by = "IDnum") %>% 
  select(IDnum, last_streak1_name) %>% 
  arrange(IDnum)

test <- highest_basal_streak %>% drop_na()




output_ceiling <- input[,1] %>% 
  left_join(input_tidy) %>% 
  group_by(IDnum) %>%
  arrange(IDnum) %>%
  filter(val == 1) %>% 
  slice(tail(row_number(), 2)) %>% 
    summarize(
      last_1_name = last(col),
      last_1_loc = last(col_num),
      sec_last_1_name = first(col),
      sec_last_1_loc = first(col_num)
    ) %>% 
  mutate(
    streak_0 = case_when(
      last_1_loc > sec_last_1_loc ~ (last_1_loc - sec_last_1_loc) - 1,
      TRUE ~ NA_real_
    )
  )
  
freq_0streak <- output_ceiling %>% 
  drop_na(streak_0) %>% 
  count(streak_0) %>% 
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

