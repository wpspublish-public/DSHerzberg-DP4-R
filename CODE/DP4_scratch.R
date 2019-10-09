suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(runner)
library(data.table)

start_data <- input_tidy %>%
  # group_by(IDnum) allows you to find start item per case
  group_by(IDnum) %>% 
  # From the tidy input, label rows that contain correct responses and are
  # part of a winning streak equal to or greater than streak_1_length + 1.
  # runner::streak_run gets count, for each cell in val col, what is the current
  # length of streak of consecutive identical values?
  mutate(
    streak_x_1 = case_when(
      val == 1 & streak_run(val) >= streak_1_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    # First step of isolating the highest basal streak: apply a label to only the first row
    # of streak_x_1, which you find by setting a logical T to the next row
    # of streak_x being NA. The label you apply is the column number, meaning
    # that `start_item` now holds the column number of a start item. However,
    # there may be more than one start item per case, if there are multiple
    # winning streaks of streak_1_length+1 within the case. We want the start item
    # to be the highest of these streaks, because that provides the most
    # stringent test of equivalency between the basic input raw total score, and
    # the raw total score with start rule applied.
    start_item = case_when(
      streak_x_1 == 1 & is.na(lead(streak_x_1)) ~ col_num,
      TRUE ~ NA_real_
    )
  ) %>% 
  # To recode items to 1 below the start item, we need all cells in `start_item`,
  # within each case, to hold the desired value of start_item. We use two fill()
  # steps to accomplish this, the first one replicating the existing value
  # (replacing NA) of start_item down the table, within each case.
  fill(start_item) %>% 
  # second fill step reverses direction and fills existing values going up the
  # table.
  fill(start_item, .direction = "up") %>%   
  # now we use `mutate()` to recode `start_item` to its maximum value per case, so
  # the start rule is applied after finding the highest winning streak of streak_1_length+1.
  # This recode step only affects cases that contain more than one value for
  # `start_item`.
  mutate(start_item = max(
    start_item
  ),
  # now we are in a position to recode all values of `val` to 1 below the
  # desired start item. We do this by isolating rows whose col_num is less
  # than the col_num of the stop item, and recoding `val` to 1.
  val = case_when(
    col_num < start_item ~ 1,
    TRUE ~ val
  )
  ) %>% 
  # spread data to get total score per case with start rule applied. First drop interim cols.
  select(-col_num, -streak_x_1, -start_item) %>% 
  spread(col, val) %>% 
  ungroup() %>% 
  mutate(
    TOT_start_rule = rowSums(.[2:38])
  )

  # Can you rescore data applying start and stop rule in a single pass?

rescore_data <- input_tidy %>%
  group_by(IDnum) %>% 
  mutate(
    streak_x_1 = case_when(
      val == 1 & streak_run(val) >= streak_1_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    streak_x_0 = case_when(
      val == 0 & streak_run(val) >= streak_0_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    start_item = case_when(
      streak_x_1 == 1 & is.na(lead(streak_x_1)) ~ col_num,
      TRUE ~ NA_real_
    ),
    stop_item = case_when(
      streak_x_0 == 1 & is.na(lag(streak_x_0)) ~ col_num,
      TRUE ~ NA_real_
  )
  ) %>% 
  fill(start_item, stop_item) %>% 
  fill(start_item, stop_item, .direction = "up") %>% 
  mutate(start_item = max(
    start_item
  ),
  stop_item = min(
    stop_item
  ),
  val = case_when(
    col_num < start_item ~ 1,
    TRUE ~ val
  ),
  val = case_when(
    col_num > stop_item ~ 0,
    TRUE ~ val
  )
  ) %>% 
  select(-col_num, -streak_x_1, -start_item, -streak_x_0, -stop_item) %>% 
  spread(col, val) %>% 
  ungroup() %>% 
  mutate(
    TOT_rescore = rowSums(.[2:38])
  )

# compare TOT_raw and TOT_rescore
TOT_compare <- input_ID_raw_score %>% 
  full_join(rescore_data, by = 'IDnum') %>% 
  select(IDnum, age_in_months, agestrat, TOT_raw, TOT_rescore) %>% 
  group_by(agestrat)

agestrat_corr_mean_diff <- TOT_compare %>% 
  summarize(r = cor(TOT_raw, TOT_rescore),
            mean_diff = mean(TOT_raw - TOT_rescore)) %>% 
  mutate(r = round(r, 3),
         mean_diff = round(mean_diff, 3))



