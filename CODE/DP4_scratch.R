suppressMessages(library(here))
suppressMessages(library(tidyverse))
library(runner)
library(data.table)


# Next sequence rescores input data by applying start rule of streak_1_length +
# 1 and stop rule of streak_0_length + 1. The approach is to find the highest
# start rule streak for each case, and recode all 0s below this streak to 1, and
# to find the lowest stop rule streak and recode all 1s above this streak to O.
# The code accomplishes this dual-recoding in a single pass.
rescore_data <- input_tidy %>%
  # group_by(IDnum) allows you to find streaks indepently within each case.
  group_by(IDnum) %>% 
  # From the tidy input, label rows that contain correct responses (val = 1) and
  # are part of a win streak equal to or greater than streak_1_length.
  # runner::streak_run gets count, for each cell in val col, what is the current
  # length of streak of consecutive identical values? Repeat for val = 0.
  mutate(
    streak_x_1 = case_when(
      val == 1 & streak_run(val) >= streak_1_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    streak_x_0 = case_when(
      val == 0 & streak_run(val) >= streak_0_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    # `start_item` and `stop_item` will contain the items from which to commence
    # upward and downward rescoring (application of start/stop rules). First
    # step of isolating the start item: apply a label to only the last row of
    # streak_x, which you find by testing whether, in the lead row to any row,
    # the value of streak_x is NA. If that condition is TRUE, then the current
    # row is the last correct response of streak_x. The label you apply is the
    # column number of that row. To apply this same function for streaks of 0,
    # you change the lead() wrapper to lag(), and if this modified `is.na`
    # condition is TRUE, the current row is the first 0 response of the streak
    # of 0s.
    start_item = case_when(
      streak_x_1 == 1 & is.na(lead(streak_x_1)) ~ col_num,
      TRUE ~ NA_real_
    ),
    stop_item = case_when(
      streak_x_0 == 1 & is.na(lag(streak_x_0)) ~ col_num,
      TRUE ~ NA_real_
  )
  ) %>% 
  # To rescore according to the stop/start rules, we need all cells in the
  # `start_item` and `stop_item` columns, within each case, to hold the items
  # that are the thresholds for rescoring above and below. We use two fill()
  # steps to accomplish this, the first one replicating the existing value
  # (replacing NA) of start_item and stop_item down the table, within each case.
  fill(start_item, stop_item) %>% 
  # second fill step reverses direction and fills existing values going up the
  # table.
  fill(start_item, stop_item, .direction = "up") %>% 
  # now we use `mutate()` to recode `start_item` to its maximum value per case,
  # and stop_item` to its minimum value per case, so the start rule is applied
  # below the highest winning streak of streak_1_length +1, and the stop rule is
  # applied above the lowest losing streak of streak_0_length+1. This recode
  # step only affects cases that contain more than one value for `start_item`
  # and/or `stop_item`.
  mutate(start_item = max(
    start_item
  ),
  stop_item = min(
    stop_item
  ),
  # now we are in a position to recode all values of `val` to 1 below the
  # desired start_item, and to 0 above the desire stop item. We do this
  # squentially, by isolating rows whose col_num is less than the col_num of teh
  # start item, and recoding `val` to 1, and then isolating rows whose col_num
  # is greater than the col_num of the stop item, and recoding `val` to 0.
  val = case_when(
    col_num < start_item ~ 1,
    TRUE ~ val
  ),
  val = case_when(
    col_num > stop_item ~ 0,
    TRUE ~ val
  )
  ) %>% 
  # spread data to get total score per case with stop rule applied. First drop interim cols.
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



