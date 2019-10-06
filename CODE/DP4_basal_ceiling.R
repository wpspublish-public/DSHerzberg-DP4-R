suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(runner)
suppressMessages(library(data.table))


input_orig_names <- suppressMessages(
  read_csv(
    here(
      'INPUT-FILES/SCORED-ITEMS/DP4-PHY-interview-scored-items.csv'
    )
  )
) 

# represent input item names as rows in a df. Thus, `df_names` provide key for
# matching input item names to sequential item numbering, the latter being
# represented in the df_names$value col.
df_names <- enframe(names(input_orig_names)[3:39])

# rename items to sequential numbering, `names(input)[1:2]` extracts first two
# var names, which don't change; `sprintf()` is a string function that allows
# you to create sequences with leading 0, e.g., '01, 02, 03'
input <- input_orig_names
names(input) <- c(names(input)[1:2], c(paste0('i', sprintf("%02d", 1:37))))

# Create a df with just IDnum age and total raw score, for later evalution of stop rule.
input_ID_raw_score <- input %>%   
  mutate(
  TOT_raw = rowSums(.[3:39])
) %>% 
  mutate(agestrat = case_when(
    age_in_months <= 23 ~ "0-0 to 1-11",
    inrange(age_in_months, 24, 47, incbounds=TRUE) ~ "2-0 to 3-11",
    inrange(age_in_months, 48, 71, incbounds=TRUE) ~ "4-0 to 5-11",
    inrange(age_in_months, 72, 258, incbounds=TRUE) ~ "6-0+",
    TRUE ~ NA_character_
  )
  ) %>% 
  select(IDnum, age_in_months, agestrat, TOT_raw) %>% 
  arrange(IDnum)


input_tidy <- input %>%
  # gather columns to express each cell as a key-value pair: col_name-cell_value
  gather(col, val, i01:i37) %>% 
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

# after examining frequency table `freq_1streak`, designate a streak_length to
# implement for each case in the input data.
streak_length <- 4

# Next sequence gets start item for start rule of streak_length + 1.
start_data <- input_tidy %>%
  # group_by(IDnum) allows you to find start item per case
  group_by(IDnum) %>% 
  # From the tidy input, label rows that contain correct responses and are part
  # of a win streak equal to or greater than streak_length. runner::streak_run
  # gets count, for each cell in val col, what is the current length of
  # streak of consecutive identical values?
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
  # streaks because they are a 1 follwed by all 0s, etc.), as well as items
  # themselves and other input data.
  full_join(input, by = "IDnum") %>% 
  mutate(agestrat = case_when(
    age_in_months <= 23 ~ "0-0 to 1-11",
    inrange(age_in_months, 24, 47, incbounds=TRUE) ~ "2-0 to 3-11",
    inrange(age_in_months, 48, 71, incbounds=TRUE) ~ "4-0 to 5-11",
    inrange(age_in_months, 72, 258, incbounds=TRUE) ~ "6-0+",
    TRUE ~ NA_character_
  )
  ) %>% 
  select(IDnum, agestrat, age_in_months, start, everything()) %>% 
  arrange(agestrat, age_in_months)

# freq table of start items within agestrats (DP-4 start ages).

freq_start <- start_data %>% 
  group_by(agestrat) %>% 
  drop_na(start) %>% 
  count(start) %>% 
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# Drop all rows where start item is top item (i33) - there are so many of these
# that it skews histogram and makes the graph difficult to read.
hist_data <- start_data %>% filter(start != "i33")


# histograms of start items within agestrats (DP-4 start ages).

hist_plot <-
  ggplot(data = hist_data, aes(start)) +
  stat_count(width = 0.5) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  labs(title = "Frequency Distribution", x = "Each bin is a count of a specific Total Score value", y = "Each histogram is an agestrat") +
  facet_wrap( ~ agestrat)
print(hist_plot)






# ceiling code below here.

input_tidy_orig_names <- input_orig_names %>%
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


output_ceiling <- input_orig_names[,1] %>% 
  left_join(input_tidy_orig_names) %>% 
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



# Next sequence tests stop rule of streak_length + 1.
stop_data <- input_tidy %>%
  # group_by(IDnum) allows you to find start item per case
  group_by(IDnum) %>% 
  # From the tidy input, label rows that contain incorrect responses and are
  # part of a losing streak equal to or greater than streak_length + 1.
  # runner::streak_run gets count, for each cell in val col, what is the current
  # length of streak of consecutive identical values?
  mutate(
    streak_x_0 = case_when(
      val == 0 & streak_run(val) >= streak_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    # First step of isolating the stop item: apply a label to only the first row
    # of streak_x_0, which you find by setting a logical T to the previous row
    # of streak_x being NA. The label you apply is the column number, meaning
    # that `stop_item` now holds the column number of a stop item. However,
    # there may be more than one stop item per case, if there are multiple
    # losing streaks of streak_length+1 within the case. We want the stop item
    # to be the lowest of these streaks, because that provides the most
    # stringent test of equivalency between the basic input raw total score, and
    # the raw total score with stop rule applied.
    stop_item = case_when(
      streak_x_0 == 1 & is.na(lag(streak_x_0)) ~ col_num,
      TRUE ~ NA_real_
    )
    ) %>% 
  # To recode items to 0 above the stop item, we need all cells in `stop_item`,
  # within each case, to hold the desired value of stop_item. We used two fill()
  # steps to accomplish this, the first one replicating the existing value
  # (replacing NA) of stop_item down the table, within each case.
  fill(stop_item) %>% 
  # second fill step reverses direction and fills existing values going up the
  # table.
  fill(stop_item, .direction = "up") %>% 
  # now we use `mutate()` to recode `stop_item` to its minimum vale per case, so
  # the stop rule is applied after the lowest losing streak of streak_length+1.
  # This recode step only affects cases that contain more than one value for
  # `stop_item`.
  mutate(stop_item = min(
    stop_item
    ),
    # now we are in a position to recode all values of `val` to 0 above the
    # desire stop item. We do this by isolating rows whose col_num is greater
    # than the col_num of the stop item, and recoding `val` to 0.
    val = case_when(
      col_num > stop_item ~ 0,
      TRUE ~ val
    )
    ) %>% 
    # spread data to get total score per case with stop rule applied. First drop interim cols.
  select(-col_num, -streak_x_0, -stop_item) %>% 
  spread(col, val) %>% 
  ungroup() %>% 
  mutate(
  TOT_stop_rule = rowSums(.[2:38])
  )

# compare TOT_raw and TOT_stop_rule
 TOT_compare <- input_ID_raw_score %>% 
  full_join(stop_data, by = 'IDnum') %>% 
  select(IDnum, age_in_months, agestrat, TOT_raw, TOT_stop_rule) %>% 
  group_by(agestrat)
  
agestrat_corr_mean_diff <- TOT_compare %>% 
  summarize(r = cor(TOT_raw, TOT_stop_rule),
            mean_diff = mean(TOT_raw - TOT_stop_rule)) %>% 
  mutate(r = round(r, 3),
         mean_diff = round(mean_diff, 3))




