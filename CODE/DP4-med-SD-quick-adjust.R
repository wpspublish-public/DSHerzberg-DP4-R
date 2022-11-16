# This script allows you to change smoothed meds and SDs on the fly and quickly
# see the how the changes affect look up tables. It's based on the final med/SD
# descriptives output table that you get from running the main norms script. The
# meds and SD cells in that table can be edited in excel and then saved out into
# .csv, for reading back into R.

# The current script first reads in the the edited med/SD output table from the
# first run of the main norms script. It then regenerates the raw-to-SS and
# print-format lookup tables, based on the edited input. The operator can cycle
# through this iteratively, adjusting means and SDs in a step-wise fashion, and
# evaluating their effects.

norms_edit_names <- c('group', 'agestrat', 'lo_SD_sm', 'median_sm', 'hi_SD_sm', 'ES_sm')
smooth_med_SD <- read_csv(here('OUTPUT-FILES/DESCRIPTIVE-TABLES/PHYscore-final_med_SD_table-2019-08-18.csv'))
names(smooth_med_SD) <- norms_edit_names
smooth_med_SD <- smooth_med_SD %>% select(group, agestrat, lo_SD_sm, hi_SD_sm, median_sm, ES_sm)

# next code section generates raw-to-SS look-up tables.

# create table with only vars needed to calc standard scores
final_med_SD <- smooth_med_SD %>% select(agestrat, median_sm, lo_SD_sm, hi_SD_sm) %>% mutate_at(vars(agestrat), ~ paste0("mo_", .x))

# create empty look up table by binding column of all possible raw scores to set
# of columns holding numerical NAs, naming each column in set using agelabels
# charvec
raw_to_SS_lookup_empty <- bind_cols(
  enframe(min_raw:max_raw, name = NULL, value = 'rawscore'),
  data.frame(matrix(NA_real_, nrow = max_raw + 1, ncol = num_agestrat)) %>%
    set_colnames(final_med_SD$agestrat)
)

# fill empty raw-to-SS lookup with standard scores by age strat.
raw_to_SS_lookup <- raw_to_SS_lookup_empty %>%
  # gather collapses the empty wide rawscore by agestrat table into tall table
  # with three columns, rawscore, agestrat, and an empty SS column. Rawscore
  # sequence is repeated down its column, once per agestrat, and value of
  # agestrat column is uniform for the entire rawscore sequence. `-rawscore` is
  # `dplyr::select` code that drops all vars except rawscore.
  gather(agestrat, SS, -rawscore) %>%
  # This tall table can now be joined with `final_med_SD`, which contains
  # smoothed meds and SDs per agestrat, because both tables have an `agestrat`
  # column that can be used as a `by` var. In the newly-constituted tall table,
  # `median_sm`, `lo_SD_sm`, and `hi_SD_sm` cols hold the correct values for
  # each agestrat.
  left_join(final_med_SD, by = 'agestrat') %>%
  # calculate SS above and below the median. `meidan_SM = NULL` drops this now
  # unnecessary column from the piped object.
  mutate(SS = case_when(
    rawscore <= median_sm ~ round(100 + (((rawscore - median_sm) / lo_SD_sm) *15)),
    TRUE ~ round(100 + (((rawscore - median_sm) / hi_SD_sm) *15))
  ), median_sm = NULL, lo_SD_sm = NULL, hi_SD_sm = NULL) %>%
  # truncate SS distribution at 40 and 160.
  mutate_at(
    vars(SS), ~ case_when(
      .x < 40 ~ 40,
      .x > 160 ~ 160,
      TRUE ~ .x
    )
  ) %>%
  # spread converts table back from tall to wide. Resulting table has one row
  # per `rawscore`. Each value of `agestrat` gets its own column, and each of
  # these columns is populated with the value of `SS` that matches `rawscore`,
  # within that `agestrat`.
  spread(agestrat, SS) %>%
  # select reorders vars so to give the correct sequence of `agestrat` going
  # left-to-right. That order of agestrats is given by the char vec
  # `c(final_med_SD$agestrat)`.
  select(rawscore, c(final_med_SD$agestrat))
rm(raw_to_SS_lookup_empty, final_med_SD)

# write final table of smoothed medians, SDs to .csv
final_med_SD_table <- smooth_med_SD %>% 
  select(
    group, agestrat, lo_SD_sm, median_sm, hi_SD_sm
  ) %>% 
  rename(
    smoothed_lo_SD = lo_SD_sm, smoothed_median = median_sm, smoothed_hi_SD = hi_SD_sm
  ) %>% 
  mutate(
    ES = (smoothed_median - lag(smoothed_median))/((smoothed_hi_SD+lag(smoothed_hi_SD)+smoothed_lo_SD+lag(smoothed_lo_SD))/4)
  )

write_csv(final_med_SD_table, here(
  paste0(
    'OUTPUT-FILES/DESCRIPTIVE-TABLES/',
    score_name,
    '-final_med_SD_table-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))

# write final raw-to-SS lookup table to .csv
write_csv(raw_to_SS_lookup, here(
  paste0(
    'OUTPUT-FILES/FINAL-RAW-TO-SS-LOOKUP-TABLES/',
    score_name,
    '-raw-SS-lookup-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))

# extract agestrat labels for processing below
norms_names <- names(raw_to_SS_lookup)[-1]

norms_pub <- raw_to_SS_lookup %>% 
  # gather collapses wide table into three-column tall table with key-value
  # pairs: rawscore, agestrat(key var, many rows for each agestrat), SS(value
  # var, one row for each value of SS within each agestrat)
  gather(agestrat, SS,-rawscore) %>% 
  group_by(agestrat) %>%
  # expand the table vertically, adding new rows, so there's a row for every possible SS value
  complete(SS = 40:160) %>% 
  ungroup() %>%
  # regroup table by two levels
  group_by(agestrat, SS) %>%
  # filter step retains all 1-row groups, and the first and last rows of any
  # multi-row groups. n() == 1 returns 1-row groups; n() > 1 & row_number()
  # %in% c(1, n()) returns rows of multi-row groups with the row number of
  # either 1 (first row), or n() which is the number or rows and also the
  # number of the last row. The first and last rows hold the min and max
  # values of raw for that value of SS (the grouping variable)
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  # Summarise creates a table with one row per group (one row per
  # possible value of SS). For the 1-row groups, str_c simply passes the
  # value of raw as a string; for the multi-row groups, str_c joins the min
  # and max values of raw with the '=' separator.
  summarise(rawscore = str_c(rawscore, collapse = '--')) %>%
  # recode missing values of raw to '-'
  mutate_at(vars(rawscore), ~ case_when(is.na(.x) ~ '-', TRUE ~ .x)) %>%
  # sort on two levels
  arrange(agestrat, desc(SS)) %>% 
  # spread table back to wide, all values of SS (one row for each), agestrat
  # columns filled with values of rawscore
  spread(agestrat, rawscore) %>%
  # sort descending on SS
  arrange(desc(SS)) %>% 
  # apply desired final column names
  select(SS, norms_names)

# write final raw-to-SS lookup table to .csv
write_csv(norms_pub, here(
  paste0(
    'OUTPUT-FILES/FINAL-RAW-TO-SS-LOOKUP-TABLES/',
    score_name,
    '-raw-SS-lookup-print-table-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))


