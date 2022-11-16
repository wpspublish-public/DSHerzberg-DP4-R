# Generate norms for test scores with no age-stratification in the input sample..

suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(magrittr) # PIPE OPERATORS
library(splitstackshape)
suppressMessages(suppressWarnings(library(tidyverse)))

input_parameters_prompt <- function() {
  writeLines(
    "To initiate the norming process, R requires certain input parameters. Please choose a method for entering these parameters:\n\n1: Interactively at the console\n2: From a file named 'Input-Parameters.R' located in '[PROJECT DIRECTORY]/INPUT-FILES'\n"
  )
  input_choice <- as.numeric(0)
  while (is.na(input_choice) || (!(input_choice %in% 1:2))) {
    input_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(input_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (input_choice == 1) {
        cat("Enter the name of the input file, including the .csv suffix.\nUse exact spelling and capitalization.")
        suppressWarnings(as.character(
          readline(prompt = "Input file: ")
        )) %>% assign('input_file_name', ., envir = .GlobalEnv)
        cat("\nEnter the column name of the score to be normed.\nUse exact spelling and capitalization.")
        suppressWarnings(as.character(
          readline(prompt = "Score name: ")
        )) %>% assign('score_name', ., envir = .GlobalEnv)
        cat("\nEnter the HIGHEST possible raw score for the score to be normed.")
        suppressWarnings(as.integer(
          readline(prompt = "Maximum raw score: ")
        )) %>% assign('max_raw', ., envir = .GlobalEnv)
        cat("\nEnter the LOWEST possible raw score for the score to be normed.")
        suppressWarnings(as.integer(
          readline(prompt = "Minimum raw score: ")
        )) %>% assign('min_raw', ., envir = .GlobalEnv)
        break
      } else if (input_choice == 2) {
        source(here('INPUT-FILES/Input-Parameters.R'))
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
input_parameters_prompt()

suppressMessages(
  read_csv(
    here(
      paste0('INPUT-FILES/', input_file_name)
    )
  )
) %>% 
  assign(paste0(score_name, '_raw'), ., envir = .GlobalEnv)

eval(as.name(paste0(score_name, '_raw'))) %>% count(!!as.name(score_name)) %>%
  mutate(
    perc = round(100 * (n / sum(n)), 4),
    cum_per = round(100 * (cumsum(n) / sum(n)), 4),
    lag_tot = lag(!!as.name(score_name)),
    lag_cum_per = lag(cum_per)
  ) %>%
  assign(paste0(score_name, '_freq'), ., envir = .GlobalEnv)

eval(as.name(paste0(score_name, '_raw'))) %>% summarise(
  n = n(),
  median = round(median(eval(
    as.name(score_name)
  )), 2),
  mean = round(mean(eval(
    as.name(score_name)
  )), 2),
  sd = round(sd(eval(
    as.name(score_name)
  )), 2)
) %>%
  assign(paste0(score_name, '_desc'), ., envir = .GlobalEnv)

# write raw score descriptives to .csv
write_csv(eval(as.name(paste0(score_name, '_desc'))), here(
  paste0(
    'OUTPUT-FILES/DESCRIPTIVE-TABLES/',
    score_name,
    '-descriptives-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))

# write raw score freq counts to .csv
freq_csv <- eval(as.name(paste0(score_name, '_freq'))) %>% select(-lag_tot, -lag_cum_per)
write_csv(freq_csv, here(
  paste0(
    'OUTPUT-FILES/DESCRIPTIVE-TABLES/',
    score_name,
    '-frequencies-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))
rm(freq_csv)


# create empty look up table containing all possible raw scores
raw_to_SS_lookup_empty <- enframe(
  min_raw:max_raw, 
  name = NULL, 
  value = 'rawscore'
  )

# create table of means, SDs with same nrow as empty lookup table.
mean_sd_cols <- expandRows(
  eval(as.name(paste0(score_name, '_desc'))),
  count=nrow(raw_to_SS_lookup_empty), 
  count.is.col=FALSE) %>% select(mean, sd)


# fill empty raw-to-SS lookup with standard scores.
raw_to_SS_lookup <- raw_to_SS_lookup_empty %>% 
  bind_cols(mean_sd_cols) %>%
  mutate(!!as.name(paste0(score_name, '_SS')) := round(100 + (((rawscore - mean) / sd) *15))) %>%
  # truncate SS distribution at 40 and 160.
  mutate_at(
    vars(!!as.name(paste0(score_name, '_SS'))), ~ case_when(
      .x < 40 ~ 40,
      .x > 160 ~ 160,
      TRUE ~ .x
    )
  ) %>%
  select(rawscore, !!as.name(paste0(score_name, '_SS')))
rm(raw_to_SS_lookup_empty, mean_sd_cols)

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

norms_pub <- raw_to_SS_lookup %>% 
  select(!!as.name(paste0(score_name, '_SS')), rawscore) %>% 
  group_by(!!as.name(paste0(score_name, '_SS'))) %>%
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
  # sort descending on SS
  arrange(desc(!!as.name(paste0(score_name, '_SS')))) 

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

# generate SS per case and write data file to .csv
mean_sd_cols <- expandRows(
  eval(as.name(paste0(score_name, '_desc'))),
  count=nrow(eval(as.name(paste0(score_name, '_raw')))), 
  count.is.col=FALSE) %>% select(mean, sd)

eval(as.name(paste0(score_name, '_raw'))) %>% 
  bind_cols(mean_sd_cols) %>% 
  mutate(!!as.name(paste0(score_name, '_SS')) := round(100 + (((!!as.name(score_name) - mean) / sd) *15))) %>%
  select(
    -mean, -sd
  ) %>% 
  assign('SS_per_case', ., envir = .GlobalEnv)

write_csv(SS_per_case, here(
  paste0(
    'OUTPUT-FILES/SS-PER-CASE/',
    score_name,
    '-SS-per-case-',
    format(Sys.Date(), "%Y-%m-%d"),
    '.csv'
  )
))


