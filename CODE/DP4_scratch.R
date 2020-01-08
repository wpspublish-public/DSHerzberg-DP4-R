suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

form <- c('interview', 'parent', 'teacher')

# Create char vec holding names of input .xlsx containing scale lookups.
# `purrr::map_chr()` returns a char vec. Mapping `paste0()` allows you to paste
# the names of the three forms into the file name stem, creating charvec with
# three file names.
scale_file_name <- map_chr(form, ~ paste0('scale_lookup_', .x))

# read in percentile lookup column
perc_lookup <- suppressMessages(read_csv(here('INPUT-FILES/Percentile-Lookup-SS.csv')))

# read in age labels for OES output lookup table
age_labels <- suppressMessages(read_csv(here('INPUT-FILES/OES-TABLES/OES-age-labels.csv'))) %>%
  mutate(agestrat = str_sub(agestrat, 4))

scale_readin <- function(x) {
  # express the directory path to the input file as a string.
  here(
    paste0('INPUT-FILES/OES-TABLES/', x, '.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)
  
  
  # input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
  # agestrat. read input file into a df, stacking tabs on top of one another, and
  # creating a new column 'agestrat' to identify the origin tab of each set of rows.
  path %>% 
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path,
           .id = 'agestrat') %>% 
  # recode agestrat so that it will sort properly
  mutate(agestrat = str_sub(agestrat, 4) %>% 
                                 str_pad(3, side = 'left', '0')) %>% 
    assign('input_lookup', ., envir = .GlobalEnv)
}

scale_lookup <- scale_file_name %>% 
  map(scale_readin) %>% 
  setNames(form) %>% 
  bind_rows(.id = 'form')

# Read in CV .xlsx
CV_lookup <- here('INPUT-FILES/OES-TABLES/Form-Agestrat-CV.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/OES-TABLES/Form-Agestrat-CV.xlsx'),
         .id = 'form') %>% 
  mutate(agestrat = str_sub(agestrat, 4) %>% 
           str_pad(3, side = 'left', '0'), 
         CV_type = str_sub(form, -4),
         form = tolower(form),
         form = str_sub(form, 1, -6)) 
CV_90_lookup <- CV_lookup %>% 
  filter(CV_type == "CV90") %>% 
  rename(
    ADP_CV90 = ADP,
    COG_CV90 = COG,
    COM_CV90 = COM,
    PHY_CV90 = PHY,
    SOC_CV90 = SOC
  ) %>%
  select(-CV_type)
CV_95_lookup <- CV_lookup %>% 
  filter(CV_type == "CV95") %>% 
  rename(
    ADP_CV95 = ADP,
    COG_CV95 = COG,
    COM_CV95 = COM,
    PHY_CV95 = PHY,
    SOC_CV95 = SOC
  ) %>%
  select(-CV_type)

scale_CV_lookup <- list(scale_lookup, CV_90_lookup, CV_95_lookup) %>% 
  reduce(left_join, by = c('form', 'agestrat')) %>% 
  # now write function to create CV variables.



# Read in GDS .xlsx, using same general method, but without requiring a function
GDS_lookup <- here('INPUT-FILES/OES-TABLES/GDS_lookup.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/OES-TABLES/GDS_lookup.xlsx'),
         .id = 'form') %>% 
  # to nest form-rawscore-GDS triplets within values of agestrat, tidyr::crossing is used
  # because inputs have no common vars
  crossing(age_labels, .) %>% 
  filter(!(form == 'teacher' & agestrat %in% c("000", "002", "004", "006", "008", 
                         "010", "012", "014", "016", "018",
                         "020", "022"))) %>% 
  select(form, rawscore, GDS, agestrat)



# Assemble OES output table
scale_GDS_lookup <- scale_lookup %>% 
  full_join(GDS_lookup, by = c('agestrat', 'form', 'rawscore')) %>% 
  gather('scale', 'SS', -agestrat, -rawscore, -form) %>% 
  # This drops rows that are NA on SS, which shouldn't exist on final output table.
  drop_na() %>% 
  left_join(perc_lookup, by = 'SS') %>% 
  mutate(descrange = case_when(
    SS >= 131 ~ 'Well above average',
    between(SS, 116, 130) ~ 'Above average',
    between(SS, 85, 115) ~ 'Average',
    between(SS, 70, 84) ~ 'Below average',
    SS <= 69 ~ 'Delayed',
    TRUE ~ NA_character_
  )) %>% 
  select(scale, form, agestrat, rawscore, SS, descrange, Percentile) %>% 
  arrange(match(scale, c('PHY', 'ADP', 'SOC', 'COG', 'COM', 'GDS')), form, agestrat) 


