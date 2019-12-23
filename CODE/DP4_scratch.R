suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

file_name <- c('Norms_RawtoSS_InterviewForm_ITTables_DH SPECS')

perc_lookup <- suppressMessages(read_csv(here('INPUT-FILES/Percentile-Lookup-SS.csv')))

# express the directory path to the input file as a string.
path <- here(
paste0('INPUT-FILES/OES-TABLES/', file_name, '.xlsx'))

# input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
# agestrat. read input file into a df, stacking tabs on top of one another, and
# creating a new column 'agestrat' to identify the origin tab of each set of rows.
temp2 <- path %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
      path = path,
      .id = 'agestrat')

# recode agestrat so that it will sort properly
str_sub(temp2$agestrat, 4)  <- str_sub(temp2$agestrat, 4) %>% 
  str_pad(3, side = 'left', '0') 

# Transform input table so that subscales and their associated raw-to-SS lookups
# are nested within each value of agestrat.
temp3 <- temp2 %>% 
  gather('scale', 'SS', -agestrat, -rawscore) %>%
  right_join(perc_lookup, by = 'SS') %>% 
  arrange(scale, agestrat) %>% 
  mutate(descrange = case_when(
    SS >= 131 ~ 'Well above average',
    between(SS, 116, 130) ~ 'Above average',
    between(SS, 85, 115) ~ 'Average',
    between(SS, 70, 84) ~ 'Below average',
    SS <= 69 ~ 'Delayed',
    TRUE ~ NA_character_
  )) %>% 
  select(scale, agestrat, rawscore, SS, descrange, Percentile)
  
