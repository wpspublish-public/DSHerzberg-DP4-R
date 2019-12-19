suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

file_name <- c('TEMP_Norms_RawtoSS_InterviewForm_ITTables_DH SPECS')

# express the directory path to the input file as a string.
path <- here(
paste0('INPUT-FILES/', file_name, '.xlsx'))

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
  arrange(scale, agestrat) %>% 
  select(scale, agestrat, rawscore, SS)



