suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

file_name <- c('TEMP_Norms_RawtoSS_InterviewForm_ITTables_DH SPECS')

temp1 <- read_excel(here(
  paste0('INPUT-FILES/', file_name, '.xlsx')
), 30
)

path <- here(
paste0('INPUT-FILES/', file_name, '.xlsx'))

temp2 <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
      path = path,
      .id = 'agestrat')

temp3 <- temp2 %>% 
  gather('scale', 'SS', -agestrat, -rawscore) %>% 
  arrange(agestrat)

