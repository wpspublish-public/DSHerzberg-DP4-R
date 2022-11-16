suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

form <- c('parent', 'teacher', 'self')
scale_acr <- c('COG', 'EMO')
CV <- c('90', '95')

scale_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('DOCUMENTATION/DEMO-INPUT-TABLES/', x, '.xlsx')) %>%
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
    assign('input_lookup', ., envir = .GlobalEnv)
}

scale_lookup_pre <- form %>% 
  map(scale_readin) %>% 
  setNames(form) %>% 
  bind_rows(.id = 'form')

# Read in CV .xlsx
CV_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('DOCUMENTATION/DEMO-INPUT-TABLES/CV', x, '.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)
  
  
  # input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
  # agestrat. read input file into a df, stacking tabs on top of one another, and
  # creating a new column 'agestrat' to identify the origin tab of each set of rows.
  path %>% 
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path,
           .id = 'form') %>% 
    assign('CV_lookup', ., envir = .GlobalEnv)
}

CV_lookup_pre <- CV %>% 
  map(CV_readin) %>% 
  setNames(CV) %>% 
  reduce(full_join, by = c('form', 'agestrat'))



