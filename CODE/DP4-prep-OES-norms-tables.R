suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

file_name <- c('Norms_RawtoSS_InterviewForm_ITTables_DH SPECS',
               'Norms_RawtoSS_ParentChecklist_ITTables_DH SPECS',
               'Norms_RawtoSS_TeacherNorms_ITTables_DH SPECS')

perc_lookup <- suppressMessages(read_csv(here('INPUT-FILES/Percentile-Lookup-SS.csv')))

lookup <- function(x) {
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
    assign('input_lookup_pre', ., envir = .GlobalEnv)
  
# recode agestrat so that it will sort properly
input_lookup_pre %>%  mutate(agestrat_x = str_sub(agestrat, 4) %>% 
    str_pad(3, side = 'left', '0')) %>% 
   select(-agestrat) %>% 
   rename(agestrat = agestrat_x) %>% 
  assign('input_lookup', ., envir = .GlobalEnv)

# erstatz GDS lookup
tribble(
    ~rawscore, ~GDS,
    500, 98,
    501, 99,
    502, 100,
    503, 101,
    504, 102
  ) %>% 
    assign('input_GDS', ., envir = .GlobalEnv)
  
  # extract agestrat values into df
  unique(input_lookup$agestrat) %>% 
    assign('agestrat', ., envir = .GlobalEnv)
  
  enframe(agestrat) %>% 
    select(value) %>% 
    rename(agestrat = value) %>% 
    assign('agestrat1', ., envir = .GlobalEnv)
  
  # new df that nests input_GDS within values of agestrat, tidyr::crossing is used
  # because inputs have now common vars
  agestrat_GDS <- crossing(agestrat1, input_GDS) %>% 
    assign('agestrat_GDS', ., envir = .GlobalEnv)
  
  # Transform input table so that subscales and their associated raw-to-SS lookups
  # are nested within each value of agestrat.
  input_lookup %>% 
    full_join(agestrat_GDS, by = c('agestrat', 'rawscore')) %>% 
    gather('scale', 'SS', -agestrat, -rawscore) %>%
    right_join(perc_lookup, by = 'SS') %>% 
    arrange(match(scale, c('PHY', 'ADP', 'SOC', 'COG', 'COM', 'GDS')), agestrat) %>% 
    mutate(descrange = case_when(
      SS >= 131 ~ 'Well above average',
      between(SS, 116, 130) ~ 'Above average',
      between(SS, 85, 115) ~ 'Average',
      between(SS, 70, 84) ~ 'Below average',
      SS <= 69 ~ 'Delayed',
      TRUE ~ NA_character_
    )) %>% 
    select(scale, agestrat, rawscore, SS, descrange, Percentile)
}

form <- c('interview', 'parent', 'teacher')

lookup_list <- file_name %>% 
  map(lookup) %>% 
  setNames(form)

interview_lookup <- lookup_list[[1]] %>% 
  mutate(form = 'interview') %>%
  select(form, everything()) 
parent_lookup <- lookup_list[[2]] %>% 
  mutate(form = 'parent') %>%
  select(form, everything()) 
teacher_lookup <- lookup_list[[3]] %>% 
  mutate(form = 'teacher') %>%
  select(form, everything()) 

all_lookup <- bind_rows(interview_lookup, parent_lookup, teacher_lookup)
