ageEquiv_lookup <- here('INPUT-FILES/OES-TABLES/ageEquiv-form-lookup.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/OES-TABLES/ageEquiv-form-lookup.xlsx'),
         .id = 'form') %>% 
  rename_at(vars(PHY:COM), ~ str_c(.x, '_AE'))
