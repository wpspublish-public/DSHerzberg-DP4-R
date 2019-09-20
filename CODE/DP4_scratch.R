input_file_name <- c('TEACHER_ADP.csv')
suppressMessages(
  read_csv(
    here(
      paste0('INPUT-FILES/', input_file_name)
    )
  )
) %>% 
  assign(paste0(score_name, '_raw_by_agestrat'), ., envir = .GlobalEnv)

eval(as.name(paste0(score_name, '_raw_by_agestrat'))) %>% 
  left_join(
    final_med_SD_table,
    by = 'agestrat'
  ) %>% 
  assign(paste0(score_name, '_SS_per_case'), ., envir = .GlobalEnv)


df1 <- adpscore_teacher_SS_per_case %>% mutate(!!as.name(paste0(score_name, '_SS')) := case_when(
# df1 <- adpscore_teacher_SS_per_case %>% mutate(adpscore_teacher_SS = case_when(
  !!as.name(score_name) <= smoothed_median ~ round(100+(((!!as.name(score_name)-smoothed_median)/smoothed_lo_SD)*15), 0),
  TRUE ~ round(100+(((!!as.name(score_name)-smoothed_median)/smoothed_hi_SD)*15), 0)
))
