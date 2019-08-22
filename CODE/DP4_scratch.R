norms_edit_names <- c('group', 'agestrat', 'lo_SD_sm', 'median_sm', 'hi_SD_sm', 'ES_sm')
smooth_med_SD <- read_csv(here('OUTPUT-FILES/DESCRIPTIVE-TABLES/PHYscore-final_med_SD_table-2019-08-18.csv'))
names(smooth_med_SD) <- norms_edit_names
smooth_med_SD <- smooth_med_SD %>% select(group, agestrat, lo_SD_sm, hi_SD_sm, median_sm, ES_sm)
