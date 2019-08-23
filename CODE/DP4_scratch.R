test <- full_join(
  eval(as.name(paste0(score_name, '_freq_agestrat'))),
  (
    eval(as.name(paste0(score_name, '_freq_agestrat'))) %>%
      group_by(agestrat) %>%
      summarise(min = min(cum_per)) %>%
      mutate(lo1 = case_when(
        min < 5 ~ 5,
        min < 10 ~ 10,
        min < 15 ~ 15,
        min < 20 ~ 20,
        TRUE ~ 25
      ))
  ),
  by = 'agestrat'
) %>%
  group_by(agestrat) %>% mutate(flag = case_when(cum_per > lo1 &
                                                   cum_per < 95 ~ 1,
                                                 TRUE ~ 0)) %>% filter(flag == 1) %>% summarise(min = min(cum_per),
                                                                                                max = max(cum_per),
                                                                                                lo1 = first(lo1)) %>%
  mutate(
    lo2 = case_when(
      lo1 == 5 & min < 10 ~ 10,
      lo1 == 5 & min < 15 ~ 15,
      lo1 == 5 & min < 20 ~ 20,
      lo1 == 5 & min >= 20 ~ 25,
      lo1 == 10 & min < 15 ~ 15,
      lo1 == 10 & min < 20 ~ 20,
      lo1 == 10 & min >= 20 ~ 25,
      lo1 == 15 & min < 20 ~ 20,
      lo1 == 15 & min >= 20 ~ 25,
      TRUE ~ 25
    ),
    hi1 = case_when(max > 90 ~ 90,
                    max > 85 ~ 85,
                    max > 80 ~ 80,
                    TRUE ~ 75),
    hi2 = 95
  ) %>% select(-min,-max) %>% 
  assign(paste0(score_name, '_age_lo1lo2_hi1hi2'), ., envir = .GlobalEnv)
