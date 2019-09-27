suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))

file_names <- list.files(
  path = here(
    'INPUT-FILES/GDS'
    )
  )
list <- map(
  file_names,
  ~ suppressMessages(read_csv(
    here(
      paste0(
        'INPUT-FILES/GDS/', 
        .x)
      )
    )
  )
)
names(list) <-   c("DP4_interview_ADP_SS", "DP4_interview_COG_SS", "DP4_interview_COM_SS",
                   "DP4_interview_PHY_SS", "DP4_interview_SOC_SS", "DP4_teacher_ADP_SS",  
                   "DP4_teacher_COG_SS", "DP4_teacher_COM_SS", "DP4_teacher_PHY_SS",  
                   "DP4_teacher_SOC_SS")
list2env(
  list,
  envir=.GlobalEnv
  )
rm(list)

teacher_SS_list <- list(
  DP4_teacher_ADP_SS, 
  DP4_teacher_COG_SS, 
  DP4_teacher_COM_SS, 
  DP4_teacher_PHY_SS,
  DP4_teacher_SOC_SS)

teacher_SS <- teacher_SS_list %>% 
  reduce(
    left_join, 
    by = "ID"
    ) %>% 
  select(ID, 
         agestrat, 
         contains('score'), 
         contains('SS'), 
         -contains('.')
         ) %>% 
  mutate(
    GDS_teacher = rowSums(select(., contains("SS")))
  )
rm(teacher_SS_list)

teacher_SS %>% select(
  ID,
  agestrat,
  GDS_teacher
  ) %>% 
  write_csv(here(
    'INPUT-FILES/TEACHER_norms-format_GDS.csv'
  )
)

