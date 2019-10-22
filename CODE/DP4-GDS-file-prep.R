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
                   "DP4_interview_PHY_SS", "DP4_interview_SOC_SS", "DP4_parent_ADP_SS", 
                   "DP4_parent_COG_SS", "DP4_parent_COM_SS",
                   "DP4_parent_PHY_SS", "DP4_parent_SOC_SS", "DP4_teacher_ADP_SS",  
                   "DP4_teacher_COG_SS", "DP4_teacher_COM_SS", "DP4_teacher_PHY_SS",  
                   "DP4_teacher_SOC_SS")
list2env(
  list,
  envir=.GlobalEnv
  )
rm(list)

parent_SS_list <- list(
  DP4_parent_ADP_SS, 
  DP4_parent_COG_SS, 
  DP4_parent_COM_SS, 
  DP4_parent_PHY_SS,
  DP4_parent_SOC_SS)

parent_SS <- parent_SS_list %>% 
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
    GDS_parent = rowSums(select(., contains("SS")))
  ) %>% 
  filter(
    !is.na(GDS_parent)
  )
rm(parent_SS_list)

parent_SS %>% select(
  ID,
  agestrat,
  GDS_parent
  ) %>% 
  write_csv(here(
    'INPUT-FILES/PARENT_norms-format_GDS.csv'
  )
)

