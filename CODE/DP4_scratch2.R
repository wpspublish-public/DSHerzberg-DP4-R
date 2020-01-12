suppressMessages(library(tidyverse))

input <- tribble(
~age, ~score, ~COG_SS, ~COG_CI, ~EMO_SS, ~EMO_CI,
1, 10, 98, '93-103', 100, '95-105',
1, 15, 107, '102-112', 109, '104-114',
1, 20, 114, '109-119', 118, '113-123',
2, 10, 94, '89-99', 97, '92-102',
2, 15, 103, '98-108', 103, '98-108',
2, 20, 109, '104-114', 114, '109-119'
)

out_temp <- input %>%
  gather(key, val, 3:ncol(.)) %>%
  extract(key, into = c("scale", "type"), "([:alpha:]{3})?\\_?(.*)") %>%
  spread(type, val) %>% 
  select(scale, age, score, SS, CI) %>% 
  arrange(scale) %>% 
  mutate(SS = as.numeric(SS))

output <- tribble(
  ~scale, ~age, ~score, ~SS, ~CI,
  'COG', 1, 10, 98, '93-103',
  'COG', 1, 15, 107, '102-112',
  'COG', 1, 20, 114, '109-119',
  'COG', 2, 10, 94, '89-99',
  'COG', 2, 15, 103, '98-108',
  'COG', 2, 20, 109, '104-114',
  'EMO', 1, 10, 100, '95-105',
  'EMO', 1, 15, 109, '104-114',
  'EMO', 1, 20, 118, '113-123',
  'EMO', 2, 10, 97, '92-102',
  'EMO', 2, 15, 103, '98-108',
  'EMO', 2, 20, 114, '109-119'
)

set.seed(1238289)
testdf=data.frame(FY=c("FY13","FY14","FY15","FY14","FY15","FY13","FY14","FY15","FY13","FY15","FY13","FY14","FY15","FY13","FY14","FY15"),
                  Region=c(rep("AFRICA",5),rep("ASIA",5),rep("AMERICA",6)),
                  QST=c(rep("Q2",3),rep("Q5",2),rep("Q2",3),rep("Q5",2),rep("Q2",3),rep("Q5",3)),
                  Very.Satisfied=runif(16,min = 0, max=1),
                  Total.Very.Satisfied=floor(runif(16,min=10,max=120)),
                  Satisfied=runif(16,min = 0, max=1),
                  Total.Satisfied=floor(runif(16,min=10,max=120)),
                  Dissatisfied=runif(16,min = 0, max=1),
                  Total.Dissatisfied=floor(runif(16,min=10,max=120)),
                  Very.Dissatisfied=runif(16,min = 0, max=1),
                  Total.Very.Dissatisfied=floor(runif(16,min=10,max=120))
)

tesdf_out <- testdf %>%
  gather(key, val, 4:ncol(.)) %>%
  extract(key, into = c("key1", "Level"), "(Total)?\\.?(.*)") %>%
  mutate(key1 = replace_na(key1, "Score")) %>%
  spread(key1, val)

