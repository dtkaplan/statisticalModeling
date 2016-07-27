# Making birth weight from mosaicData::Gestation.

require(rpart)
require(rpart.plot)
require(dplyr)
require(mosaicData)
Birth_weight <- 
  mosaicData::Gestation %>%
  select(baby_wt = wt, 
         income = inc, 
         mother_age = age, smoke, gestation, mother_wt = wt.1 ) %>%
  filter(smoke < 2) %>% 
  na.omit() %>%
  mutate(smoke = ifelse(smoke == 1, "smoker", "nonsmoker"),
         income = paste0("level_", income))