# Recreate Table 5.1 in Gibler (2012).
#DV: number of military personnel (change from prev yr).
#IV: territorial threat (lag, 1) 
# Controls: confrontation onset in year, state is a major power, total state population, change in milex from prevyr, lag of military personnel-->

library(tidyverse)
library(stevemisc)
library(modelsummary)
library(peacesciencer)

TTI <- readRDS("data-raw/TTI.rds") # %>% rename(ccode = ccode1)
TTIm <- readRDS("data-raw/TTIm.rds") # %>% rename(ccode = ccode1)


States <- read_csv("~/Dropbox/data/cow/states/states2016.csv")
Maj <- read_csv("~/Dropbox/data/cow/states/majors2016.csv")

NMC <- read_csv("~/Dropbox/data/cow/nmc/NMC_5_0.csv")

Part <- read_csv("~/Dropbox/projects/mid-project/mic-data/1.0/mic-part-1.0.csv")

create_stateyears() %>%
  add_nmc() %>%
  add_cow_majors() -> Data

TTI %>%
  select(ccode, year, value) %>%
  rename(tti = value) %>%
  group_by(ccode) %>%
  mutate(ttidiff = tti - lag(tti, 1)) %>%
  ungroup() %>%
  mutate(year = year + 1) %>% # quick and dirty lag
  left_join(Data, .) -> Data

TTIm %>%
  select(ccode, year, value) %>%
  rename(ttim = value) %>%
  group_by(ccode) %>%
  mutate(ttimdiff = ttim - lag(ttim, 1)) %>%
  ungroup() %>%
  mutate(year = year + 1) %>% # quick and dirty lag
  left_join(Data, .) -> Data


Part %>%
  select(ccode, styear) %>%
  group_by(ccode, styear) %>%
  mutate(n_onset = n()) %>%
  ungroup() %>%
  rename(year = styear) %>%
  left_join(Data, .) %>%
  mutate(n_onset = ifelse(is.na(n_onset), 0, n_onset),
         confonset = ifelse(n_onset >= 1, 1, 0)) -> Data

Data %>%
  group_by(ccode) %>%
  mutate(milperdiff = milper - lag(milper, 1),
         milexdiff = milex - lag(milex, 1),
         l1_milper = lag(milper, 1)) %>%
  ungroup() -> Data

Data %>%
  mutate(z_tti = r2sd(tti),
         z_ttidiff = r2sd(ttidiff),
         z_ttim = r2sd(ttim),
         z_ttimdiff = r2sd(ttimdiff),
         z_milperdiff = r2sd(milperdiff),
         z_milexdiff = r2sd(milexdiff),
         z_l1_milper = r2sd(l1_milper),
         z_tpop = r2sd(tpop)) -> Data


saveRDS(Data, "data/dataMilper.rds")

library(lmerTest)
# library(fixest)

modMilper <- list()

modMilper$"Model 1" <- lmerTest::lmer(milperdiff ~ z_tti + cowmaj + confonset + z_tpop + z_milexdiff + z_l1_milper +  (1 | ccode),
                                    data = Data)

modMilper$"Model 2" <- lmerTest::lmer(milperdiff ~ z_ttidiff + cowmaj + confonset + z_tpop + z_milexdiff + z_l1_milper +  (1 | ccode),
                                            data = Data)


saveRDS(modMilper, "data/modMilper.rds")