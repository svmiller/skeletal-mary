library(tidyverse)
library(stevemisc)
library(modelsummary)

KK <- haven::read_dta("~/Dropbox/data/kangkim2020ttwl/Data.dta")
TTI <- readRDS("data-raw/TTI.rds") # %>% rename(ccode = ccode1)
TTIm <- readRDS("data-raw/TTIm.rds") # %>% rename(ccode = ccode1)


KK %>% filter(year >= 1985 & e_lexical_index == 6) %>%
  select(cowcode, year,Fv2lgfemleg, terrrivalry,nonterrrivalry, 
              PR, v2x_polyarchy, cedaw, lngdppc_full, prio_civilwar, postwar) %>%
  rename(ccode = cowcode) %>%
  left_join(., TTI %>% select(ccode, year, value) %>% rename(tti = value)) %>%
  left_join(., TTIm %>% select(ccode, year, value) %>% rename(ttim = value)) %>%
  na.omit %>% 
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  r2sd_at(c("v2x_polyarchy", "lngdppc_full", "tti", "ttim")) %>%
  mutate_at(vars("v2x_polyarchy", "lngdppc_full", "tti", "ttim"), ~. - mean(., na.rm=TRUE)) -> dataKK

saveRDS(dataKK, "data/dataKK.rds")


library(fixest)

modKK <- list()

modKK$"Model 1" <- feols(Fv2lgfemleg ~ terrrivalry + nonterrrivalry + PR + 
                           v2x_polyarchy + cedaw + lngdppc_full + prio_civilwar + 
                           postwar,  cluster = "ccode", fixef="ccode", data=dataKK)

modKK$"Model 2" <- feols(Fv2lgfemleg ~ tti + nonterrrivalry + PR + 
                           v2x_polyarchy + cedaw + lngdppc_full + prio_civilwar + 
                           postwar,  cluster = "ccode", fixef="ccode", data=dataKK)

saveRDS(modKK, "data/modKK.rds")