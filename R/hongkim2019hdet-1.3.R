library(tidyverse)
library(stevemisc)
# library(fixest)

HK <- haven::read_dta("~/Dropbox/data/hongkim2019hdet/data.dta")
TTI <- readRDS("data-raw/TTI.rds")
TTIm <- readRDS("data-raw/TTIm.rds")
Raw <- readRDS("data-raw/rawTTI.rds")

HK %>%
  select(cowcode, year, mkonset, lelc_eliti, lterrrivalry, postcold,#  lterrr_elc,  
         lprio_civilwar, lprio_civilwar, Lrsh_war, le_migdppcln, lv2x_libdem, ethfrac, styear, styear2, styear3, statefail) %>%
  rename(ccode = cowcode) -> Dat


Dat %>% filter(statefail ==1) %>% na.omit -> Dat



TTI %>%
  select(ccode, year, value) %>%
  rename(tti = value) %>%
  mutate(year = year + 1) %>%
  left_join(Dat, .) %>%
  mutate(z_tti = r2sd(tti)) -> Dat


TTIm %>%
  select(ccode, year, value) %>%
  rename(ttim = value) %>%
  mutate(year = year + 1) %>%
  left_join(Dat, .) %>%
  mutate(z_ttim = r2sd(ttim)) -> Dat



Raw %>%
  filter(year >= 1945) %>%
  group_by(ccode1, year) %>%
  mutate(n_spatial = sum(spatial),
         n_claim = sum(tgtclaim)) %>% 
  ungroup() %>%
  select(ccode1, year, n_spatial, n_claim) %>%
  distinct() %>%
  mutate(spatial = ifelse(n_spatial > 0, 1, 0),
         tgtclaim = ifelse(n_claim > 0, 1, 0)) %>%
  select(-n_spatial, -n_claim) %>%
  rename(ccode = ccode1) %>%
  left_join(Dat, .) -> Dat

Dat %>% r2sd_at(c("le_migdppcln", "lv2x_libdem", "ethfrac", "styear", "styear2", "styear3")) -> Dat

saveRDS(Dat, "data/dataHK.rds")

modHK <- list()

modHK$"Model 1" <- glm(mkonset ~ lelc_eliti*lterrrivalry + postcold +
                             lprio_civilwar + lprio_civilwar + Lrsh_war + le_migdppcln + lv2x_libdem + ethfrac + 
                         styear + I(styear^2) + I(styear^3),
                           data=subset(Dat, statefail == 1), family=binomial(link="logit"))


modHK$"Model 2" <- glm(mkonset ~ lelc_eliti*z_tti + postcold +
                         lprio_civilwar + lprio_civilwar + Lrsh_war + le_migdppcln + lv2x_libdem + ethfrac + 
                         styear + I(styear^2) + I(styear^3),
                       data=subset(Dat, statefail == 1), family=binomial(link="logit"))


saveRDS(modHK, "data/modHK.rds")

library(stevemisc)
library(modelr)


simsHK <- list()

Dat %>%
  data_grid(.model = modHK[[1]], lelc_eliti = c(0,1), lterrrivalry = c(0, 1), mkonset = 0) -> newdatM1


simsM1 <- get_sims(modHK[[1]], newdatM1, 1000, 8675309)

newdatM1 %>%
  slice(rep(row_number(), 1000)) %>%
  bind_cols(simsM1, .) %>%
  mutate(y = plogis(y)) -> simsM1

simsHK$"Base Sims" <- simsM1


Dat %>%
  data_grid(.model = modHK[[2]], lelc_eliti = c(0,1), z_tti = c(0,1), mkonset = 0) -> newdatM2

simsM2 <- get_sims(modHK[[2]], newdatM2, 1000, 8675309)

newdatM2 %>%
  slice(rep(row_number(), 1000)) %>%
  bind_cols(simsM2, .) %>%
  mutate(y = plogis(y)) -> simsM2

simsHK$"TTI Sims [0,1]" <- simsM2

Dat %>%
  data_grid(.model = modHK[[2]], lelc_eliti = c(0,1), z_tti = seq_range(z_tti, 100), mkonset = 0) -> newdatM2


simsM2 <- get_sims(modHK[[2]], newdatM2, 1000, 8675309)

newdatM2 %>%
  slice(rep(row_number(), 1000)) %>%
  bind_cols(simsM2, .) %>%
  mutate(y = plogis(y)) -> simsM2

simsHK$"TTI Sims (Range)" <- simsM2

saveRDS(simsHK, "data/simsHK.rds")