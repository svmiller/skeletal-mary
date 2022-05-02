# Recreate Table 5.1 in Gibler (2012).
#DV: number of military personnel (change from prev yr).
#IV: territorial threat (lag, 1) 
# Controls: confrontation onset in year, state is a major power, total state population, change in milex from prevyr, lag of military personnel-->

# https://strengejacke.github.io/mixed-models-snippets/random-effects-within-between-effects-model.html#comparison-fe--and-rewb-model

library(tidyverse)
library(stevemisc)
library(modelsummary)
library(peacesciencer)

TTI <- readRDS("data-raw/TTI.rds") # %>% rename(ccode = ccode1)
TTIm <- readRDS("data-raw/TTIm.rds") # %>% rename(ccode = ccode1)



create_stateyears() %>%
  filter(between(year, 1975, 2010)) %>%
  add_nmc() %>%
  add_sdp_gdp() %>%
  add_democracy() %>%
  mutate(milit = milper/tpop,
         cw = ifelse(year <= 1989, 1, 0)) %>%
  select(-statenme) -> Data

DPI <- haven::read_dta("~/Dropbox/data/DPI/DPI2020/DPI2020.dta") %>% 
  select(gwno, 1:3, checks, checks_lax, stabs_strict, stabs, stabns, stabns_strict,  polariz) %>%
  filter(year <= 2010)



DPI %>% # gvi(polariz)
  mutate_at(vars(checks:polariz), ~ifelse(. == -999, NA, .)) %>%
  filter(gwno != -999) %>%
  mutate(ccode = countrycode::countrycode(gwno, "gwn", "cown"),
         ccode = ifelse(gwno == 816, 816, ccode),
         ccode = ifelse(gwno == 817, 817, ccode)) -> DPI


DPI %>%
  left_join(Data, .) -> Data

TTI %>%
  select(ccode, year, value) %>%
  # mutate(year = year +1) %>%
  left_join(Data, .) -> Data


TTIm %>%
  select(ccode, year, value) %>%
  rename(m_value = value) %>%
  # mutate(year = year +1) %>%
  left_join(Data, .) -> Data



Data %>%
  mutate(checkst = ifelse(checks >= 10, 10, checks)) %>%
  group_by(ccode) %>%
  mutate(c_tti = lag(value, 1) - mean(value),
         c_ttim = lag(m_value, 1) - mean(m_value),
         c_polariz = lag(polariz, 1) - mean(polariz),
         c_wbgdppc2011est = lag(wbgdppc2011est, 1) - mean(wbgdppc2011est),
         c_milit = lag(milit, 1) - mean(milit),
         c_polity2 = lag(polity2, 1) - mean(polity2),
         c_xm_qudsest = xm_qudsest - mean(xm_qudsest)) %>%
  ungroup() -> Data

dataChecks <- Data

saveRDS(dataChecks, "data/dataChecks.rds")

library(fixest)

modChecks <- list()


modChecks$"Model 1" <- feols(checks ~ c_tti, fixef = "ccode", data=subset(dataChecks))

modChecks$"Model 1" <- feols(checks ~ c_tti + c_polariz + c_wbgdppc2011est + 
                                  c_milit + c_polity2 , fixef = "ccode", data=subset(dataChecks))


saveRDS(modChecks, "data/modChecks.rds")
             
