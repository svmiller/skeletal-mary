library(tidyverse)
library(peacesciencer)


ICOW <- read_csv("~/Dropbox/data/icow/icow-provisional-1.01/ICOWprov101.csv")
ICOW01 <- read_csv("~/Dropbox/data/icow/icow-provisional-1.01/200199.csv")
ICOWyr <- read_csv("~/Dropbox/data/icow/icow-provisional-1.01/ICOWprovyr101.csv")

TTI <- readRDS("data-raw/TTI.rds")


create_dyadyears() %>% add_strategic_rivalries() %>% filter(between(year, 1816, 2010)) %>%
  mutate(spatial = ifelse(type1 == "spatial" | type2 == "spatial", 1, 0)) %>%
  add_cow_majors() %>% add_contiguity() %>%
  mutate(prd = case_when(
    conttype >= 1 ~ 1,
    conttype == 0 & cowmaj1 == 1 ~ 1,
    conttype == 0 & cowmaj2 == 1 ~ 1,
    TRUE ~ 0
  )) -> DDY

ICOW01 %>%
  select(claimdy, claim, adjend) %>%
  left_join(ICOW, .) %>%
  mutate(endclaim = ifelse(endclaim == "200199", adjend, endclaim)) %>%
  select(claim, tgt, chal, begclaim, endclaim) %>%
  mutate(styear = as.numeric(str_sub(begclaim, 1, 4)),
         endyear = as.numeric(str_sub(endclaim, 1, 4))) %>%
  # In this case: targets are ccode1. That's what we want.
  # The claim data are directed.
  rename(ccode1 = tgt,
         ccode2 = chal) %>%
  select(-begclaim, -endclaim) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  unnest(year) %>%
  select(-styear, -endyear) %>%
  # Let's get an indicator for number of targeted claims in a dyad.
  group_by(ccode1, ccode2, year) %>%
  mutate(tottgtclaim = n()) %>%
  ungroup() %>%
  mutate(tgtclaim = 1) %>%
  distinct() %>%
  left_join(DDY, .) %>%
  mutate_at(vars("tottgtclaim", "tgtclaim"), ~ifelse(is.na(.), 0, .)) -> DDY


noPRD <- list()

DDY %>% filter(spatial == 1 & prd == 0) %>% distinct(rivalryno) %>%
  semi_join(td_rivalries, .) -> noPRD$"spatial"



DDY %>% filter(tgtclaim == 1 & prd == 0) %>% distinct(ccode1, ccode2, claim)  %>%
  semi_join(ICOW %>% rename(ccode1 = tgt,
                            ccode2 = chal), .) %>%
  select(ccode1, ccode2, claim, name, begclaim, endclaim) %>% distinct() -> noPRD$"claim"

saveRDS(noPRD, "data/noPRD.rds")


ICOW01 %>%
  select(claimdy, claim, adjend) %>%
  left_join(ICOW, .) %>%
  mutate(endclaim = ifelse(endclaim == "200199", adjend, endclaim)) %>%
  select(claim, tgt, chal, begclaim, endclaim) %>%
  mutate(styear = as.numeric(str_sub(begclaim, 1, 4)),
         endyear = as.numeric(str_sub(endclaim, 1, 4))) %>%
  # In this case: targets are ccode1. That's what we want.
  # The claim data are directed.
  rename(ccode1 = tgt,
         ccode2 = chal) %>%
  select(-begclaim, -endclaim) %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  unnest(year) %>%
  select(-styear, -endyear) %>%
  group_by(ccode1, year) %>%
  summarize(numtgtclaims = n(),
            tgtclaim = ifelse(numtgtclaims >= 1, 1, 0)) %>%
  ungroup() %>% rename(ccode = ccode1) -> icowSY

create_stateyears() %>% filter(between(year, 1816, 2010)) %>% #attributes(.)
  left_join(., TTI %>% select(ccode, year, value) %>% rename(tti = value)) %>%
  add_strategic_rivalries() %>%
  left_join(., icowSY) %>%
  mutate_at(vars("numtgtclaims", "tgtclaim"), ~ifelse(is.na(.), 0, .)) -> dataCompare

saveRDS(dataCompare, "data/dataCompare.rds")
