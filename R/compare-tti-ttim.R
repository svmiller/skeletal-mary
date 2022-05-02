library(tidyverse)

allrds <- list.files(pattern = ".rds", path = "data") %>%
  stringr::str_remove(., ".rds")

for(i in allrds) {
  filepath <- file.path(paste0("data/",i,".rds"))
  assign(i, readRDS(filepath))
}


M1 <- lmerTest::lmer(milperdiff ~ z_ttim + cowmaj + confonset + z_tpop + z_milexdiff + z_l1_milper +  (1 | ccode),
                                      data = dataMilper)

M2 <- lmerTest::lmer(milperdiff ~ z_ttimdiff + cowmaj + confonset + z_tpop + z_milexdiff + z_l1_milper +  (1 | ccode),
                                      data = dataMilper)



broom::tidy(M1) %>% bind_rows(.,broom::tidy(M2)) %>%
  filter(term %in% c("z_ttim", "z_ttimdiff")) %>%
  mutate(cat = "Minimum Distance") %>%
  bind_rows(.,modMilper %>%
              map(., broom::tidy) %>%
              bind_rows() %>%
              filter(term %in% c("z_tti", "z_ttidiff")) %>%
              mutate(cat = "Distance Categories")) %>%
  mutate(mod = "Military Personnel") -> C1


M4 <- fixest::feols(checks ~ c_ttim + c_polariz + c_wbgdppc2011est + 
                               c_milit + c_polity2 , fixef = "ccode", data=subset(dataChecks)) 


modChecks[[1]] %>% broom::tidy(.) %>% slice(1) %>% mutate(cat = "Distance Categories") %>%
  bind_rows(., M4 %>% broom::tidy(.) %>% slice(1) %>% mutate(cat = "Minimum Distance")) %>%
  mutate(mod = "Changes in Veto Players") -> C2


M5 <- glm(mkonset ~ lelc_eliti*z_ttim + postcold +
                         lprio_civilwar + lprio_civilwar + Lrsh_war + le_migdppcln + lv2x_libdem + ethfrac + styear + styear2 + styear3,
                       data=subset(dataHK, statefail == 1), family=binomial(link="logit"))


modHK[[2]] %>% broom::tidy(.) %>% slice(2,3,13) %>% mutate(cat = "Distance Categories") %>%
  bind_rows(., M5 %>% broom::tidy(.) %>% slice(2,3,13) %>% mutate(cat = "Minimum Distance")) %>%
  mutate(mod = "Mass Killing Onsets") -> C3


M6 <- fixest::feols(Fv2lgfemleg ~ ttim + nonterrrivalry + PR + 
                           v2x_polyarchy + cedaw + lngdppc_full + prio_civilwar + 
                           postwar,  cluster = "ccode", fixef="ccode", data=dataKK)


modKK[[2]] %>% broom::tidy(.) %>% slice(1) %>% mutate(cat = "Distance Categories") %>%
  bind_rows(., M6 %>% broom::tidy(.) %>% slice(1) %>% mutate(cat = "Minimum Distance") )%>%
  mutate(mod = "Women's Legislative Representation") -> C4


bind_rows(C1, C2, C3, C4) %>%
  select(term:mod) %>%
  rename(se = `std.error`) %>%
  mutate(lab = case_when(
    term %in% c("z_tti", "tti", "c_tti", "ttim", "c_ttim", "z_ttim") ~ "Territorial Threat",
    term %in% c("z_ttidiff", "z_ttimdiff") ~ "Territorial Threat\n(Diff. from Previous Year)",
    term == "lelc_eliti" ~ "Exclusionary Ideology",
    term %in% c("lelc_eliti:z_ttim", "lelc_eliti:z_tti") ~ "Territorial Threat*Exclusionary Ideology"
  )) -> dataCompareMods

saveRDS(dataCompareMods, "data/dataCompareMods.rds")