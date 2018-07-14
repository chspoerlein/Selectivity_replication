############################################################
# Replication file for Afghanistan
#
# Data gathered from IPUMS-International
#
# Generates a file each for regional- and country-level
# educational distributions
##
##############################################################

library(tidyverse)

source("helper_agerecode.R")

data <- haven::read_dta("hl.dta")
data <- data %>% rename(HL6=age)
data$age <- age_recode(data$HL6)
data <- data %>%
  mutate(female=sex-1,
         region=geolev1) %>% 
  mutate(educat=NA) %>%
  mutate(educat=replace(educat,edattaind==110 | edattaind==120 | edattaind==130 | edattaind==211 | edattaind==212,1), 
         educat=replace(educat,edattaind==221 | edattaind==222,2),
         educat=replace(educat,edattaind==311 | edattaind==312 | edattaind==320 | edattaind==321 | edattaind==322,3),
         educat=replace(educat,edattaind==400,4)) %>%
  select(region, educat, age, female) %>%
  filter(!is.na(educat),!is.na(age),!is.na(female)) %>%
  group_by(region, educat, age, female) %>%
  count() %>%
  spread(key=educat, value=n) %>%
  rename(isced1="1",
         isced2="2",
         isced3="3",
         isced4="4") %>%
  replace(is.na(.),0) %>%
  mutate(tot=isced1+isced2+isced3+isced4) %>%
  mutate(isced1=isced1/tot,
         isced2=isced2/tot,
         isced3=isced3/tot,
         isced4=isced4/tot) %>%
  mutate(rel_edu1=isced1/2,
         rel_edu2=isced1+(isced2/2),
         rel_edu3=isced1+isced2+(isced3/2),
         rel_edu4=isced1+isced2+isced3+(isced4/2)) %>%
  select(female, age, region, starts_with("rel_")) %>%
  gather(isced, rel_edu, starts_with("rel_")) %>%
  mutate(isced=replace(isced,isced=="rel_edu1",1),
         isced=replace(isced,isced=="rel_edu2",2),
         isced=replace(isced,isced=="rel_edu3",3),
         isced=replace(isced,isced=="rel_edu4",4)) %>%
  mutate(ISCO3C="ARG", year=2010)

file_name <- paste0("data/regional/",data$ISCO3C[1],data$year[1],".csv")
write_csv(data, file_name)

#######################################################################
#######################################################################
######################## COUNTRY-LEVEL DATA ###########################
#######################################################################
#######################################################################


data <- haven::read_dta("hl.dta")
data <- data %>% rename(HL6=age)
data$age <- age_recode(data$HL6)
data <- data %>%
  mutate(female=sex-1) %>% 
  mutate(educat=NA) %>%
  mutate(educat=replace(educat,edattaind==110 | edattaind==120 | edattaind==130 | edattaind==211 | edattaind==212,1), 
         educat=replace(educat,edattaind==221 | edattaind==222,2),
         educat=replace(educat,edattaind==311 | edattaind==312 | edattaind==320 | edattaind==321 | edattaind==322,3),
         educat=replace(educat,edattaind==400,4)) %>%
  select(educat, age, female) %>%
  filter(!is.na(educat),!is.na(age),!is.na(female)) %>%
  group_by(educat, age, female) %>%
  count() %>%
  spread(key=educat, value=n) %>%
  rename(isced1="1",
         isced2="2",
         isced3="3",
         isced4="4") %>%
  replace(is.na(.),0) %>%
  mutate(tot=isced1+isced2+isced3+isced4) %>%
  mutate(isced1=isced1/tot,
         isced2=isced2/tot,
         isced3=isced3/tot,
         isced4=isced4/tot) %>%
  mutate(rel_edu1=isced1/2,
         rel_edu2=isced1+(isced2/2),
         rel_edu3=isced1+isced2+(isced3/2),
         rel_edu4=isced1+isced2+isced3+(isced4/2)) %>%
  select(female, age, starts_with("rel_")) %>%
  gather(isced, rel_edu, starts_with("rel_")) %>%
  mutate(isced=replace(isced,isced=="rel_edu1",1),
         isced=replace(isced,isced=="rel_edu2",2),
         isced=replace(isced,isced=="rel_edu3",3),
         isced=replace(isced,isced=="rel_edu4",4)) %>%
  mutate(ISCO3C="ARG", year=2010)

file_name <- paste0("data/country/",data$ISCO3C[1],data$year[1],"_cnt.csv")
write_csv(data, file_name)


