############################################################
# Replication file for Algeria
#
# Data gathered from XXX
#
# Generates a file each for regional- and country-level
# educational distributions
# 
# Additional info on regional variable at the bottom
##############################################################

library(tidyverse)

source("helper_agerecode.R")

data <- haven::read_dta("hl.dta") 
data$age <- age_recode(data$HL6)
data <- data %>%
  mutate(female=HL4-1,
         lgreg3=HH7) %>% 
  mutate(educat=NA) %>%
  mutate(educat=replace(educat,ED3==2 | ED4A==0 | ED4A==1,1), 
         educat=replace(educat,ED4A==2,2),
         educat=replace(educat,ED4A==3,3),
         educat=replace(educat,ED4A==4,4)) %>%
  select(lgreg3, educat, age, female) %>%
  filter(!is.na(educat),!is.na(age),!is.na(female)) %>%
  group_by(lgreg3, educat, age, female) %>%
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
  select(female, age, lgreg3, starts_with("rel_")) %>%
  gather(isced, rel_edu, starts_with("rel_")) %>%
  mutate(isced=replace(isced,isced=="rel_edu1",1),
         isced=replace(isced,isced=="rel_edu2",2),
         isced=replace(isced,isced=="rel_edu3",3),
         isced=replace(isced,isced=="rel_edu4",4)) %>%
  mutate(ISCO3C="DZA", year=2013)

file_name <- paste0("data/regional/",data$ISCO3C[1],data$year[1],".csv")
write_csv(data, file_name)

#######################################################################
#######################################################################
######################## COUNTRY-LEVEL DATA ###########################
#######################################################################
#######################################################################


data <- haven::read_dta("hl.dta") 
data$age <- age_recode(data$HL6)
data <- data %>%
  mutate(female=HL4-1) %>% 
  mutate(educat=NA) %>%
  mutate(educat=replace(educat,ED3==2 | ED4A==0 | ED4A==1,1), 
         educat=replace(educat,ED4A==2 & (ED4B>=7 & ED4B<=9),2),
         educat=replace(educat,ED4A==2 & (ED4B>=10 & ED4B<=12),3),
         educat=replace(educat,ED4A==3,4)) %>%
  filter(!is.na(educat)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(educat)) %>%
  filter(!is.na(female)) %>%
  select(educat, age, female) %>%
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
  mutate(ISCO3C="DZA", year=2013)

file_name <- paste0("data/country/",data$ISCO3C[1],data$year[1],"_cnt.csv")
write_csv(data, file_name)


######################################
# additional info for the regional variable
# Nord-Centre: Alger, Blida, Boumerde, Tipaza?, Bouira, Medea, Tizi Ouzou?, Bejaia, Chlef, Ain Defla
# Nord-Est: Annaba, Constantine, Skikda?, Jijel, Mila, Souk Ahras?, El Tarf, Guelma
# Nord-Ouest: Oran, Tlemcen?, Mostaganem, Ain Temouchent, Relizane, Sidi Bel Abbes, Mascara
# Hauts Plateaux-Centre: Djelfa, Laghouat, M'Sila
# Hauts Plateaux-Est: Setif, Batna, Khenchela, Bordj Bou Arreridj, Oum El Bouaghi, Tebessa?
# Hauts Plateaux-Ouest: Tiaret?, Saida, Tissemsilt?, Naama, El Bayadh
# Sud: Bechar, Tindouf?, Adrar, Ghardaia, Biskra, El Oued, Ouargla, Tamanrasset?, Illizi


