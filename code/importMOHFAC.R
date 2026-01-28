# clean MOHFAC_202501.xlsx
library(tidyverse)
library(readr)
library(readxl)
source<-"E:/Public/OntarioHospitalNamesCrosswalk/sources"
fileName<-"MOH_FAC_202601.xlsx"
MOHFAC<-read_xlsx(file.path(source,fileName)) %>%
  rename(FAC='Facility ID') %>%
  mutate(FAC=as.character(FAC))%>%
  rename(MOHName='Facility') %>%
  rename(Type='Facility Type') %>%
  select(FAC,MOHName,Type)
saveRDS(MOHFAC,file.path(source,"MOHFAC.rds"))
write_csv(MOHFAC,file.path(source,"MOHFAC.csv"))
