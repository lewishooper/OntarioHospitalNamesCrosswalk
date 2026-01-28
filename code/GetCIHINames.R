library(readxl)
library(tidyverse)
rm(list=ls())
reaCIHIsepsi<-read_xlsx("E:/Public/CIHI Source Data/831-in-hospital-sepsis-data-table-enJan2025.xlsx",sheet="Table 1")
Names<-as.character(reaCIHIsepsi[1,])
str(Names)
OntSepsis<-tibble(reaCIHIsepsi)
ncol(OntSepsis)
length(Names)

colnames(OntSepsis)<-make.names(Names)
 OntSepsis<-OntSepsis
 OntSepsis<-OntSepsis[-1,]
 
 CIHINamesAndTypes<-OntSepsis %>%
   filter(Province.Territory=='Ontario') %>%
   filter(Hospital.Peer.Group!="–") %>%
   select(Place.or.organization,Corporation,Hospital.Peer.Group)
 
write_csv(CIHINamesAndTypes,"E:/Public/OntarioHospitalNamesCrosswalk/sources/CIHINamesAndTypes.csv")
HowMany<-CIHINamesAndTypes%>%
  group_by(Place.or.organization) %>%
  mutate(NumCount=n()) %>% unique()
  