C#select hospitals from statistics canada odhf Database
library(tidyverse)
guess_encoding("E:/Public/HospitalNames/source/ODHF_v1.1/ODHF_v1.1/odhf_v1.1.csv")
StatsCan<-read.csv("E:/Public/HospitalNames/source/ODHF_v1.1/ODHF_v1.1/odhf_v1.1.csv",
                   fileEncoding = "windows-1252")

OntarioHospitalsStatsCan<-StatsCan %>%
  filter(province=="on") %>%
  filter(odhf_facility_type=="Hospitals")
guess_encoding("E:/Public/HospitalNames/source/2021_92-150-X_eng/csd.csv")

SACType<-read.csv("E:/Public/HospitalNames/source/2021_92-150-X_eng/csd.csv",fileEncoding = "ISO-8859-1") %>%
  select(CSDuid,CSDname,SACtype)
OntarioHospitalsStatsCan<-left_join(OntarioHospitalsStatsCan,SACType,by="CSDuid")

saveRDS(OntarioHospitalsStatsCan,"E:/Public/OntarioHospitalNamesCrosswalk/sources/SACType.RDS")
