


rm(list=ls())

ptm <- proc.time()

library(foreign)
library(data.table)
library(survey)
library(plyr)
library(sas7bdat)
library(survival)

dx<-paste0("DX", 1:25)
dxccs<-paste0("DXCCS", 1:15)
e_ccs<-paste0("E_CCS", 1:4)
ecode<-paste0("ECODE", 1:4)
bicycle_ccs<-paste0("BICYCLE_CCS", 1:4)
pr<-paste0("PR", 1:15)
bicycle_icd<-paste0("BICYCLE_ICD", 1:4)


hcup05<-fread("/home/cc/HCUP/2005/Data/nis_2005_core.csv")
weights05 <- read.sas7bdat(file = "/home/cc/HCUP/2005/Data/nis_2005_hospital_trendwt.sas7bdat")
cost05<-fread("/home/cc/HCUP/2005/Data/cc2005NIS.csv")
hcup<-hcup05[1000:1000000,]
hcup$intracranial=0
hcup$skullandface=0
hcup$spinalcord=0
hcup$cransurg=0
hcup$spinsurg=0
for (i in 1:15){
  hcup$intracranial<-(ifelse(hcup[[dxccs[i]]]==233 & !is.na(hcup[[dxccs[i]]]),1, hcup$intracranial))
  hcup$skullandface<-(ifelse(hcup[[dxccs[i]]]==228 & !is.na(hcup[[dxccs[i]]]),1,hcup$skullandface))
  hcup$spinalcord<-(ifelse(hcup[[dxccs[i]]]==227 & !is.na(hcup[[dxccs[i]]]),1,hcup$spinalcord))
  hcup$cransurg<-(ifelse(hcup[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                        & !is.na(hcup[[pr[i]]]),1,hcup$cransurg))
  hcup$spinsurg<-(ifelse(hcup[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                        & !is.na(hcup[[pr[i]]]),1,hcup$spinsurg))
}



hcup$bicycle_ccs=0
hcup$bicycle_icd=0
for (i in 1:4){
  hcup$bicycle_ccs<-ifelse(hcup[[e_ccs[i]]]==2608 & !is.na(hcup[[e_ccs[i]]]),1,hcup$bicycle_ccs)
  hcup$bicycle_icd<-ifelse(hcup[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                           & !is.na(hcup[[ecode[i]]]),1,hcup$bicycle_icd)
}


hcups<-merge(hcup,weights05,"HOSPID")
hcups<-merge(hcups,cost05,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial1<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial1<-plyr::rename(intracranial1, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))


skullandface<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface1<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface1<-plyr::rename(skullandface1, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord1<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord1<-plyr::rename(spinalcord1, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg1<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg1<-plyr::rename(cransurg1, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg1<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg1<-plyr::rename(spinsurg1, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord1<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord1<-plyr::rename(spinalcord1, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial1<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial1<-plyr::rename(intracranial1, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))






##HCUP 04
hcup04<-fread("/home/cc/HCUP/2004/Data/nis_2004_core.csv")
weights04<-fread("/home/cc/HCUP/2004/Data/nis_2004_hospital_trendwt.csv")
hcup04$intracranial=0
hcup04$skullandface=0
hcup04$spinalcord=0
hcup04$cransurg=0
hcup04$spinsurg=0
for (i in 1:15){
  hcup04$intracranial<-(ifelse(hcup04[[dxccs[i]]]==233 & !is.na(hcup04[[dxccs[i]]]),1, hcup04$intracranial))
  hcup04$skullandface<-(ifelse(hcup04[[dxccs[i]]]==228 & !is.na(hcup04[[dxccs[i]]]),1,hcup04$skullandface))
  hcup04$spinalcord<-(ifelse(hcup04[[dxccs[i]]]==227 & !is.na(hcup04[[dxccs[i]]]),1,hcup04$spinalcord))
  hcup04$cransurg<-(ifelse(hcup04[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                         & !is.na(hcup04[[pr[i]]]),1,hcup04$cransurg))
  hcup04$spinsurg<-(ifelse(hcup04[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                         & !is.na(hcup04[[pr[i]]]),1,hcup04$spinsurg))
}



hcup04$bicycle_ccs=0
hcup04$bicycle_icd=0
for (i in 1:4){
  hcup04$bicycle_ccs<-ifelse(hcup04[[e_ccs[i]]]==2608 & !is.na(hcup04[[e_ccs[i]]]),1,hcup04$bicycle_ccs)
  hcup04$bicycle_icd<-ifelse(hcup04[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                           & !is.na(hcup04[[ecode[i]]]),1,hcup04$bicycle_icd)
}


hcups<-merge(hcup04,weights04,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table04<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial04.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial04.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial04.weight<-plyr::rename(intracranial04.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface04.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface04.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface04.weight<-plyr::rename(skullandface04.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord04.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord04.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord04.weight<-plyr::rename(spinalcord04.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg04.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg04.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg04.weight<-plyr::rename(cransurg04.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg04.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg04.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg04.weight<-plyr::rename(spinsurg04.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord04.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord04.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord04.weight<-plyr::rename(spinalcord04.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial04.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial04.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial04.weight<-plyr::rename(intracranial04.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup04, hcups)

proc.time() - ptm

##HCUP 05

hcup05<-fread("/home/cc/HCUP/2005/Data/nis_2005_core.csv")
# weights05<-fread("/home/cc/HCUP/2005/Data/nis_2005_hospital_trendwt.csv")
weights05 <- read.sas7bdat(file = "/home/cc/HCUP/2005/Data/nis_2005_hospital_trendwt.sas7bdat")
cost05<-fread("/home/cc/HCUP/2005/Data/cc2005NIS.csv")
hcup05$intracranial=0
hcup05$skullandface=0
hcup05$spinalcord=0
hcup05$cransurg=0
hcup05$spinsurg=0
for (i in 1:15){
  hcup05$intracranial<-(ifelse(hcup05[[dxccs[i]]]==233 & !is.na(hcup05[[dxccs[i]]]),1, hcup05$intracranial))
  hcup05$skullandface<-(ifelse(hcup05[[dxccs[i]]]==228 & !is.na(hcup05[[dxccs[i]]]),1,hcup05$skullandface))
  hcup05$spinalcord<-(ifelse(hcup05[[dxccs[i]]]==227 & !is.na(hcup05[[dxccs[i]]]),1,hcup05$spinalcord))
  hcup05$cransurg<-(ifelse(hcup05[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                           & !is.na(hcup05[[pr[i]]]),1,hcup05$cransurg))
  hcup05$spinsurg<-(ifelse(hcup05[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                           & !is.na(hcup05[[pr[i]]]),1,hcup05$spinsurg))
}



hcup05$bicycle_ccs=0
hcup05$bicycle_icd=0
for (i in 1:4){
  hcup05$bicycle_ccs<-ifelse(hcup05[[e_ccs[i]]]==2608 & !is.na(hcup05[[e_ccs[i]]]),1,hcup05$bicycle_ccs)
  hcup05$bicycle_icd<-ifelse(hcup05[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                             & !is.na(hcup05[[ecode[i]]]),1,hcup05$bicycle_icd)
}


hcups<-merge(hcup05,weights05,"HOSPID")
hcups<-merge(hcups,cost05,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table05<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial05.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial05.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial05.weight<-plyr::rename(intracranial05.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface05.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface05.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface05.weight<-plyr::rename(skullandface05.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord05.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord05.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord05.weight<-plyr::rename(spinalcord05.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg05.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg05.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg05.weight<-plyr::rename(cransurg05.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg05.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg05.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg05.weight<-plyr::rename(spinsurg05.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord05.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord05.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord05.weight<-plyr::rename(spinalcord05.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial05.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial05.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial05.weight<-plyr::rename(intracranial05.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup05, hcups)

##HCUP 06

hcup06<-fread("/home/cc/HCUP/2006/Data/nis_2006_core.csv")
# weights06<-fread("/home/cc/HCUP/2006/Data/nis_2006_hospital_trendwt.csv")
weights06 <- read.sas7bdat(file = "/home/cc/HCUP/2006/Data/nis_2006_hospital_trendwt.sas7bdat")
cost06<-fread("/home/cc/HCUP/2006/Data/cc2006NIS.csv")
hcup06$intracranial=0
hcup06$skullandface=0
hcup06$spinalcord=0
hcup06$cransurg=0
hcup06$spinsurg=0
for (i in 1:15){
  hcup06$intracranial<-(ifelse(hcup06[[dxccs[i]]]==233 & !is.na(hcup06[[dxccs[i]]]),1, hcup06$intracranial))
  hcup06$skullandface<-(ifelse(hcup06[[dxccs[i]]]==228 & !is.na(hcup06[[dxccs[i]]]),1,hcup06$skullandface))
  hcup06$spinalcord<-(ifelse(hcup06[[dxccs[i]]]==227 & !is.na(hcup06[[dxccs[i]]]),1,hcup06$spinalcord))
  hcup06$cransurg<-(ifelse(hcup06[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                           & !is.na(hcup06[[pr[i]]]),1,hcup06$cransurg))
  hcup06$spinsurg<-(ifelse(hcup06[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8061', '0309')
                           & !is.na(hcup06[[pr[i]]]),1,hcup06$spinsurg))
}



hcup06$bicycle_ccs=0
hcup06$bicycle_icd=0
for (i in 1:4){
  hcup06$bicycle_ccs<-ifelse(hcup06[[e_ccs[i]]]==2608 & !is.na(hcup06[[e_ccs[i]]]),1,hcup06$bicycle_ccs)
  hcup06$bicycle_icd<-ifelse(hcup06[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                             & !is.na(hcup06[[ecode[i]]]),1,hcup06$bicycle_icd)
}


hcups<-merge(hcup06,weights06,"HOSPID")
hcups<-merge(hcups,cost06,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table06<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial06.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial06.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial06.weight<-plyr::rename(intracranial06.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface06.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface06.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface06.weight<-plyr::rename(skullandface06.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord06.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord06.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord06.weight<-plyr::rename(spinalcord06.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg06.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg06.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg06.weight<-plyr::rename(cransurg06.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg06.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg06.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg06.weight<-plyr::rename(spinsurg06.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord06.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord06.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord06.weight<-plyr::rename(spinalcord06.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial06.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial06.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial06.weight<-plyr::rename(intracranial06.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup06, hcups)


##HCUP 07

hcup07<-fread("/home/cc/HCUP/2007/Data/nis_2007_core.csv")
# weights07<-fread("/home/cc/HCUP/2007/Data/nis_2007_hospital_trendwt.csv")
weights07 <- read.sas7bdat(file = "/home/cc/HCUP/2007/Data/nis_2007_hospital_trendwt.sas7bdat")
cost07<-fread("/home/cc/HCUP/2007/Data/cc2007NIS.csv")
hcup07$intracranial=0
hcup07$skullandface=0
hcup07$spinalcord=0
hcup07$cransurg=0
hcup07$spinsurg=0
for (i in 1:15){
  hcup07$intracranial<-(ifelse(hcup07[[dxccs[i]]]==233 & !is.na(hcup07[[dxccs[i]]]),1, hcup07$intracranial))
  hcup07$skullandface<-(ifelse(hcup07[[dxccs[i]]]==228 & !is.na(hcup07[[dxccs[i]]]),1,hcup07$skullandface))
  hcup07$spinalcord<-(ifelse(hcup07[[dxccs[i]]]==227 & !is.na(hcup07[[dxccs[i]]]),1,hcup07$spinalcord))
  hcup07$cransurg<-(ifelse(hcup07[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                           & !is.na(hcup07[[pr[i]]]),1,hcup07$cransurg))
  hcup07$spinsurg<-(ifelse(hcup07[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                           & !is.na(hcup07[[pr[i]]]),1,hcup07$spinsurg))
}



hcup07$bicycle_ccs=0
hcup07$bicycle_icd=0
for (i in 1:4){
  hcup07$bicycle_ccs<-ifelse(hcup07[[e_ccs[i]]]==2608 & !is.na(hcup07[[e_ccs[i]]]),1,hcup07$bicycle_ccs)
  hcup07$bicycle_icd<-ifelse(hcup07[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                             & !is.na(hcup07[[ecode[i]]]),1,hcup07$bicycle_icd)
}


hcups<-merge(hcup07,weights07,"HOSPID")
hcups<-merge(hcups,cost07,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table07<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial07.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial07.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial07.weight<-plyr::rename(intracranial07.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface07.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface07.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface07.weight<-plyr::rename(skullandface07.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord07.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord07.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord07.weight<-plyr::rename(spinalcord07.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg07.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg07.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg07.weight<-plyr::rename(cransurg07.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg07.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg07.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg07.weight<-plyr::rename(spinsurg07.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord07.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord07.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord07.weight<-plyr::rename(spinalcord07.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial07.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial07.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial07.weight<-plyr::rename(intracranial07.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup07, hcups)

##HCUP 08

hcup08<-fread("/home/cc/HCUP/2008/Data/nis_2008_core.csv")
# weights08<-fread("/home/cc/HCUP/2008/Data/nis_2008_hospital_trendwt.csv")
weights08 <- read.sas7bdat(file = "/home/cc/HCUP/2008/Data/nis_2008_hospital_trendwt.sas7bdat")
cost08<-fread("/home/cc/HCUP/2008/Data/cc2008NIS.csv")
hcup08$intracranial=0
hcup08$skullandface=0
hcup08$spinalcord=0
hcup08$cransurg=0
hcup08$spinsurg=0
for (i in 1:15){
  hcup08$intracranial<-(ifelse(hcup08[[dxccs[i]]]==233 & !is.na(hcup08[[dxccs[i]]]),1, hcup08$intracranial))
  hcup08$skullandface<-(ifelse(hcup08[[dxccs[i]]]==228 & !is.na(hcup08[[dxccs[i]]]),1,hcup08$skullandface))
  hcup08$spinalcord<-(ifelse(hcup08[[dxccs[i]]]==227 & !is.na(hcup08[[dxccs[i]]]),1,hcup08$spinalcord))
  hcup08$cransurg<-(ifelse(hcup08[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                           & !is.na(hcup08[[pr[i]]]),1,hcup08$cransurg))
  hcup08$spinsurg<-(ifelse(hcup08[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                           & !is.na(hcup08[[pr[i]]]),1,hcup08$spinsurg))
}



hcup08$bicycle_ccs=0
hcup08$bicycle_icd=0
for (i in 1:4){
  hcup08$bicycle_ccs<-ifelse(hcup08[[e_ccs[i]]]==2608 & !is.na(hcup08[[e_ccs[i]]]),1,hcup08$bicycle_ccs)
  hcup08$bicycle_icd<-ifelse(hcup08[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                             & !is.na(hcup08[[ecode[i]]]),1,hcup08$bicycle_icd)
}


hcups<-merge(hcup08,weights08,"HOSPID")
hcups<-merge(hcups,cost08,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table08<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial08.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial08.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial08.weight<-plyr::rename(intracranial08.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface08.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface08.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface08.weight<-plyr::rename(skullandface08.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord08.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord08.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord08.weight<-plyr::rename(spinalcord08.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg08.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg08.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg08.weight<-plyr::rename(cransurg08.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg08.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg08.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg08.weight<-plyr::rename(spinsurg08.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord08.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord08.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord08.weight<-plyr::rename(spinalcord08.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial08.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial08.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial08.weight<-plyr::rename(intracranial08.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup08, hcups)

##HCUP 09

hcup09<-fread("/home/cc/HCUP/2009/Data/nis_2009_core.csv")
# weights09<-fread("/home/cc/HCUP/2009/Data/nis_2009_hospital_trendwt.csv")
weights09 <- read.sas7bdat(file = "/home/cc/HCUP/2009/Data/nis_2009_hospital_trendwt.sas7bdat")
cost09<-fread("/home/cc/HCUP/2009/Data/cc2009NIS_V2.csv")
hcup09$intracranial=0
hcup09$skullandface=0
hcup09$spinalcord=0
hcup09$cransurg=0
hcup09$spinsurg=0
for (i in 1:15){
  hcup09$intracranial<-(ifelse(hcup09[[dxccs[i]]]==233 & !is.na(hcup09[[dxccs[i]]]),1, hcup09$intracranial))
  hcup09$skullandface<-(ifelse(hcup09[[dxccs[i]]]==228 & !is.na(hcup09[[dxccs[i]]]),1,hcup09$skullandface))
  hcup09$spinalcord<-(ifelse(hcup09[[dxccs[i]]]==227 & !is.na(hcup09[[dxccs[i]]]),1,hcup09$spinalcord))
  hcup09$cransurg<-(ifelse(hcup09[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                           & !is.na(hcup09[[pr[i]]]),1,hcup09$cransurg))
  hcup09$spinsurg<-(ifelse(hcup09[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                           & !is.na(hcup09[[pr[i]]]),1,hcup09$spinsurg))
}



hcup09$bicycle_ccs=0
hcup09$bicycle_icd=0
for (i in 1:4){
  hcup09$bicycle_ccs<-ifelse(hcup09[[e_ccs[i]]]==2608 & !is.na(hcup09[[e_ccs[i]]]),1,hcup09$bicycle_ccs)
  hcup09$bicycle_icd<-ifelse(hcup09[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                             & !is.na(hcup09[[ecode[i]]]),1,hcup09$bicycle_icd)
}


hcups<-merge(hcup09,weights09,"HOSPID")
hcups<-merge(hcups,cost09,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table09<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial09.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial09.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial09.weight<-plyr::rename(intracranial09.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface09.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface09.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface09.weight<-plyr::rename(skullandface09.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord09.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord09.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord09.weight<-plyr::rename(spinalcord09.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg09.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg09.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg09.weight<-plyr::rename(cransurg09.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg09.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg09.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg09.weight<-plyr::rename(spinsurg09.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord09.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord09.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord09.weight<-plyr::rename(spinalcord09.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial09.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial09.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial09.weight<-plyr::rename(intracranial09.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup09, hcups)

##HCUP 10

hcup10<-fread("/home/cc/HCUP/2010/Data/nis_2010_core.csv")
# weights10<-fread("/home/cc/HCUP/2010/Data/nis_2010_hospital_trendwt.csv")
weights10 <- read.sas7bdat(file = "/home/cc/HCUP/2010/Data/nis_2010_hospital_trendwt.sas7bdat")
cost10<-fread("/home/cc/HCUP/2010/Data/cc2010NIS.csv")
hcup10$intracranial=0
hcup10$skullandface=0
hcup10$spinalcord=0
hcup10$cransurg=0
hcup10$spinsurg=0
for (i in 1:15){
  hcup10$intracranial<-(ifelse(hcup10[[dxccs[i]]]==233 & !is.na(hcup10[[dxccs[i]]]),1, hcup10$intracranial))
  hcup10$skullandface<-(ifelse(hcup10[[dxccs[i]]]==228 & !is.na(hcup10[[dxccs[i]]]),1,hcup10$skullandface))
  hcup10$spinalcord<-(ifelse(hcup10[[dxccs[i]]]==227 & !is.na(hcup10[[dxccs[i]]]),1,hcup10$spinalcord))
  hcup10$cransurg<-(ifelse(hcup10[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                           & !is.na(hcup10[[pr[i]]]),1,hcup10$cransurg))
  hcup10$spinsurg<-(ifelse(hcup10[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                           & !is.na(hcup10[[pr[i]]]),1,hcup10$spinsurg))
}



hcup10$bicycle_ccs=0
hcup10$bicycle_icd=0
for (i in 1:4){
  hcup10$bicycle_ccs<-ifelse(hcup10[[e_ccs[i]]]==2608 & !is.na(hcup10[[e_ccs[i]]]),1,hcup10$bicycle_ccs)
  hcup10$bicycle_icd<-ifelse(hcup10[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                             & !is.na(hcup10[[ecode[i]]]),1,hcup10$bicycle_icd)
}


hcups<-merge(hcup10,weights10,"HOSPID")
hcups<-merge(hcups,cost10,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table10<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial10.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial10.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial10.weight<-plyr::rename(intracranial10.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface10.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface10.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface10.weight<-plyr::rename(skullandface10.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord10.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord10.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord10.weight<-plyr::rename(spinalcord10.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg10.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg10.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg10.weight<-plyr::rename(cransurg10.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg10.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg10.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg10.weight<-plyr::rename(spinsurg10.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord10.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord10.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord10.weight<-plyr::rename(spinalcord10.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial10.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial10.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial10.weight<-plyr::rename(intracranial10.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup10, hcups)

##HCUP 11

hcup11<-fread("/home/cc/HCUP/2011/Data/nis_2011_core.csv")
# weights11<-fread("/home/cc/HCUP/2011/Data/nis_2011_hospital_trendwt.csv")
weights11 <- read.sas7bdat(file = "/home/cc/HCUP/2011/Data/nis_2011_hospital_trendwt.sas7bdat")
cost11<-fread("/home/cc/HCUP/2011/Data/cc2011NIS.csv")
hcup11$intracranial=0
hcup11$skullandface=0
hcup11$spinalcord=0
hcup11$cransurg=0
hcup11$spinsurg=0
for (i in 1:15){
  hcup11$intracranial<-(ifelse(hcup11[[dxccs[i]]]==233 & !is.na(hcup11[[dxccs[i]]]),1, hcup11$intracranial))
  hcup11$skullandface<-(ifelse(hcup11[[dxccs[i]]]==228 & !is.na(hcup11[[dxccs[i]]]),1,hcup11$skullandface))
  hcup11$spinalcord<-(ifelse(hcup11[[dxccs[i]]]==227 & !is.na(hcup11[[dxccs[i]]]),1,hcup11$spinalcord))
  hcup11$cransurg<-(ifelse(hcup11[[pr[i]]] %in% c('0131', '0121', '0124', '0125', '0109', '0126', '0128', '0202', '0299', '0110', '0221', '0139', '0159')
                           & !is.na(hcup11[[pr[i]]]),1,hcup11$cransurg))
  hcup11$spinsurg<-(ifelse(hcup11[[pr[i]]] %in% c('8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8451', '8162', '8163', '8164', '8051', '0309')
                           & !is.na(hcup11[[pr[i]]]),1,hcup11$spinsurg))
}



hcup11$bicycle_ccs=0
hcup11$bicycle_icd=0
for (i in 1:4){
  hcup11$bicycle_ccs<-ifelse(hcup11[[e_ccs[i]]]==2608 & !is.na(hcup11[[e_ccs[i]]]),1,hcup11$bicycle_ccs)
  hcup11$bicycle_icd<-ifelse(hcup11[[ecode[i]]] %in% c('E8106', 'E8116', 'E8126', 'E8136', 'E8146', 'E8156' , 'E8166', 'E8176', 'E8186', 'E8196') 
                             & !is.na(hcup11[[ecode[i]]]),1,hcup11$bicycle_icd)
}


hcups<-merge(hcup11,weights11,"HOSPID")
hcups<-merge(hcups,cost11,"HOSPID")
hcups$cost<-hcups$TOTCHG * hcups$GAPICC
hcups$bicycle<-ifelse(hcups$bicycle_ccs==1 | hcups$bicycle_icd==1, 1, 0)
design<-svydesign(id = ~ HOSPID, strata = ~NIS_STRATUM.x, weights= ~TRENDWT, data=hcups)
options(survey.lonely.psu = "certainty")
table11<-as.data.frame(svyby(~AGE+LOS+TOTCHG+cost, ~bicycle, design, svymean,na.rm=T))

intracranial11.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial11.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial11.weight<-plyr::rename(intracranial11.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

skullandface11.raw<-as.data.frame(svyby(~skullandface, ~bicycle+skullandface, design, unwtd.count))
skullandface11.weight<-as.data.frame(svyby(~as.factor(skullandface), ~bicycle, design, svytotal))
skullandface11.weight<-plyr::rename(skullandface11.weight, c("as.factor(skullandface)0"="Skullandface No", "as.factor(skullandface)1"="Skullandface Yes", "se.as.factor(skullandface)0"="Skullandface No SE", "se.as.factor(skullandface)1"="Skullandface Yes SE"))

spinalcord11.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord11.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord11.weight<-plyr::rename(spinalcord11.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

cransurg11.raw<-as.data.frame(svyby(~cransurg, ~bicycle+cransurg, design, unwtd.count))
cransurg11.weight<-as.data.frame(svyby(~as.factor(cransurg), ~bicycle, design, svytotal))
cransurg11.weight<-plyr::rename(cransurg11.weight, c("as.factor(cransurg)0"="Cransurg No", "as.factor(cransurg)1"="Cransurg Yes", "se.as.factor(cransurg)0"="Cransurg No SE", "se.as.factor(cransurg)1"="Cransurg Yes SE"))

spinsurg11.raw<-as.data.frame(svyby(~spinsurg, ~bicycle+spinsurg, design, unwtd.count))
spinsurg11.weight<-as.data.frame(svyby(~as.factor(spinsurg), ~bicycle, design, svytotal))
spinsurg11.weight<-plyr::rename(spinsurg11.weight, c("as.factor(spinsurg)0"="Spinsurg No", "as.factor(spinsurg)1"="Spinsurg Yes", "se.as.factor(spinsurg)0"="Spinsurg No SE", "se.as.factor(spinsurg)1"="Spinsurg Yes SE"))

spinalcord11.raw<-as.data.frame(svyby(~spinalcord, ~bicycle+spinalcord, design, unwtd.count))
spinalcord11.weight<-as.data.frame(svyby(~as.factor(spinalcord), ~bicycle, design, svytotal))
spinalcord11.weight<-plyr::rename(spinalcord11.weight, c("as.factor(spinalcord)0"="Spinalcord No", "as.factor(spinalcord)1"="Spinalcord Yes", "se.as.factor(spinalcord)0"="Spinalcord No SE", "se.as.factor(spinalcord)1"="Spinalcord Yes SE"))

intracranial11.raw<-as.data.frame(svyby(~intracranial, ~bicycle+intracranial, design, unwtd.count))
intracranial11.weight<-as.data.frame(svyby(~as.factor(intracranial), ~bicycle, design, svytotal))
intracranial11.weight<-plyr::rename(intracranial11.weight, c("as.factor(intracranial)0"="Intracranial No", "as.factor(intracranial)1"="Intracranial Yes", "se.as.factor(intracranial)0"="Intracranial No SE", "se.as.factor(intracranial)1"="Intracranial Yes SE"))

rm(hcup11, hcups)



proc.time() - ptm
