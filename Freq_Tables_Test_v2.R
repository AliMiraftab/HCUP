ptm <- proc.time()

rm(list=ls())


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


proc.time() - ptm

