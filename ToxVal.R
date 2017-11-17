if(!require(readxl)) {
  install.packages("readxl"); require(readxl)}
if(!require(gdata)) {
  install.packages("gdata"); require(gdata)}
if(!require(gridExtra)) {
  install.packages("gridExtra"); require(gridExtra)}
if(!require(httk)) {
  install.packages("httk"); require(httk)}


Chem<-c(
  "BENZO(A)ANTHRACENE",
  "NAPHTHALENE",
  "FLUORANTHENE",
  "DDT, P,P'-",
  "DIELDRIN",
  "ALDRIN",
  "HEPTACHLOR",
  "HEXACHLOROCYCLOHEXANE, GAMMA-",
  "DISULFOTON",
  "ENDRIN",
  "DIAZINON",
  "HEPTACHLOR EPOXIDE",
  "PENTACHLOROPHENOL",
  "DI-N-BUTYL PHTHALATE",
  "CHLORPYRIFOS",
  "DI(2-ETHYLHEXYL)PHTHALATE",
  "2,4,6-TRICHLOROPHENOL",
  "ETHION",
  "AZINPHOS-METHYL",
  "2,4,5-TRICHLOROPHENOL",
  "PARATHION",
  "LEAD",
  "CADMIUM",
  "ZINC",
  "BENZO(B)FLUORANTHENE",
  "TRIFLURALIN",
  "ACENAPHTHENE",
  "DDD, P,P'-",
  "BENZIDINE",
  "ENDOSULFAN",
  "METHOXYCHLOR",
  "2,4-DINITROPHENOL",
  "2,4-DINITROTOLUENE",
  "DICOFOL",
  "CRESOL, PARA-",
  "MERCURIC CHLORIDE",
  "DDT, O,P'-",
  "4,6-DINITRO-O-CRESOL",
  "1,2,3-TRICHLOROBENZENE",
  "CHROMIUM, HEXAVALENT",
  "COBALT",
  "NICKEL"
  
)

CAS<-c(
  "56-55-3",
  "91-20-3",
  "206-44-0",
  "50-29-3",
  "60-57-1",
  "309-00-2",
  "76-44-8",
  "58-89-9",
  "298-04-4",
  "72-20-8",
  "333-41-5",
  "1024-57-3",
  "87-86-5",
  "84-74-2",
  "2921-88-2",
  "117-81-7",
  "88-06-2",
  "563-12-2",
  "86-50-0",
  "95-95-4",
  "56-38-2",
  "7439-92-1",
  "7440-43-9",
  "7440-66-6",
  "205-99-2",
  "1582-09-8",
  "83-32-9",
  "72-54-8",
  "92-87-5",
  "115-29-7",
  "72-43-5",
  "51-28-5",
  "121-14-2",
  "115-32-2",
  "106-44-5",
  "7487-94-7",
  "789-02-6",
  "534-52-1",
  "87-61-6",
  "18540-29-9",
  "7440-48-4",
  "7440-02-0"
)

CASRN<-c( # to find the toxicity value for HEAST
  "000056-55-3",
  "000091-20-3",
  "000206-44-0",
  "000050-29-3",
  "000060-57-1",
  "000309-00-2",
  "000076-44-8",
  "000058-89-9",
  "000298-04-4",
  "000072-20-8",
  "000333-41-5",
  "001024-57-3",
  "000087-86-5",
  "000084-74-2",
  "002921-88-2",
  "000117-81-7",
  "000088-06-2",
  "000563-12-2",
  "000086-50-0",
  "000095-95-4",
  "000056-38-2",
  "007439-92-1",
  "007440-43-9",
  "007440-66-6",
  "000205-99-2",
  "001582-09-8",
  "000083-32-9",
  "000072-54-8",
  "000092-87-5",
  "000115-29-7",
  "000072-43-5",
  "000051-28-5",
  "000121-14-2",
  "000115-32-2",
  "000106-44-5",
  "007487-94-7",
  "000789-02-6",
  "000534-52-1",
  "000087-61-6",
  "018540-29-9",
  "007440-48-4",
  "007440-02-0"
)


Chem.df <- data.frame(Chem, CAS, CASRN)
no.Chem<-nrow(Chem.df)


# Check the RSLs data
url <- "http://semspub.epa.gov/src/document/03/2245074.xls"
rsl.tb <- read.xls(url)
Chem.df[,"RSLs"]<-""

for (i in 1:no.Chem)
{
  if (is.element(Chem.df$CAS[i], rsl.tb[,15]) == TRUE) { # rsl.tb[,15] is CAS location
    Chem.df[i,"RSLs"]<-1
  } else {
    Chem.df[i,"RSLs"]<-0
  }
}

# Check IRIS data
tmp<-readLines(paste("https://cfpub.epa.gov/ncea/iris/search/basic/index.cfm"))
Chem.df[,"IRIS"]<-""

for (i in 1:no.Chem)
{
  test<-grepl(Chem.df$CAS[i], tmp)
  test1<-test[!duplicated(test)]
  if (is.element(TRUE, test1)== TRUE) {
    Chem.df[i,"IRIS"]<-1
  } else {
    Chem.df[i,"IRIS"]<-0
  }
}

# Check ASTDR data 
tmp<-readLines(paste("https://www.atsdr.cdc.gov/mrls/mrllist.asp"))
Chem.df[,"ASTDR"]<-""

for (i in 1:no.Chem)
{
  test<-grepl(Chem.df$CAS[i], tmp)
  test1<-test[!duplicated(test)]
  if (is.element(TRUE, test1)== TRUE) {
    Chem.df[i,"ASTDR"]<-1
  } else {
    Chem.df[i,"ASTDR"]<-0
  }
}

# Check PPRTV data
tmp<-readLines(paste("https://hhpprtv.ornl.gov/quickview/pprtv_papers.php"))
Chem.df[,"PPRTV"]<-""

for (i in 1:no.Chem)
{
  test<-grepl(Chem.df$CAS[i], tmp)
  test1<-test[!duplicated(test)]
  if (is.element(TRUE, test1)== TRUE) {
    Chem.df[i,"PPRTV"]<-1
  } else {
    Chem.df[i,"PPRTV"]<-0
  }
}

# Check OEHHA-Chemical data
tmp<-readLines(paste("https://oehha.ca.gov/chemicals"))
Chem.df[,"OEHHA"]<-""

for (i in 1:no.Chem)
{
  test<-grepl(Chem.df$CAS[i], tmp)
  test1<-test[!duplicated(test)]
  if (is.element(TRUE, test1)== TRUE) {
    Chem.df[i,"OEHHA"]<-1
  } else {
    Chem.df[i,"OEHHA"]<-0
  }
}

# Check HEAST data
tmp<-readLines(paste("https://epa-heast.ornl.gov/heast_appa.html"))
Chem.df[,"HEAST"]<-""

for (i in 1:no.Chem)
{
  test<-grepl(Chem.df$CASRN[i], tmp)
  test1<-test[!duplicated(test)]
  if (is.element(TRUE, test1)== TRUE) {
    Chem.df[i,"HEAST"]<-1
  } else {
    Chem.df[i,"HEAST"]<-0
  }
}

# 
#https://oehha.ca.gov/media/CPFs042909.pdf

Chem.df[,"OEHHA-CPV"]<-""
source("OEHHA_CPV.R")

for (i in 1:no.Chem)
{
  if (is.element(Chem.df$CAS[i], OEHHACPV) == TRUE) { # tmp.df[,3] is CAS location
    Chem.df[i,"OEHHA-CPV"]<-1
  } else {
    Chem.df[i,"OEHHA-CPV"]<-0
  }
}


#
Chem.df[,"RSL-SFO"]<-""
Chem.df[,"RSL-IUR"]<-""
Chem.df[,"RSL-RfDo"]<-""
Chem.df[,"RSL-RfCi"]<-""
Chem.df$CAS <- factor(Chem.df$CAS, levels=levels(rsl.tb$X.13)) # Adjust level

for (i in 1:no.Chem)
{
  tmp<-subset(rsl.tb, rsl.tb[,15]==Chem.df$CAS[i])
  Chem.df[i,"RSL-SFO"]<-as.numeric(as.character(tmp[1,1])) # change factor to numeric # Same chemical have different results
  Chem.df[i,"RSL-IUR"]<-as.numeric(as.character(tmp[1,3]))
  Chem.df[i,"RSL-RfDo"]<-as.numeric(as.character(tmp[1,5]))
  Chem.df[i,"RSL-RfCi"]<-as.numeric(as.character(tmp[1,7]))
}

Chem.df$CAS<-CAS # Revise CAS
#names(cpv.tb)

#
cpv.tb<-read.xls("CPFs042909.xls")

Chem.df[,"OEHHA-SFO"]<-""
Chem.df[,"OEHHA-IUR"]<-""

Chem.df$CAS <- factor(Chem.df$CAS, levels=levels(cpv.tb$CAS)) # Adjust level

for (i in 1:no.Chem)
{
  tmp<-subset(cpv.tb, cpv.tb[,3]==Chem.df$CAS[i])
  Chem.df[i,"OEHHA-SFO"]<-as.character(tmp[1,6]) # change factor to numeric # Same chemical have different results
  Chem.df[i,"OEHHA-IUR"]<-as.character(tmp[1,5])
}

Chem.df$CAS<-CAS # Revise CAS


# HTTK
# Generate Toxicity Table w/ AC50
tc.dt.sub <- tc.dt[`Activity Call`=="Active", 
                   .(`Chemical Name`, CASRN, `Assay Endpoint`, `Activity Call`, `AC 50`)]

Chem.tc.dt <- tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[1,2],]

for(i in 1:no.Chem){
  Chem.tc.dt <- rbind(Chem.tc.dt, tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[i,2],])
}

# Create Variables
M<-as.matrix(Chem.tc.dt[,2])
Chem.df[,"ToxCast"]<-""
Chem.df[,"Min.AC50"]<-""
Chem.df[,"Max.AC50"]<-""

for(i in 1:no.Chem){
  tmp<-subset(Chem.tc.dt, CASRN==Chem.df$CAS[i])
  Chem.df[i,"ToxCast"]<-length(which(M==Chem.df$CAS[i]))
  Chem.df[i,"Min.AC50"]<-min(tmp$`AC 50`)
  Chem.df[i,"Max.AC50"]<-max(tmp$`AC 50`)
}

# Remove Inf
tmp<-cbind(as.numeric(Chem.df[,18]),as.numeric(Chem.df[,19]))
tmp[!is.finite(tmp)] <- NA
Chem.df[, 18:19] <- tmp


# Create report
#pdf("ToxVal.pdf", height=14, width=22)
png(file="ToxVal.png",width=4000,height=2400,res=200)
grid.table(Chem.df)
dev.off()
