# 1108

library("httk")

chem<-c(
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

Chem.df <- data.frame(chem, CAS)

Chem.df[,"ToxCast"]<-""
Chem.df[,"Tox21"]<-""
Chem.df[,"ExpoCast"]<-""
Chem.df[,"NHANES"]<-""

no.Chem <- length(Chem.df[,1]) # The number of chemicals

# Double check the excel table and httk (*some information are different*) 
for (this.cas in Chem.df$CAS[1:no.Chem])
{
  this.index <- Chem.df$CAS==this.cas
  if (is.nhanes(this.cas)) Chem.df[this.index,"NHANES"] <- 1  # 1 = yes
  if (is.tox21(this.cas)) Chem.df[this.index,"Tox21"] <- 1
  if (is.toxcast(this.cas)) Chem.df[this.index,"ToxCast"] <- 1 
  if (is.expocast(this.cas)) Chem.df[this.index,"ExpoCast"] <- 1
}


# Curl ExpoCast data
Chem.df[,"Expo.Total_median"]<-"" # median for total population
Chem.df[,"Expo.Total_95perc"]<-"" # 95% for total population


for(i in 1:no.Chem){
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][1], 44, 51)
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][1], 44, 51)
}

tmp.df <- Chem.df[grep(">", Chem.df$Expo.Total_median), ] # detect the wrong data
tmp.no <- as.numeric(row.names(tmp.df)) # detect the error row number

for(i in tmp.no){ # revise the correct value
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][2], 44, 51) # 1->2
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][2], 44, 51) # 1->2
}

#write.csv(Chem.df, file = "SF42.csv")
# Estimate CSS


Chem.df[,"Css.med_medpbtk.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.med_95pbtk.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.95perc_medpbtk.plasma.uM"]<-"" # 95% for total population
Chem.df[,"Css.95perc_95pbtk.plasma.uM"]<-"" # 95% for total population

for (i in 42:no.Chem){
  cas<-Chem.df$CAS[i]
  md<-as.numeric(Chem.df$Expo.Total_median[i])
  u95<-as.numeric(Chem.df$Expo.Total_95perc[i])
  a<-calc_mc_css(chem.cas=cas, which.quantile=.5, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T)
  b<-calc_mc_css(chem.cas=cas, which.quantile=.95, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T)

  Chem.df[i,"Css.med_medpbtk.plasma.uM"] <- md *a
  Chem.df[i,"Css.med_95pbtk.plasma.uM"] <- md * b
  Chem.df[i,"Css.95perc_medpbtk.plasma.uM"] <- u95 * a
  Chem.df[i,"Css.95perc_95pbtk.plasma.uM"] <- u95 * b
}

# 12 HEPTACHLOR EPOXIDE
# Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.
# 16 DI(2-ETHYLHEXYL)PHTHALATE
# Missing protein binding data for given species. Set default.to.human to true to substitute human value.
# 22 LEAD
# 23 CADMIUM
# 24 ZINC
# 36 MERCURIC CHLORIDE
# 40 CHROMIUM, HEXAVALENT
# 41 COBALT
# 42 NICKEL
# CAS number not found, use get_cheminfo() for valid CAS numbers or set chem.name= argument.

Chem.df[,"Css.med_med.3cpt.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.med_95.3cpt.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.95perc_med.3cpt.plasma.uM"]<-"" # 95% for total population
Chem.df[,"Css.95perc_95.3cpt.plasma.uM"]<-"" # 95% for total population

for (i in 37:no.Chem){
  cas<-Chem.df$CAS[i]
  md<-as.numeric(Chem.df$Expo.Total_median[i])
  u95<-as.numeric(Chem.df$Expo.Total_95perc[i])
  a<-calc_mc_css(chem.cas=cas, which.quantile=.5, output.units='uM', model='3compartmentss', httkpop=TRUE, default.to.human=T)
  b<-calc_mc_css(chem.cas=cas, which.quantile=.95, output.units='uM', model='3compartmentss', httkpop=TRUE, default.to.human=T)
  
  Chem.df[i,"Css.med_med.3cpt.plasma.uM"] <- md *a
  Chem.df[i,"Css.med_95.3cpt.plasma.uM"] <- md * b
  Chem.df[i,"Css.95perc_med.3cpt.plasma.uM"] <- u95 * a
  Chem.df[i,"Css.95perc_95.3cpt.plasma.uM"] <- u95 * b
}

# 12 HEPTACHLOR EPOXIDE
# Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.
# 16 DI(2-ETHYLHEXYL)PHTHALATE
# Missing protein binding data for given species. Set default.to.human to true to substitute human value.
# 22 LEAD
# 23 CADMIUM
# 24 ZINC
# 36 MERCURIC CHLORIDE
# 40 CHROMIUM, HEXAVALENT
# 41 COBALT
# 42 NICKEL
# CAS number not found, use get_cheminfo() for valid CAS numbers or set chem.name= argument.

#write.csv(Chem.df, file = "SF42.csv")