library(httk) # for toxcast AC50

# Generate the main data frame
Chem.df<-read.csv("ChemData.csv")
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

#View(Chem.df)

# Generate Toxicity Table w/ AC50
tc.dt.sub <- tc.dt[`Activity Call`=="Active", 
                   .(`Chemical Name`, CASRN, `Assay Endpoint`, `Activity Call`, `AC 50`)]

Chem.tc.dt <- tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[1,3],]
for(i in 2:no.Chem){
  Chem.tc.dt <- rbind(Chem.tc.dt, tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[i,3],])
}

#View(Chem.tc.dt)

# Summarize the no. of toxicity data, min-AC50, and max-AC50 in the dataset 
M<-as.matrix(Chem.tc.dt[,2])
Chem.df[,"No.ToxData"]<-""
Chem.df[,"Min.AC50"]<-""
Chem.df[,"Max.AC50"]<-""

for(i in 1:no.Chem){
  tmp<-subset(Chem.tc.dt, CASRN==Chem.df$CAS[i])
  Chem.df[i,"No.ToxData"]<-length(which(M==Chem.df$CAS[i]))
  Chem.df[i,"Min.AC50"]<-min(tmp$`AC 50`)
  Chem.df[i,"Max.AC50"]<-max(tmp$`AC 50`)
}

tmp<-cbind(as.numeric(Chem.df[,13]),as.numeric(Chem.df[,14]))
tmp[!is.finite(tmp)] <- NA
Chem.df[, 13:14] <- tmp

View(Chem.df)

# Find Tox21 data in Chem.tc.dt
Chem.t21.dt <- Chem.tc.dt[grep("Tox21", Chem.tc.dt$`Assay Endpoint`)]
View(Chem.t21.dt)

# Curl ExpoCast data
Chem.df[,"Expo.Total_median"]<-"" # median for total population
Chem.df[,"Expo.Total_95perc"]<-"" # 95% for total population

# CAUTION! This step will take long time ~30 min
for(i in 1:no.Chem){
  CAS<-Chem.df[i,3]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][1], 44, 51)
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][1], 44, 51)
}

tmp.df <- Chem.df[grep(">", Chem.df$Expo.Total_median), ] # detect the wrong data

for(i in tmp.df[,1]){ # revise the correct value
  CAS<-Chem.df[i,3]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][2], 44, 51)
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][2], 44, 51)
}

write.csv(Chem.df, file = "ChemTox.csv")


# 0525 new master list----
Chem.df<-read.csv("ChemData0524.csv")
Chem.df[,"ToxCast"]<-""
Chem.df[,"Tox21"]<-""
Chem.df[,"ExpoCast"]<-""
Chem.df[,"NHANES"]<-""

no.Chem <- length(Chem.df$Order) # The number of chemicals

# Double check the excel table and httk (*some information are different*) 
for (this.cas in Chem.df$CAS[1:no.Chem])
{
  this.index <- Chem.df$CAS==this.cas
  if (is.nhanes(this.cas)) Chem.df[this.index,"NHANES"] <- 1  # 1 = yes
  if (is.tox21(this.cas)) Chem.df[this.index,"Tox21"] <- 1
  if (is.toxcast(this.cas)) Chem.df[this.index,"ToxCast"] <- 1 
  if (is.expocast(this.cas)) Chem.df[this.index,"ExpoCast"] <- 1
}

# Generate Toxicity Table w/ AC50
tc.dt.sub <- tc.dt[`Activity Call`=="Active", 
                   .(`Chemical Name`, CASRN, `Assay Endpoint`, `Activity Call`, `AC 50`)]

Chem.tc.dt <- tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[1,3],]
for(i in 2:no.Chem){
  Chem.tc.dt <- rbind(Chem.tc.dt, tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[i,3],])
}

M<-as.matrix(Chem.tc.dt[,2])
Chem.df[,"No.ToxData"]<-""
Chem.df[,"Min.AC50"]<-""
Chem.df[,"Max.AC50"]<-""

for(i in 1:no.Chem){
  tmp<-subset(Chem.tc.dt, CASRN==Chem.df$CAS[i])
  Chem.df[i,"No.ToxData"]<-length(which(M==Chem.df$CAS[i]))
  Chem.df[i,"Min.AC50"]<-min(tmp$`AC 50`)
  Chem.df[i,"Max.AC50"]<-max(tmp$`AC 50`)
}

tmp<-cbind(as.numeric(Chem.df[,13]),as.numeric(Chem.df[,14]))
tmp[!is.finite(tmp)] <- NA
Chem.df[, 13:14] <- tmp

View(Chem.df)

# Find Tox21 data in Chem.tc.dt
Chem.t21.dt <- Chem.tc.dt[grep("Tox21", Chem.tc.dt$`Assay Endpoint`)]
View(Chem.t21.dt)

# Curl ExpoCast data
Chem.df[,"Expo.Total_median"]<-"" # median for total population
Chem.df[,"Expo.Total_95perc"]<-"" # 95% for total population

# CAUTION! This step will take long time ~30 min
for(i in 1:no.Chem){
  CAS<-Chem.df[i,3]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][1], 44, 51)
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][1], 44, 51)
}

tmp.df <- Chem.df[grep(">", Chem.df$Expo.Total_median), ] # detect the wrong data

for(i in tmp.df[,1]){ # revise the correct value
  CAS<-Chem.df[i,3]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][2], 44, 51)
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][2], 44, 51)
}


# Estimate CSS
Chem.df<-read.csv("ChemTox_v2.csv", header = T)

Chem.df[,"httk"]<-""
Chem.df[,"Css.med_medRTK.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.med_95RTK.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.95perc_medRTK.plasma.uM"]<-"" # 95% for total population
Chem.df[,"Css.95perc_95RTK.plasma.uM"]<-"" # 95% for total population

# Double check the excel table and httk (*some information are different*) 
for (this.cas in Chem.df$CAS[1:no.Chem])
{
  this.index <- Chem.df$CAS==this.cas
  if (is.httk(this.cas)) Chem.df[this.index,"httk"] <- 1
}

tmp.df <- Chem.df[grep("1", Chem.df$httk), ] # detect the httk
tmp.df[,1]

for (i in tmp.df[,1]){
  cas<-Chem.df$CAS_trimmed[i]
  md<-Chem.df$Expo.Total_median[i]
  u95<-Chem.df$Expo.Total_95perc[i]
  a<-calc_mc_css(chem.cas=cas, which.quantile=.5, output.units='uM', model='3compartmentss', httkpop=FALSE)
  b<-calc_mc_css(chem.cas=cas, which.quantile=.95, output.units='uM', model='3compartmentss', httkpop=FALSE)
  ratio<-b/a
  Chem.df[i,"Css.med_medRTK.plasma.uM"] <- calc_analytic_css(chem.cas=cas, output.units='uM', model='3compartmentss', daily.dose=md)
  Chem.df[i,"Css.med_95RTK.plasma.uM"] <- Chem.df[i,"Css.med_medRTK.plasma.uM"] * ratio
  Chem.df[i,"Css.95perc_medRTK.plasma.uM"] <- calc_analytic_css(chem.cas=cas, output.units='uM', model='3compartmentss', daily.dose=u95)
  Chem.df[i,"Css.95perc_95RTK.plasma.uM"] <- Chem.df[i,"Css.95perc_medRTK.plasma.uM"] * ratio
  }

#write.csv(Chem.df, file = "ChemTox.csv")

cas<-Chem.df$CAS_trimmed[1]
md<-Chem.df$Expo.Total_median[1]
u95<-Chem.df$Expo.Total_95perc[1]

a<-calc_mc_css(chem.cas=cas, which.quantile=.5, output.units='uM', model='3compartmentss', httkpop=FALSE)
b<-calc_mc_css(chem.cas=cas, which.quantile=.95, output.units='uM', model='3compartmentss', httkpop=FALSE)
b/a


#
Chem.df<-read.csv("ChemTox_v2.csv", header = T)
no.Chem <- length(Chem.df[,1]) # The number of chemicals

Chem.df<-Chem.df[c(4:6)]
Chem.df[,"Ave.exp"]<-""
Chem.df[,"Ave.prd"]<-""
Chem.df[,"Med.exp"]<-""
Chem.df[,"Med.prd"]<-""
Chem.df[,"Rng.exp"]<-""
Chem.df[,"Rng.prd"]<-""
Chem.df[,"Deg"]<-""

for(i in 1:no.Chem){
  CAS<-Chem.df[i,3]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Ave.exp"]<-substr(tmp[grep('>Boiling Point<',tmp)+3][1], 31, 37)
  Chem.df[i,"Ave.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+8][1], 31, 37)
  Chem.df[i,"Med.exp"]<-substr(tmp[grep('>Boiling Point<',tmp)+13][1], 31, 37)
  Chem.df[i,"Med.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+18][1], 31, 37)
  Chem.df[i,"Rng.exp"]<-substr(tmp[grep('>Boiling Point<',tmp)+23][1], 35, 44)
  Chem.df[i,"Rng.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+28][1], 35, 44)
  Chem.df[i,"Deg"]<-substr(tmp[grep('>Boiling Point<',tmp)+33][1], 36, 37)
}

tmp.df <- Chem.df[grep("class=", Chem.df$Ave.exp), ] # detect the wrong data

for(i in tmp.df[,1]){
  CAS<-Chem.df[i,3]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Ave.exp"]<-""
  Chem.df[i,"Ave.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+3][1], 56, 58)
  Chem.df[i,"Med.exp"]<-""
  Chem.df[i,"Med.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+9][1], 56, 58)
  Chem.df[i,"Rng.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+15][1], 60, 69)
  Chem.df[i,"Deg"]<-substr(tmp[grep('>Boiling Point<',tmp)+20][1], 61, 61)
}

write.csv(Chem.df, file = "ChemBoil.csv")
