# Check
if(!require(httk)) {
  install.packages("httk"); require(httk)}

Chem.df<-read.csv("OrangeTox.csv", row.names=NULL)
no.Chem <- length(Chem.df[,1]) # The number of chemicals

#
Chem.df[,"ToxCast"]<-""
Chem.df[,"Tox21"]<-""
Chem.df[,"ExpoCast"]<-""
Chem.df[,"NHANES"]<-""


# Double check the excel table and httk (*some information are different*) 
for (this.cas in Chem.df$CAS[1:no.Chem])
{
  this.index <- Chem.df$CAS==this.cas
  if (is.nhanes(this.cas)) Chem.df[this.index,"NHANES"] <- 1  # 1 = yes
  if (is.tox21(this.cas)) Chem.df[this.index,"Tox21"] <- 1
  if (is.toxcast(this.cas)) Chem.df[this.index,"ToxCast"] <- 1 
  if (is.expocast(this.cas)) Chem.df[this.index,"ExpoCast"] <- 1
}

Chem.df[,"Expo.Total_median"]<-"" # median for total population
Chem.df[,"Expo.Total_95perc"]<-"" # 95% for total population

for(i in 480:no.Chem){
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][1], 44, 51)
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][1], 44, 51)
}



Chem.df[,"MW"]<-"" 

# grep from sigmaaldrich
for(i in 573:no.Chem){
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("http://www.sigmaaldrich.com/catalog/search?term=", CAS,"&interface=CAS%20No.&N=0+&mode=partialmax&lang=en&region=US&focus=product", sep = "" ))
  match <- tmp[grep('Molecular Weight',tmp)][1]
  Chem.df[i,"MW"]<-gsub('^.*<span [^>]*>([^<]*)</span>.*$','\\1', match)
}


tmp.df <- which(is.na(Chem.df[,9]), arr.ind=TRUE) # detect NA
tmp.df <- which(!is.na(Chem.df[,8]), arr.ind=TRUE) # detect non-NA
tmp.df




# grep from chem.nlm.nih.gov # 17:155 26:155 31:155
for(i in tmp.df[,1]){
#for(i in tmp.df[141:155,1]){
#for(i in tmp.df[17:155,1]){
#for(i in tmp.df[26:155,1]){
#for(i in tmp.df[31:155,1]){
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://chem.nlm.nih.gov/chemidplus/rn/", CAS, sep = "" ))
  match <- tmp[grep('Molecular Weight',tmp)][1]
  Chem.df[i,"MW"]<-gsub('^.*<li>([^<]*)</li>.*$','\\1', match)
}


#Chem.df[,"logP"]<-"" # median for total population
#Chem.df[,"EXP"]<-"" # 95% for total population


for(i in 604:no.Chem){
  CAS<-Chem.df[i,3]
  tmp<-readLines(paste("https://chem.nlm.nih.gov/chemidplus/rn/", CAS, sep = ""))
  logP <- tmp[grep('log P',tmp)+2]
  EXP <- tmp[grep('log P',tmp)+8]
  if (identical(logP, character(0)) == TRUE) {
    Chem.df[i,"logP"] <- NA
  } else {
    Chem.df[i,"logP"] <- sub('^.*<TD [^>]*>([^<]*)</TD>.*$', "\\1", logP)
  }
  if (identical(EXP, character(0)) == TRUE) {
    Chem.df[i,"EXP"] <- NA
  } else {
    Chem.df[i,"EXP"] <- sub('^.*<TD [^>]*>([^<]*)</TD>.*$', "\\1", EXP)
  }
}


#

#Chem.df[,"Expo.Total_median"]<-"" # median for total population
#Chem.df[,"Expo.Total_95perc"]<-"" # 95% for total population

#Chem.df[,"Boiling Point Ave.exp"]<-""
#Chem.df[,"Boiling Point Ave.prd"]<-""
#Chem.df[,"Boiling Point Med.exp"]<-""
#Chem.df[,"Boiling Point Med.prd"]<-""
#Chem.df[,"Boiling Point Rng.exp"]<-""
#Chem.df[,"Boiling Point Rng.prd"]<-""
#Chem.df[,"Boiling Point Deg"]<-""

#Chem.df[,"LogP Ave.exp"]<-""
#Chem.df[,"LogP Ave.prd"]<-""
#Chem.df[,"LogP Med.exp"]<-""
#Chem.df[,"LogP Med.prd"]<-""
#Chem.df[,"LogP Rng.exp"]<-""
#Chem.df[,"LogP Rng.prd"]<-""

#source("grepALL.R")


## Revise error
tmp.df <- cbind(c(1:614), Chem.df)
tmp.df <- tmp.df[grep("class=", tmp.df[,13]), ] # detect the wrong data
tmp.df[,1]

for(i in tmp.df[61:236,1]){ # Only Predited Boiling Point
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Boiling Point Ave.exp"]<-NA
  Chem.df[i,"Boiling Point Ave.prd"]<-tmp[grep('>Boiling Point<',tmp)+4]
  Chem.df[i,"Boiling Point Med.exp"]<-NA
  Chem.df[i,"Boiling Point Med.prd"]<-tmp[grep('>Boiling Point<',tmp)+10]
  Chem.df[i,"Boiling Point Rng.exp"]<-NA
  Chem.df[i,"Boiling Point Rng.prd"]<-tmp[grep('>Boiling Point<',tmp)+16]
  Chem.df[i,"Boiling Point Deg"]<-"C"
}


### Revise error
tmp.df <- cbind(c(1:614), Chem.df)
tmp.df <- tmp.df[grep("class=", tmp.df[,20]), ] # detect the wrong data
tmp.df[,1]

for(i in tmp.df[,1]){ # Only Predited LogP
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"LogP Ave.exp"]<-NA
  Chem.df[i,"LogP Ave.prd"]<-tmp[grep('>LogP:',tmp)+4]
  Chem.df[i,"LogP Med.exp"]<-NA
  Chem.df[i,"LogP Med.prd"]<-tmp[grep('>LogP:',tmp)+10]
  Chem.df[i,"LogP Rng.exp"]<-NA
  Chem.df[i,"LogP Rng.prd"]<-tmp[grep('>LogP:',tmp)+16]
}









truth <- sapply(Chem.df,is.character)
df1 <- data.frame(cbind(sapply(Chem.df[,truth],trimws,which="both"),Chem.df[,!truth]))

#write.csv(Chem.df, file = "ChemTox2.csv")
#write.csv(df1, file = "df1.csv")
