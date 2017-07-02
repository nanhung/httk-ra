# Check
if(!require(httk)) {
  install.packages("httk"); require(httk)}

Chem.df<-read.csv("ChemTox2.csv")
no.Chem <- length(Chem.df[,1]) # The number of chemicals

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


tmp.df <- which(is.na(Chem.df), arr.ind=TRUE) # detect NA
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




#write.csv(Chem.df, file = "ChemTox2.csv")