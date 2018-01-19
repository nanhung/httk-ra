if(!require(httk)) {
  install.packages("httk"); require(httk)}
if(!require(stringr)) {
  install.packages("stringr"); require(stringr)}

# Generate the main data frame
df<-read.csv("ToxValFull_data_ouput-corrected_Liver_noncancer.csv", row.names = NULL)
Chem.df<-df[,1:2]

##
no.Chem <- length(Chem.df[,1]) # The number of chemicals

# Add dash line to CAS
reverse_int <- function(n) {
  t1 <- floor(log10(n))
  t2 <- 0
  for (i in t1:1) t2 <- t2 + floor(n/10^i) * 10^(t1-i)
  return(n*10^t1 - 99*t2)
}

df2<-as.data.frame(Chem.df[,2])
df2[,"rev"]<-""
df2[,"dash"]<-""
Chem.df[,"CAS"]<-""

for(i in 1:no.Chem){
  df2[i,"rev"]<-reverse_int(df2[i,1])
  df2[i,"dash"]<-str_replace_all(df2[i,"rev"], "(?<=^\\d{1}|^\\d{3})", "-")
  Chem.df[i,"CAS"]<-stringi::stri_reverse(df2[i,3])
}

#
Chem.df[49,3]<-"135410-20-7"
Chem.df[65,3]<-Chem.df[66,3]<-"129909-90-6"
Chem.df[70,3]<-Chem.df[66,3]<-"131860-33-8"
Chem.df[80,3]<-Chem.df[81,3]<-"177406-68-7"
Chem.df[80,3]<-Chem.df[81,3]<-"32536-52-0"
Chem.df[83:86,3]<-"32536-52-0"
Chem.df[95,3]<-"149877-41-8"

# 
Chem.df[,"httk"]<-""
Chem.df[,"ToxCast"]<-""
Chem.df[,"Tox21"]<-""
Chem.df[,"NHANES"]<-""
Chem.df[,"ExpoCast"]<-""
Chem.df[,"Boiling Point Ave.exp"]<-""
Chem.df[,"Boiling Point Ave.prd"]<-""
Chem.df[,"Boiling Point Med.exp"]<-""
Chem.df[,"Boiling Point Med.prd"]<-""
Chem.df[,"Boiling Point Rng.exp"]<-""
Chem.df[,"Boiling Point Rng.prd"]<-""
Chem.df[,"Boiling Point Deg"]<-""
Chem.df[,"MW"]<-"" 
Chem.df[,"LogP Ave.exp"]<-""
Chem.df[,"LogP Ave.prd"]<-""
Chem.df[,"LogP Med.exp"]<-""
Chem.df[,"LogP Med.prd"]<-""
Chem.df[,"LogP Rng.exp"]<-""
Chem.df[,"LogP Rng.prd"]<-""


# Information ----

# Double check the excel table and httk (*some information are different*) 
for (this.cas in Chem.df$CAS[1:no.Chem])
{
  this.index <- Chem.df$CAS==this.cas
  if (is.httk(this.cas)) Chem.df[this.index,"httk"] <- 1  # 1 = yes
  if (is.tox21(this.cas)) Chem.df[this.index,"Tox21"] <- 1
  if (is.toxcast(this.cas)) Chem.df[this.index,"ToxCast"] <- 1 
  if (is.nhanes(this.cas)) Chem.df[this.index,"NHANES"] <- 1  # 1 = yes
  if (is.expocast(this.cas)) Chem.df[this.index,"ExpoCast"] <- 1
}


# Boiling point ----

for(i in 1:no.Chem){
  CAS<-Chem.df$CAS[i]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Boiling Point Ave.exp"]<-substr(tmp[grep('>Boiling Point<',tmp)+3][1], 31, 37)
  Chem.df[i,"Boiling Point Ave.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+8][1], 31, 37)
  Chem.df[i,"Boiling Point Med.exp"]<-substr(tmp[grep('>Boiling Point<',tmp)+13][1], 31, 37)
  Chem.df[i,"Boiling Point Med.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+18][1], 31, 37)
  Chem.df[i,"Boiling Point Rng.exp"]<-substr(tmp[grep('>Boiling Point<',tmp)+23][1], 35, 44)
  Chem.df[i,"Boiling Point Rng.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+28][1], 35, 44)
  Chem.df[i,"Boiling Point Deg"]<-substr(tmp[grep('>Boiling Point<',tmp)+33][1], 36, 37)
}

tmp.df <- Chem.df[grep("class=", Chem.df$`Boiling Point Ave.exp`), ] # detect the wrong data

for(i in tmp.df[,1]){
  CAS<-Chem.df$CAS[i]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Boiling Point Ave.exp"]<-""
  Chem.df[i,"Boiling Point Ave.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+3][1], 56, 58)
  Chem.df[i,"Boiling Point Med.exp"]<-""
  Chem.df[i,"Boiling Point Med.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+9][1], 56, 58)
  Chem.df[i,"Boiling Point Rng.prd"]<-""
  Chem.df[i,"Boiling Point Rng.prd"]<-substr(tmp[grep('>Boiling Point<',tmp)+15][1], 60, 69)
  Chem.df[i,"Boiling Point Deg"]<-substr(tmp[grep('>Boiling Point<',tmp)+20][1], 61, 61)
}


# Molecular Weight ----

for(i in 1:no.Chem){
  CAS<-Chem.df$CAS[i]
  tmp<-readLines(paste("http://www.sigmaaldrich.com/catalog/search?term=", CAS,"&interface=CAS%20No.&N=0+&mode=partialmax&lang=en&region=US&focus=product", sep = "" ))
  match <- tmp[grep('Molecular Weight',tmp)][1]
  Chem.df[i,"MW"]<-gsub('^.*<span [^>]*>([^<]*)</span>.*$','\\1', match)
}

# LogP ----
for(i in 1:no.Chem){
  CAS<-Chem.df$CAS[i]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  logP<-tmp[grep('>LogP:',tmp)+3]
  if (identical(logP, character(0)) == TRUE) {
    Chem.df[i,"LogP Ave.exp"] <- NA
  } else {
    Chem.df[i,"LogP Ave.exp"]<-tmp[grep('>LogP:',tmp)+3]
  }
  if (identical(logP, character(0)) == TRUE) {
    Chem.df[i,"LogP Ave.prd"] <- NA
  } else {
    Chem.df[i,"LogP Ave.prd"]<-tmp[grep('>LogP:',tmp)+8]
  }
  if (identical(logP, character(0)) == TRUE) {
    Chem.df[i,"LogP Med.exp"] <- NA
  } else {
    Chem.df[i,"LogP Med.exp"]<-tmp[grep('>LogP:',tmp)+13]
  }
  if (identical(logP, character(0)) == TRUE) {
    Chem.df[i,"LogP Med.prd"] <- NA
  } else {
    Chem.df[i,"LogP Med.prd"]<-tmp[grep('>LogP:',tmp)+18]
  }
  if (identical(logP, character(0)) == TRUE) {
    Chem.df[i,"LogP Rng.exp"] <- NA
  } else {
    Chem.df[i,"LogP Rng.exp"]<-tmp[grep('>LogP:',tmp)+23]
  }
  if (identical(logP, character(0)) == TRUE) {
    Chem.df[i,"LogP Rng.prd"] <- NA
  } else {
    Chem.df[i,"LogP Rng.prd"]<-tmp[grep('>LogP:',tmp)+28]
  }
}


tmp.df <- Chem.df[grep("class=", Chem.df$`LogP Ave.exp`), ] # detect the wrong data

#for(i in 1:no.Chem){
for(i in as.numeric(row.names(tmp.df))){
  CAS<-Chem.df$CAS[i]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"LogP Ave.exp"]<-""
  Chem.df[i,"LogP Ave.prd"]<-substr(tmp[grep('>LogP:',tmp)+3][1], 56, 59)
  Chem.df[i,"LogP Med.exp"]<-""
  Chem.df[i,"LogP Med.prd"]<-substr(tmp[grep('>LogP:',tmp)+9][1], 56, 59)
  Chem.df[i,"LogP Rng.exp"]<-""
  Chem.df[i,"LogP Rng.prd"]<-substr(tmp[grep('>LogP:',tmp)+16][1], 35, 46)
}

