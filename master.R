library(tcpl) # for tox21 AC50
library(httk) # for toxcast AC50


# Generate the main data frame
Chem.df<-read.csv("ChemData.csv")
Chem.df[,"ToxCast"]<-""
Chem.df[,"Tox21"]<-""
Chem.df[,"ExpoCast"]<-""
Chem.df[,"NHANES"]<-""

no.Chem <- length(Chem.df[,1])

# Double check the excel table and httk (*some information are different*) 
for (this.cas in Chem.df$CAS[1:no.Chem])
{
  this.index <- Chem.df$CAS==this.cas
  if (is.nhanes(this.cas)) Chem.df[this.index,"NHANES"] <- 1
  if (is.tox21(this.cas)) Chem.df[this.index,"Tox21"] <- 1 
  if (is.toxcast(this.cas)) Chem.df[this.index,"ToxCast"] <- 1 
  if (is.expocast(this.cas)) Chem.df[this.index,"ExpoCast"] <- 1
}

View(Chem.df)

# Generate Toxicity Table w/ AC50
tc.dt.sub <- tc.dt[`Activity Call`=="Active", 
                   .(`Chemical Name`, CASRN, `Assay Endpoint`, `Activity Call`, `AC 50`)]

Chem.tc.dt <- tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[1,3],]
for(i in 2:no.Chem){
  Chem.tc.dt <- rbind(Chem.tc.dt, tc.dt.sub[tc.dt.sub$CASRN %in% Chem.df[i,3],])
}

View(Chem.tc.dt)

# Summarize the no. of toxicity, min-AC50, and max-AC50 in the dataset 
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

# Find Tox21 data
Chem.t21.dt <- Chem.tc.dt[grep("Tox21", Chem.tc.dt$`Assay Endpoint`)]
View(Chem.t21.dt)




CAS<-Chem.df[5,3]
s<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
#grep('>Total<',s)
#s[grep('>Total<',s)+1]
#s[grep('>Total<',s)+2]

substr(s[grep('>Total<',s)+1], 44, 51)
substr(s[grep('>Total<',s)+2], 44, 51)
