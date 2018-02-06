

if(!require(httk)) {
  install.packages("httk"); require(httk)}

substance.name<-c("2-Chloronitrobenzene",
                  "4-Chloronitrobenzene",
                  "1,4-Dichloro-2-nitrobenzene",
                  "2,4-Dichloro-1-nitrobenzene",
                  "2-Amino-4-chlorophenol",
                  "ortho-Phenylenediamine dihydrochloride",
                  "para-Nitroanisole",
                  "N,N-Dimethylacetamide"
)

CAS<-c("88-73-3",
       "100-00-5",
       "89-61-2",
       "611-06-3",
       "95-85-2",
       "615-28-1",
       "100-17-4",
       "127-19-5")

Chem.df<-data.frame(substance.name,CAS)
no.Chem <- length(Chem.df[,1]) # The number of chemicals


Chem.df[,"httk"]<-""
Chem.df[,"ToxCast"]<-""
Chem.df[,"Tox21"]<-""
Chem.df[,"NHANES"]<-""
Chem.df[,"ExpoCast"]<-""


for (this.cas in Chem.df$CAS[1:no.Chem])
{
  this.index <- Chem.df$CAS==this.cas
  if (is.httk(this.cas)) Chem.df[this.index,"httk"] <- 1  # 1 = yes
  if (is.tox21(this.cas)) Chem.df[this.index,"Tox21"] <- 1
  if (is.toxcast(this.cas)) Chem.df[this.index,"ToxCast"] <- 1 
  if (is.nhanes(this.cas)) Chem.df[this.index,"NHANES"] <- 1  # 1 = yes
  if (is.expocast(this.cas)) Chem.df[this.index,"ExpoCast"] <- 1
}