#rm(list=ls())

if(!require(readxl)) {
  install.packages("readxl"); require(readxl)}
if(!require(httk)) {
  install.packages("httk"); require(httk)}
if(!require(pdftools)) {
  install.packages("pdftools"); require(pdftools)}

Chem.df<-as.data.frame(read_excel("Chemicals_and_drugs.xlsx")[,c(1:2)])

no.Chem<-nrow(Chem.df)

Chem.df[,"MW"]<-"" 

# Curl ExpoCast data
Chem.df[,"Expo.Total_median"]<-"" # median for total population
Chem.df[,"Expo.Total_95perc"]<-"" # 95% for total population

for(i in 1:no.Chem){
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][1], 44, 51)
  Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][1], 44, 51)
  Chem.df[i,"MW"]<-substr(tmp[grep('>Average Mass:<',tmp)][1], 55, 60)
  if(grepl('>', Chem.df$Expo.Total_median[i]) == TRUE){ # fix error 
    Chem.df[i,"Expo.Total_median"]<-substr(tmp[grep('>Total<',tmp)+1][2], 44, 51) # 1->2
    Chem.df[i,"Expo.Total_95perc"]<-substr(tmp[grep('>Total<',tmp)+2][2], 44, 51) # 1->2
  }
}


#
Chem.df[,"Css.medpbtk.plasma.uM"]<-"" 
Chem.df[,"Css.95pbtk.plasma.uM"]<-"" 
Chem.df[,"Css.upr_medpbtk.plasma.uM"]<-"" 
Chem.df[,"Css.upr_95pbtk.plasma.uM"]<-"" 
Chem.df[,"Css.lwr_medpbtk.plasma.uM"]<-"" 
Chem.df[,"Css.lwr_95pbtk.plasma.uM"]<-"" 


for (i in 1:no.Chem){
  cas<-Chem.df$CAS[i]
  upr<-as.numeric(Chem.df$Expo.Total_median[i])
  lwr<-as.numeric(Chem.df$Expo.Total_95perc[i])
  
  # Use tryCatch to prevent the stopping by error
  a<-tryCatch(calc_mc_css(chem.cas=cas, which.quantile=.5, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T), error=function(err) NA)
  b<-tryCatch(calc_mc_css(chem.cas=cas, which.quantile=.95, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T), error=function(err) NA)
  
  Chem.df[i,"Css.medpbtk.plasma.uM"] <- a
  Chem.df[i,"Css.95pbtk.plasma.uM"] <-b
  Chem.df[i,"Css.upr_medpbtk.plasma.uM"] <- upr *a
  Chem.df[i,"Css.upr_95pbtk.plasma.uM"] <- upr * b
  Chem.df[i,"Css.lwr_medpbtk.plasma.uM"] <- lwr * a
  Chem.df[i,"Css.lwr_95pbtk.plasma.uM"] <- lwr * b
}


#
URL<-"https://www.cdc.gov/exposurereport/pdf/FourthReport_UpdatedTables_Volume1_Jan2017.pdf"
filename<-"CDCReport"
download.file(URL, filename, mode = 'wb')
txt <- pdf_text("CDCReport")
Chem.df[i,"CDC_Report"] <-""

for (i in 1:no.Chem){
  Chem.df[i,"CDC_Report"] <- any(grepl(Chem.df$CAS[i], txt))
}

second.no<-as.numeric(row.names(Chem.df[grep(TRUE, Chem.df$CDC_Report), ]))
second.MW<-as.numeric(Chem.df[grep(TRUE, Chem.df$CDC_Report), ]$MW)

Chem.df[,"volumn(page)"]<-""
Chem.df[,"Biomonitoring"]<-""
Chem.df[,"Conc_med"]<-""
Chem.df[,"Conc_med.lwr"]<-""
Chem.df[,"Conc_med.upr"]<-""
Chem.df[,"Conc_95p"]<-""
Chem.df[,"Conc_95p.lwr"]<-""
Chem.df[,"Conc_95p.upr"]<-""


vp<-c("2(55)","1(88)","2(23)","2(5)","2(7)","2(3)","1(80)",
      "1(64)","1(132)","2(37)","1(554)","1(566)","1(108)","2(543)")
bm<-c("Srm","Urn","Srm","Srm","Srm","Srm","Urn",
      "Urn","Urn","Srm","Bld","Bld","Urn","Urn")
med<-c(0,0,0,0,0,0,0.23,
      0,0,14.9,0,0,0,0)
med.lwr<-c(0,0,0,0,0,0,0.18,
           0,0,14.2,0,0,0,0)
med.upr<-c(0,0,0,0,0,0,0.32,
           0,0,15.7,0,0,0,0)
c95p<-c(57.1,0,28.0,20.3,5.1,0,1.71,
       3.63,0.4,28.9,0,0.02,0,0)
c95p.lwr<-c(13.2,0,21.9,18.7,0,0,1.41,
            2.98,0.3,25.6,0,0.01,0,0)
c95p.upr<-c(230,0,34.0,22.4,5.4,0,2.37,
            4.61,0.5,32.8,0,0.05,0,0)

for (i in 1:14){
  Chem.df[second.no[i],"volumn(page)"]<-vp[i]  
  Chem.df[second.no[i],"Biomonitoring"]<-bm[i] 
  Chem.df[second.no[i],"Conc_med"]<-med[i]/second.MW[i]
  Chem.df[second.no[i],"Conc_med.lwr"]<-med.lwr[i]/second.MW[i] 
  Chem.df[second.no[i],"Conc_med.upr"]<-med.upr[i]/second.MW[i] 
  Chem.df[second.no[i],"Conc_95p"]<-c95p[i]/second.MW[i]
  Chem.df[second.no[i],"Conc_95p.lwr"]<-c95p.lwr[i]/second.MW[i]
  Chem.df[second.no[i],"Conc_95p.upr"]<-c95p.upr[i]/second.MW[i]
}

Chem.df[Chem.df==""]  <- NA 


View(Chem.df)
write.csv(Chem.df, file = "0126_ChemList.csv", row.names=F)



