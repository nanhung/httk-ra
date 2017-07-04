for(i in 401:614){
  CAS<-Chem.df[i,2]
  tmp<-readLines(paste("https://comptox.epa.gov/dashboard/dsstoxdb/results?utf8=%E2%9C%93&search=", CAS, sep = ""))
  logP<-tmp[grep('>LogP:',tmp)+3]
  BP<-tmp[grep('>Boiling Point<',tmp)+3]
  totmed<-tmp[grep('>Total<',tmp)+1]
  tot95p<-tmp[grep('>Total<',tmp)+2]
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
  #
  if (identical(BP, character(0)) == TRUE) {
    Chem.df[i,"Boiling Point Ave.exp"] <- NA
  } else {
    Chem.df[i,"Boiling Point Ave.exp"]<-tmp[grep('>Boiling Point<',tmp)+3]
  }
  if (identical(BP, character(0)) == TRUE) {
    Chem.df[i,"Boiling Point Ave.prd"] <- NA
  } else {
    Chem.df[i,"Boiling Point Ave.prd"]<-tmp[grep('>Boiling Point<',tmp)+8]
  }
  if (identical(BP, character(0)) == TRUE) {
    Chem.df[i,"Boiling Point Med.exp"] <- NA
  } else {
    Chem.df[i,"Boiling Point Med.exp"]<-tmp[grep('>Boiling Point<',tmp)+13]
  }
  if (identical(BP, character(0)) == TRUE) {
    Chem.df[i,"Boiling Point Med.prd"] <- NA
  } else {
    Chem.df[i,"Boiling Point Med.prd"]<-tmp[grep('>Boiling Point<',tmp)+18]
  }
  if (identical(BP, character(0)) == TRUE) {
    Chem.df[i,"Boiling Point Rng.exp"] <- NA
  } else {
    Chem.df[i,"Boiling Point Rng.exp"]<-tmp[grep('>Boiling Point<',tmp)+23]
  }
  if (identical(BP, character(0)) == TRUE) {
    Chem.df[i,"Boiling Point Rng.prd"] <- NA
  } else {
    Chem.df[i,"Boiling Point Rng.prd"]<-tmp[grep('>Boiling Point<',tmp)+28]
  }
  if (identical(BP, character(0)) == TRUE) {
    Chem.df[i,"Boiling Point Deg"] <- NA
  } else {
    Chem.df[i,"Boiling Point Deg"]<-"C"
  }
  if (identical(totmed, character(0)) == TRUE) {
    Chem.df[i,"Expo.Total_median"] <- NA
  } else if (length(totmed) == 2) {
    Chem.df[i,"Expo.Total_median"] <- sub('^.*<p [^>]*>([^<]*)</p>.*$', "\\1", totmed)[2]
  } else 
    Chem.df[i,"Expo.Total_median"] <- sub('^.*<p [^>]*>([^<]*)</p>.*$', "\\1", totmed)  
  if (identical(tot95p, character(0)) == TRUE) {
    Chem.df[i,"Expo.Total_95perc"] <- NA
  } else if (length(tot95p) == 2) {
    Chem.df[i,"Expo.Total_95perc"] <- sub('^.*<p [^>]*>([^<]*)</p>.*$', "\\1", tot95p)[2]
  } else 
    Chem.df[i,"Expo.Total_95perc"] <- sub('^.*<p [^>]*>([^<]*)</p>.*$', "\\1", tot95p)  
}