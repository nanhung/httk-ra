if(!require(httk)) {
  install.packages("httk"); require(httk)}

substance.name<-c("2,4-D",
                  "HEXAZINONE",
                  "DIURON",
                  "IMAZAPIC",
                  "IMAZAPYR",
                  "ATRAZINE",
                  "GLUFOSINATE",
                  "METSULFURON METHYL",
                  "ACETOCHLOR",
                  "TRIFLOXYSULFURON",
                  "ISOXAFLUTOLE",
                  "PENDIMETHALIN",
                  "PICLORAM",
                  "TERBUTRYN",
                  "DICAMBA",
                  "AMETRYN",
                  "METOLACHLOR",
                  "HEPTACHLOR EPOXIDE",
                  "DI(2-ETHYLHEXYL)PHTHALATE",
                  "Cadmium Chloride",
                  "Zinc Chloride",
                  "NICKEL Chloride")

CAS<-c("94-75-7",
       "51235-04-2",
       "330-54-1",
       "104098-48-8",
       "81334-34-1",
       "1912-24-9",
       "77182-82-2",
       "74223-64-6",
       "34256-82-1",
       "145099-21-4",
       "141112-29-0",
       "40487-42-1",
       "1918-02-1",
       "886-50-0",
       "1918-00-9",
       "834-12-8",
       "51218-45-2",
       "1024-57-3",
       "117-81-7",
       "10108-64-2",
       "7646-85-7",
       "7718-54-9")

exposure.med<-c(3.85E-07,
                7.69E-08,
                1.31E-08,
                8.54E-08,
                9.23E-08,
                6.56E-08,
                1.22E-08,
                2.20E-08,
                9.51E-08,
                1.60E-07,
                1.97E-08,
                1.66E-08,
                8.08E-08,
                1.46E-08,
                1.26E-08,
                8.11E-08,
                8.02E-08,
                1.23E-07,
                1.70E-04,
                1.13E-08,
                5.19E-07,
                2.97E-08)

exposure.95th<-c(3.37E-05,
                 5.40E-06,
                 1.12E-06,
                 5.45E-06,
                 4.17E-06,
                 4.12E-06,
                 1.28E-06,
                 2.52E-06,
                 5.65E-06,
                 9.59E-06,
                 1.83E-06,
                 1.19E-06,
                 5.42E-06,
                 1.03E-06,
                 1.04E-06,
                 5.09E-06,
                 3.55E-06,
                 6.24E-06,
                 1.14E-02,
                 1.35E-06,
                 5.78E-05,
                 2.82E-06)

Chem.df <- data.frame(substance.name, CAS, exposure.med, exposure.95th)

no.Chem<-nrow(Chem.df)

Chem.df[,"Css.med_medpbtk.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.med_95pbtk.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.95perc_medpbtk.plasma.uM"]<-"" # 95% for total population
Chem.df[,"Css.95perc_95pbtk.plasma.uM"]<-"" # 95% for total population

for (i in 1:no.Chem){
  cas<-Chem.df$CAS[i]
  md<-as.numeric(Chem.df$exposure.med[i])
  u95<-as.numeric(Chem.df$exposure.95th[i])
  
  # Use tryCatch to prevent the stopping by error
  a<-tryCatch(calc_mc_css(chem.cas=cas, which.quantile=.5, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T, Funbound.plasma.pc.correction = FALSE), error=function(err) NA)
  b<-tryCatch(calc_mc_css(chem.cas=cas, which.quantile=.95, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T, Funbound.plasma.pc.correction = FALSE), error=function(err) NA)
  
  Chem.df[i,"Css.med_medpbtk.plasma.uM"] <- md *a
  Chem.df[i,"Css.med_95pbtk.plasma.uM"] <- md * b
  Chem.df[i,"Css.95perc_medpbtk.plasma.uM"] <- u95 * a
  Chem.df[i,"Css.95perc_95pbtk.plasma.uM"] <- u95 * b
}
write.csv(Chem.df, file = "Exposure2uM.csv")

#
substance.name<-c("BENZO(A)ANTHRACENE",
                  "HEXACHLOROCYCLOHEXANE, GAMMA-",
                  "ENDRIN",
                  "HEPTACHLOR EPOXIDE",
                  "DI(2-ETHYLHEXYL)PHTHALATE",
                  "BENZO(B)FLUORANTHENE",
                  "DDD, P,P'-",
                  "CRESOL, PARA-",
                  "DDT, O,P'-",
                  "Lead Chromate",
                  "Cadmium Chloride",
                  "Zinc Chloride",
                  "Methylmercury",
                  "Potassium Chromate",
                  "COBALT Chloride",
                  "NICKEL Chloride")

CAS<-c("56-55-3",
       "58-89-9",
       "72-20-8",
       "1024-57-3",
       "117-81-7",
       "205-99-2",
       "72-54-8",
       "106-44-5",
       "789-02-6",
       "7758-97-6",
       "10108-64-2",
       "7646-85-7",
       "22967-92-6",
       "7789-00-6",
       "7646-79-9",
       "7718-54-9")

RfD.Low<-c(0.0014,
           0.0003,
           5.00E-05,
           1.3E-05,
           0.02,
           0.00147,
           7.64E-05,
           0.0726,
           1.43E-04,
           0.0244,
           0.0005,
           0.00722,
           0.0295,
           0.0147,
           0.0133,
           0.00784)
RfD.High<-c(29.8,
            1.03,
            0.189,
            0.581,
            123,
            9.38,
            1.11,
            83.4,
            1.1,
            21.8,
            0.001,
            21.1,
            26.8,
            13.6,
            20.6,
            10.3)

Chem.df <- data.frame(substance.name, CAS, RfD.Low, RfD.High)

no.Chem<-nrow(Chem.df)

Chem.df[,"Css.low_medpbtk.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.low_95pbtk.plasma.uM"]<-"" # median for total population
Chem.df[,"Css.high_medpbtk.plasma.uM"]<-"" # 95% for total population
Chem.df[,"Css.high_95pbtk.plasma.uM"]<-"" # 95% for total population

for (i in 1:no.Chem){
  cas<-Chem.df$CAS[i]
  md<-as.numeric(Chem.df$RfD.Low[i])
  u95<-as.numeric(Chem.df$RfD.High[i])
  
  # Use tryCatch to prevent the stopping by error
  a<-tryCatch(calc_mc_css(chem.cas=cas, which.quantile=.5, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T, Funbound.plasma.pc.correction = FALSE), error=function(err) NA)
  b<-tryCatch(calc_mc_css(chem.cas=cas, which.quantile=.95, output.units='uM', model='pbtk', httkpop=TRUE, default.to.human=T, Funbound.plasma.pc.correction = FALSE), error=function(err) NA)
  
  Chem.df[i,"Css.low_medpbtk.plasma.uM"] <- md *a
  Chem.df[i,"Css.low_95pbtk.plasma.uM"] <- md * b
  Chem.df[i,"Css.high_medpbtk.plasma.uM"] <- u95 * a
  Chem.df[i,"Css.high_95pbtk.plasma.uM"] <- u95 * b
}

write.csv(Chem.df, file = "RfD2uM.csv")