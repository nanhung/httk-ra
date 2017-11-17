if(!require(readxl)) {
  install.packages("readxl"); require(readxl)}
if(!require(gdata)) {
  install.packages("gdata"); require(gdata)}
if(!require(gridExtra)) {
  install.packages("gridExtra"); require(gridExtra)}
if(!require(httk)) {
  install.packages("httk"); require(httk)}


Chem<-c(
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

IRISno<-c(
  "0454",
  "0436",
  "0444",
  "0147",
  "0225",
  "0130",
  "0243",
  "0065",
  "0154",
  "0363",
  NA,
  "0160",
  "0086",
  "0038",
  "0026",
  "0014",
  "0122",
  "0152",
  "0285",
  "0121",
  "0327",
  "0277",
  "0141",
  "0426",
  "0453",
  "0268",
  "0442",
  "0347",
  "0135",
  "0235",
  "0272",
  "0152",
  "0524",
  "0324",
  "0302",
  "0692",
  NA,
  NA,
  NA,
  "0147",
  NA,
  NA
)

Chem.df <- data.frame(Chem, CAS, IRISno)
no.Chem<-nrow(Chem.df)



for (i in 1:4){
  URL <- paste("https://cfpub.epa.gov/ncea/iris/iris_documents/documents/subst/",Chem.df$IRISno[i],"_summary.pdf", sep = "")
  filename<-paste(Chem.df$Chem[i], "-", Chem.df$IRISno[i], ".pdf", sep="")
  download.file(URL, filename, mode = 'wb')
}




