# https://stackoverflow.com/questions/41368628/read-excel-file-from-a-url-using-the-readxl-package

if(!require(readxl)) {
  install.packages("readxl"); require(readxl)}
if(!require(gridExtra)) {
  install.packages("gridExtra"); require(gridExtra)}

df<-read.csv('SF42.csv')

rsl.tb <- read_excel("2245074.xls")

RSL <- rsl.tb[rsl.tb$'CAS No.' %in% df[,3],]

no.Chem<-nrow(df)

for(i in 2:no.Chem){
  RSL <- rbind(RSL, rsl.tb[rsl.tb$'CAS No.' %in% df[i,2],])
}

names(RSL)[15]<-"CASRN"
newRSL<-RSL[, c(14:15,1:13,16:29)]

newRSL[42,1:2]<-as.matrix(df[30,2:3])
newRSL[43,1:2]<-as.matrix(df[37,2:3])

#write.csv(newRSL, file = "RSLTV.csv")


pdf("RSLTV.pdf", height=20, width=12)
grid.table(newRSL[,1:10])
dev.off()