if(!require(readr)){
  install.packages('readr'); required(readr)
}

# read theoph 1cpt mcmc test result
theoph.c1.1<-as.data.frame(fread('theoph_groups_1234.csv'))
theoph.c1.2<-as.data.frame(fread('theoph_groups_2345.csv'))
