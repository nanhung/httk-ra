if(!require(readr)){
  install.packages('readr'); required(readr)
}

if(!require(data.table)){
  install.packages('data.table'); required(data.table)
}


# read theoph 1cpt mcmc test result
theoph.c1.1<-as.data.frame(fread('1cpt.MCMC1.out'))
theoph.c1.2<-as.data.frame(fread('1cpt.MCMC2.out'))
