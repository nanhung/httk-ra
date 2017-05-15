library(httk)
library(data.table)
library(parallel)

seed.int <- TeachingDemos::char2seed('Nanhung Hsieh', set=FALSE)+1

nsamp<-1000
ExpoCast.group<-list("Total",
                     "Age.6.11",
                     "Age.12.19",
                     "Age.20.65",
                     "Age.GT65",
                     "BMIgt30",
                     "BMIle30",
                     "Females",
                     "Males",
                     "ReproAgeFemale",
                     "Age.20.50.nonobese")

gendernum <- c(rep(list(NULL),7), 
               list(list(Male=0, Female=1000)), 
               list(list(Male=1000, Female=0)), 
               list(list(Male=0, Female=1000)), 
               list(NULL))

agelim<-c(list(c(0,79),
               c(6,11),
               c(12,19),
               c(20,65),
               c(66,79)),
          rep(list(c(0,79)),4),
          list(c(16,49)),
          list(c(20,50)))

bmi_category <- c(rep(list(c('Underweight', 
                             'Normal',
                             'Overweight',
                             'Obese')),5),
                  list('Obese',
                       c('Underweight','Normal', 'Overweight')),
                  rep(list(c('Underweight', 
                             'Normal',
                             'Overweight',
                             'Obese')),3),
                  list(c('Underweight', 'Normal', 'Overweight')))

tmpfun <- function(gendernum, agelim, bmi_category, ExpoCast_grp,
                   nsamp, method){
  result <- tryCatch({
    pops <- httk::httkpop_generate(
      method=method,
      nsamp=nsamp,
      gendernum = gendernum,
      agelim_years = agelim,
      weight_category = bmi_category)
    
    filepart <- switch(method,
                       'virtual individuals' = 'vi',
                       'direct resampling' = 'dr')
    saveRDS(object=pops,
            file=paste0('data/httkpop_',
                        filepart,
                        '_',
                        ExpoCast_grp,
                        '2.Rdata')) #Note we've added a 2 to the file name!
    return(0)
  }, error = function(err){
    print(paste('Error occurred:', err))
    return(1)
  })
}
