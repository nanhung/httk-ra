# Create the "data" directory first
# This vignette provides the code used to generate the virtual populations 
# for the ten subpopulations of interest, plus a non-obese adult subpopulation.

library("httk") # to httkpop_generate
library("data.table")
library("parallel") # to detectCores()


nsamp<-1000
#List subpop names
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
#List subpop gender specifications
gendernum <- c(rep(list(NULL),7), 
               list(list(Male=0, Female=1000)), 
               list(list(Male=1000, Female=0)), 
               list(list(Male=0, Female=1000)), 
               list(NULL))
#List subpop age limits in years
agelim<-c(list(c(0,79),
               c(6,11),
               c(12,19),
               c(20,65),
               c(66,79)),
          rep(list(c(0,79)),4),
          list(c(16,49)),
          list(c(20,50)))
#List subpop weight categories
bmi_category <- c(rep(list(c('Underweight', 
                             'Normal',
                             'Overweight',
                             'Obese')),5),
                  list('Obese', c('Underweight','Normal', 'Overweight')),
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
      nsamp = nsamp,
      gendernum = gendernum,
      agelim_years = agelim,
      weight_category = bmi_category)
    
    filepart <- switch(method,
                       'virtual individuals' = 'vi',
                       'direct resampling' = 'dr')
    saveRDS(object=pops,
            file=paste0(paste('data/httkpop',
                              filepart, ExpoCast_grp, 
                              sep='_'),
                        '.Rdata'))
    return(0)
  }, error = function(err){
    print(paste('Error occurred:', err))
    return(1)
  })
}

no_cores <- parallel::detectCores()
cluster <- parallel::makeCluster(no_cores, 
                                 outfile='subpopulations_parallel_out.txt')

evalout <- parallel::clusterEvalQ(cl=cluster,
                                  {library(data.table)
                                    library(httk)})
parallel::clusterExport(cl = cluster,
                        varlist = 'tmpfun')
#Set seeds on all workers for reproducibility
parallel::clusterSetRNGStream(cluster, 
                              TeachingDemos::char2seed("Nanhung Hsieh"))
out_vi <- parallel::clusterMap(cl=cluster,
                               fun = tmpfun,
                               gendernum=gendernum,
                               agelim=agelim,
                               bmi_category=bmi_category,
                               ExpoCast_grp = ExpoCast.group,
                               MoreArgs = list(nsamp = nsamp,
                                               method = 'virtual individuals'))
out_dr <- parallel::clusterMap(cl=cluster,
                               fun = tmpfun,
                               gendernum=gendernum,
                               agelim=agelim,
                               bmi_category=bmi_category,
                               ExpoCast_grp = ExpoCast.group,
                               MoreArgs = list(nsamp = nsamp,
                                               method = 'direct resampling'))
parallel::stopCluster(cluster)

# Read created data #############################
vi_Total<-readRDS("data/httkpop_vi_Total.Rdata")
View(vi_Total)
# Use this method to create prior knowledge
prior<-get_httk_params(vi_Total, chemcas="58-55-9", "1compartment", poormetab=F, fup.censor=T,
                       sigma.factor = 0.3, Clint.vary = TRUE, lod = 0.01)
prior<-get_httk_params(vi_Total, chemcas="58-55-9", "3compartmentss", poormetab=F, fup.censor=T,
                sigma.factor = 0.3, Clint.vary = TRUE, lod = 0.01)

##########################################################
## Not run:
#Simply generate a virtual population of 100 individuals,
#using the direct-resampling method
set.seed(42)
httkpop_generate(method='direct resampling', nsamp=100)
#Generate a population using the virtual-individuals method,
#including 80 females and 20 males,
#including only ages 20-65,
#including only Mexican American and
#Non-Hispanic Black individuals,
#including only non-obese individuals
vi_data<-httkpop_generate(method = 'virtual individuals',
                 gendernum=list(Female=80,
                                Male=20),
                 agelim_years=c(20,65),
                 reths=c('Mexican American',
                         'Non-Hispanic Black'),
                 weight_category=c('Underweight',
                                   'Normal',
                                   'Overweight'))

View(vi_Total)
## End(Not run)


