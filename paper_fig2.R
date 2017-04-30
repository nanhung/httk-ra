library('data.table')
library('GGally')
library('ggplot2')

subpop <- 'Age.20.65'
pop.vi <- readRDS(paste0('data/', paste('httkpop',
                                        'vi',
                                        subpop,
                                        sep='_'), '.Rdata'))
pop.dr <- readRDS(paste0('data/', paste('httkpop',
                                        'dr',
                                        subpop,
                                        sep='_'), '.Rdata'))