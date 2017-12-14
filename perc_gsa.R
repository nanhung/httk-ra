library(sensitivity)
library(EnvStats) # to use truncation lognormal
library(RCurl) # to use getURL 
library(readr) # to use read_table2 

if(!require(sensitivity)) {
  install.packages("sensitivity"); require(sensitivity)}
if(!require(EnvStats)) {
  install.packages("EnvStats"); require(EnvStats)} # to access break formatting functions
