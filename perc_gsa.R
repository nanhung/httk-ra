library(sensitivity)
library(EnvStats) # to use truncation lognormal
library(RCurl) # to use getURL 
library(readr) # to use read_table2 

if(!require(sensitivity)) {
  install.packages("sensitivity"); require(sensitivity)}
if(!require(EnvStats)) {
  install.packages("EnvStats"); require(EnvStats)} # to access break formatting functions

# Set the prior information for the testing parameters
# Physiological parameter
LeanBodyWt = 66  # lean body weight (kg)
Flow_pul = 10.0 # Pulmonary ventilation rate (minute volume)
Vent_Perf = 1.6  # ventilation over perfusion ratio

# Percent mass of tissues with ranges shown
Pct_M_fat  = .16  # % total body mass
Pct_LM_liv = .033  # liver, % of lean mass
Pct_LM_wp  = .28  # well perfused tissue, % of lean mass
#Pct_LM_pp  = .70  # poorly perfused tissue, will be recomputed in scale

# Percent blood flows to tissues
Pct_Flow_fat = .07
Pct_Flow_liv = .25
#Pct_Flow_wp  = .50 
Pct_Flow_pp  = .20

# Tissue/blood partition coeficients (geometric mean)
PC_fat = 125 
PC_liv = 4.8
PC_wp  = 4.8 
PC_pp  = 1.6
PC_art = 12.0

sc_Vmax = .042   # scaling coeficient of body weight for Vmax
Km = 16

# Set minimun and maximum parameter value for Morris
# Use 1.6 to make sure the min-max can cover the poster
# the range is wider than prvious study
# min
LeanBodyWt_min <- exp(log(LeanBodyWt)-log(1.6))
Flow_pul_min <- exp(log(Flow_pul)-log(1.6))
Vent_Perf_min <- exp(log(Vent_Perf)-log(1.6))

Pct_M_fat_min <- exp(log(Pct_M_fat)-log(1.6))
Pct_LM_liv_min <- exp(log(Pct_LM_liv)-log(1.6))
Pct_LM_wp_min <- exp(log(Pct_LM_wp)-log(1.6))

Pct_Flow_fat_min <- exp(log(Pct_Flow_fat)-log(1.6))
Pct_Flow_liv_min <- exp(log(Pct_Flow_liv)-log(1.6))
Pct_Flow_pp_min <- exp(log(Pct_Flow_pp)-log(1.6))

PC_fat_min <- exp(log(PC_fat)-log(1.6))
PC_liv_min <- exp(log(PC_liv)-log(1.6))
PC_wp_min <- exp(log(PC_wp)-log(1.6))
PC_pp_min <- exp(log(PC_pp)-log(1.6))
PC_art_min <- exp(log(PC_art)-log(1.6))

sc_Vmax_min <- exp(log(sc_Vmax)-log(10))
Km_min <- exp(log(Km)-Km_trun*log(10))

# Max
LeanBodyWt_max <- exp(log(LeanBodyWt)+log(1.6))
Flow_pul_max <- exp(log(Flow_pul)+log(1.6))
Vent_Perf_max <- exp(log(Vent_Perf)+log(1.6))

Pct_M_fat_max <- exp(log(Pct_M_fat)+log(1.6))
Pct_LM_liv_max <- exp(log(Pct_LM_liv)+log(1.6))
Pct_LM_wp_max <- exp(log(Pct_LM_wp)+log(1.6))

Pct_Flow_fat_max <- exp(log(Pct_Flow_fat)+log(1.6))
Pct_Flow_liv_max <- exp(log(Pct_Flow_liv)+log(1.6))
Pct_Flow_pp_max <- exp(log(Pct_Flow_pp)+log(1.6))

PC_fat_max <- exp(log(PC_fat)+log(1.6))
PC_liv_max <- exp(log(PC_liv)+log(1.6))
PC_wp_max <- exp(log(PC_wp)+log(1.6))
PC_pp_max <- exp(log(PC_pp)+log(1.6))
PC_art_max <- exp(log(PC_art)+log(1.6))

sc_Vmax_max <- exp(log(sc_Vmax)+log(10))
Km_max <- exp(log(Km)-Km_trun*log(10))

#
set.seed(1234)
morr <- morris(model = NULL, factors = 16, r = 1024, 
               design = list(type = "oat", levels = 5, grid.jump = 3), 
               binf = c(LeanBodyWt_min,
                        Flow_pul_min,
                        Vent_Perf_min,
                        Pct_M_fat_min, 
                        Pct_LM_liv_min, 
                        Pct_LM_wp_min,
                        Pct_Flow_fat_min, 
                        Pct_Flow_liv_min, 
                        Pct_Flow_pp_min, 
                        PC_fat_min, 
                        PC_liv_min, 
                        PC_wp_min, 
                        PC_pp_min, 
                        PC_art_min,
                        sc_Vmax_min, 
                        Km_min
                        ),
               bsup = c(LeanBodyWt_max,
                        Flow_pul_max,
                        Vent_Perf_max,
                        Pct_M_fat_max, 
                        Pct_LM_liv_max, 
                        Pct_LM_wp_max,
                        Pct_Flow_fat_max, 
                        Pct_Flow_liv_max, 
                        Pct_Flow_pp_max, 
                        PC_fat_max, 
                        PC_liv_max, 
                        PC_wp_max, 
                        PC_pp_max, 
                        PC_art_max,
                        sc_Vmax_max, 
                        Km_max), scale = TRUE)                  

morr.perc.df <- cbind(1, morr$X) 
nrow(morr$X) # nrow=17408
write.table(morr.perc.df, file="perc.setpoint.dat", row.names=FALSE, sep="\t")
