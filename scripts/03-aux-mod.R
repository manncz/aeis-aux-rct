# script: 03-aux-mod.R
# author: Charlotte Mann
# purpose: Fit auxiliary model on Texas schools not included in CTAI study

library(tidyr)
library(dplyr)
library(randomForest)
library(stringr)

#-----------------------------------------------------------------------------------------#

load("../data/HS_MSdata_prep.Rdata")

#==========================================================================================#
                                ## Fit High School Model ##
#==========================================================================================#

mod.dat.hs <- outRem_hs %>%
  select(CAMPUS, outhA08) %>%
  left_join(covsRem_hs, by = "CAMPUS") %>%
  select(!starts_with("GRDSPAN"))

start.time <- Sys.time()
set.seed(1845)
hsRF <- randomForest(outhA08 ~ . -CAMPUS, data = mod.dat.hs, importance = T)
end.time <- Sys.time()
end.time - start.time

print(hsRF)
check <- hsRF$importance

save(hsRF, file = "../output/models/hs_rem_mod.Rdata")

mod.dat.hs %>%
  summarize(mean_y = mean(outhA08), var_y = var(outhA08))

#==========================================================================================#
                              ## Fit Middle School Model ##
#==========================================================================================#

mod.dat.ms <- outRem_ms %>%
  select(CAMPUS, outmA08) %>%
  left_join(covsRem_ms, by = "CAMPUS") %>%
  select(!starts_with("GRDSPAN"))
         
start.time <- Sys.time()
set.seed(1845)
msRF <- randomForest(outmA08 ~ . -CAMPUS, data = mod.dat.ms, importance = T)
end.time <- Sys.time()
end.time - start.time

print(msRF)

save(msRF, file = "../output/models/ms_rem_mod.Rdata")

mod.dat.ms %>%
  summarize(mean_y = mean(outmA08), var_y = var(outmA08))
