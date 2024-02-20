# script: 02-aux-cv.R
# author: Charlotte Mann
# purpose: Cross validation in the auxiliary data

library(tidyr)
library(dplyr)
library(randomForest)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)

#-----------------------------------------------------------------------------------------#

load("data/HS_MSdata_prep.Rdata")

#==========================================================================================#
                              ## CROSS VALIDATION ##
#==========================================================================================#

mod.dat.hs <- outRem_hs %>%
  select(CAMPUS, outhA08) %>%
  left_join(covsRem_hs, by = "CAMPUS") %>%
  select(!starts_with("GRDSPAN"))

mod.dat.ms <- outRem_ms %>%
  select(CAMPUS, outmA08) %>%
  left_join(covsRem_ms, by = "CAMPUS") %>%
  select(!starts_with("GRDSPAN"))

var(mod.dat.hs$outhA08)
var(mod.dat.ms$outmA08)

set.seed(29)

folds_hs <- cut(sample(nrow(mod.dat.hs)), breaks = 10, labels = FALSE)
folds_ms <- cut(sample(nrow(mod.dat.ms)), breaks = 10, labels = FALSE)

numCores <- detectCores()
registerDoParallel(numCores)

cv.out <- foreach(k = 1:10) %dopar% {
  
  set.seed(k)
  
  # split data for fold k
  hs.train = mod.dat.hs[folds_hs != k,]
  hs.test  = mod.dat.hs[folds_hs == k,]
  
  ms.train = mod.dat.ms[folds_ms != k,]
  ms.test = mod.dat.ms[folds_ms == k,]
  
  # fit models
  preOLS_hs <- lm(outhA08 ~ prehA, data = hs.train)
  preRF_hs <- randomForest(outhA08 ~ prehA, data = hs.train)
  
  print(paste("running hs all random forest for fold",k))
  allRF_hs <- randomForest(outhA08 ~ . -CAMPUS, data = hs.train)
  
  preOLS_ms <- lm(outmA08 ~ premA, data = ms.train)
  preRF_ms <- randomForest(outmA08 ~ premA, data = ms.train)
  
  print(paste("running ms all random forest for fold",k))
  allRF_ms <- randomForest(outmA08 ~ . -CAMPUS, data = ms.train)
  
  # initiate data frames to save fold k model predictions
  pred.hs <- data.frame(CAMPUS = hs.test$CAMPUS, fold = k) 
  pred.ms <- data.frame(CAMPUS = ms.test$CAMPUS, fold = k)
  
  # test predictions
  pred.hs$preols <- predict(preOLS_hs, newdata = hs.test)
  pred.hs$prerf <- predict(preRF_hs, newdata = hs.test)
  pred.hs$allrf <- predict(allRF_hs, newdata = hs.test)
  
  pred.ms$preols <- predict(preOLS_ms, newdata = ms.test)
  pred.ms$prerf <- predict(preRF_ms, newdata = ms.test)
  pred.ms$allrf <- predict(allRF_ms, newdata = ms.test)
  
  print(paste("save data for fold",k))
  save(pred.hs, pred.ms, file = paste0("temp/preds_fold",k,".Rdata"))
  
  
  list(hs = pred.hs, ms = pred.ms)
  
}

stopImplicitCluster()

hspredscv <- foreach(k = 1:10, .combine = rbind) %do% {
  cv.out[[k]]$hs
}

mspredscv <- foreach(k = 1:10, .combine = rbind) %do% {
  cv.out[[k]]$ms
}

hs.cv <- mod.dat.hs %>%
  select(CAMPUS, outhA08)%>%
  left_join(hspredscv, by = "CAMPUS") %>%
  pivot_longer(preols:allrf, names_to = "mod", values_to = "pred") %>%
  group_by(mod, fold) %>%
  mutate(mean_obs = mean(outhA08)) %>%
  summarize(mse = mean((outhA08-pred)^2), r2 = 1 - sum((outhA08-pred)^2)/sum((outhA08 - mean_obs)^2))

ms.cv <- mod.dat.ms %>%
  select(CAMPUS, outmA08)%>%
  left_join(mspredscv, by = "CAMPUS") %>%
  pivot_longer(preols:allrf, names_to = "mod", values_to = "pred") %>%
  group_by(mod, fold) %>%
  mutate(mean_obs = mean(outmA08)) %>%
  summarize(mse = mean((outmA08-pred)^2), r2 = 1 - sum((outmA08-pred)^2)/sum((outmA08 - mean_obs)^2))


save(hspredscv, mspredscv, hs.cv, ms.cv,
     file = "../output/aux_cv.Rdata")


hs.cv %>%
  ungroup() %>%
  group_by(mod) %>%
  summarize(mse = mean(mse), r2 = 1- mean(r2))

ms.cv %>%
  ungroup() %>%
  group_by(mod) %>%
  summarize(mse = mean(mse), r2 = 1- mean(r2))
