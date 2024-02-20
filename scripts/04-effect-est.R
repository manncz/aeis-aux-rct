# script: 04-effect-est.R
# author: Charlotte Mann
# purpose: Treatment effect estimation 

library(experiment)
library(tidyr)
library(dplyr)
library(randomForest)
library(stringr)

# install dRCT package if not already installed
# package for sample splitting estimator
library(devtools)
if(!("dRCT" %in% installed.packages())) install_github("manncz/dRCT/dRCT")
library(dRCT)

#-----------------------------------------------------------------------------------------#

load("../data/HS_MSdata.Rdata")
load("../data/HS_MSdata_prep.Rdata")
load("../data/xwalk/grade_xwalk.Rdata")
load("../output/models/ms_rem_mod.Rdata")
load("../output/models/hs_rem_mod.Rdata")

#==========================================================================================#
                                  ## Prepare CTAI Trial Data ##
#==========================================================================================#

trialdat <- schools %>%
  left_join(grd.xwalk, by = "CAMPUS") %>%
  mutate(out = case_when(schooltype08 == "HS" ~ outhA08,
                         schooltype08 == "MS" ~ outmA08),
          pre = case_when(schooltype08 == "HS" ~ prehA,
                          schooltype08 == "MS" ~ premA)) %>%
  dplyr::select(CAMPUS, type = schooltype08, grdlvl, trt, pair, out, pre)

# our generated `type` and `grdlvl` from the trial data are the same
checktype <- trialdat %>%
  select(CAMPUS, type, grdlvl) %>%
  mutate(grdlvl = paste0(grdlvl, "S"),
         same = grdlvl == type)
mean(checktype$same)

#==========================================================================================#
                              ## Difference in Means ##
#==========================================================================================#

result <- c()

# from `experiment` package by Imai (2022) 
dm_out <- ATEnocov(Y = out, Z = trt, data = trialdat, match = pair)

result["dm_tau"] = dm_out$ATE.est
result["dm_se"] = sqrt(dm_out$ATE.var)

# experiment package requires loading MASS, which then conflicts with the dplyr "select" function.
# therefore, unload both packages after they have been used
detach("package:experiment", unload = TRUE)
detach("package:MASS", unload = TRUE)

#==========================================================================================#
                     ## Sample Splitting Estimator w PRETEST ##
#==========================================================================================#

pre_out <- p_loop(Y = trialdat$out, Tr = trialdat$trt, Z = trialdat$pre, P = trialdat$pair,
                pred = p_ols_interp)

result["pre_tau"] = pre_out["tauhat"]
result["pre_se"] = sqrt(pre_out["varhat"])

#==========================================================================================#
  ## Sample Splitting Estimator w ALL COVARIATES (INCLUDING SCHOOL TYPE AND PRETEST) ##
#==========================================================================================#

covsE = bind_rows(covsE_hs, covsE_ms) %>%
  mutate(across(where(is.numeric),~case_when(is.na(.x) ~ 0, 
                                             TRUE ~ .x)))

covsE_all <- trialdat %>%
  select(CAMPUS, type) %>%
  left_join(covsE, by = "CAMPUS") %>%
  mutate(hs = as.numeric(type == "HS")) %>%
  select(!starts_with("GRDSPAN"), -type)

vars <- model.matrix(CAMPUS ~ -1 + ., data = covsE_all)

# takes a long time to run so only run once
# ploop_all_out <- p_loop(Y = trialdat$out, Tr = trialdat$trt, Z = vars, P = trialdat$pair,
#                       pred = p_rf_po)
# save(ploop_all_out, "../output/models/ploop_all_out.Rdata")

load("../output/models/ploop_all_out.Rdata")

result["ploopall_tau"] = ploop_all_out["tauhat"]
result["ploopall_se"] = sqrt(ploop_all_out["varhat"])

#==========================================================================================#
                ## Sample Splitting Estimator w AUXILIARY PREDICTIONS ##
#==========================================================================================#

#get remnant predictions
pred_hs = predict(hsRF, newdata = covsE_hs)
pred_hs = data.frame(CAMPUS = covsE_hs$CAMPUS, rempred = pred_hs)

pred_ms = predict(msRF, newdata = covsE_ms)
pred_ms = data.frame(CAMPUS = covsE_ms$CAMPUS, rempred = pred_ms)

preds = rbind(pred_ms, pred_hs)

trialdat <- trialdat %>%
  left_join(preds, by = "CAMPUS")

reploop_out <- p_loop(Y = trialdat$out, Tr = trialdat$trt, Z = trialdat$rempred, P = trialdat$pair,
                              pred = p_ols_interp) 

result["reploop_tau"] = reploop_out["tauhat"]
result["reploop_se"] = sqrt(reploop_out["varhat"])

#==========================================================================================#
                             ## FORMAT RESULT DATA ##
#==========================================================================================#

result.dat <- data.frame(val = result, est = names(result)) %>%
  separate(est, into = c("est","est_type"), fill = "right") %>%
  pivot_wider(names_from = est_type, values_from = val) %>%
  mutate(var = se^2,
         var_dm = max(var),
         re = var_dm / var)

save(result.dat, file = "../output/cta_result_dat.Rdata")  
