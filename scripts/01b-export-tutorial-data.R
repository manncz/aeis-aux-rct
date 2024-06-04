# script: 01b-export-tutorial-data
# author: Charlotte Mann
# original date: June 4, 2024
# purpose: Export publicly available data for EDM tutorial

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

#-----------------------------------------------------------------------------------#

# data outputs from 01-make-data.R and 01a-prep-data.R
load("../data/HS_MSdata.Rdata")
load("../data/xwalk/grade_xwalk.Rdata")
load("../data/HS_MSdata_prep.RData")

#-----------------------------------------------------------------------------------#

# combine data for middle schools

covs_ms <- rbind(covsE_ms, covsRem_ms)

join.cols <- colnames(schools)[colnames(schools) %in% colnames(outRem_ms)]

out_ms <- schools %>%
  select(all_of(join.cols)) %>%
  rbind(outRem_ms)

save(covs_ms, out_ms, grd.xwalk, file = "../data/MS_data_public.Rdata")
