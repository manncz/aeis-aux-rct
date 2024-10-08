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

# combine processed covariate data for middle schools
covs_ms <- rbind(covsE_ms, covsRem_ms)

# also combine covariate data from before the major processing in 01a-prep-data.R
keepcols <- intersect(colnames(covsE_ms), colnames(covsE_noscale))

covs_ms_noprep <- covsE_noscale %>%
  rbind(covsRem_noscale) %>%
  filter(CAMPUS %in% covs_ms$CAMPUS) %>%
  select(all_of(keepcols), DISTRICT, COUNTY, REGION, GRDTYPE, CFLCHART, DTYPE, C_RATING_67)

# combine processed outcome data for middle schools
join.cols <- colnames(schools)[colnames(schools) %in% colnames(outRem_ms)]

out_ms <- schools %>%
  filter(grdlvl == "M") %>%
  select(all_of(join.cols)) %>%
  rbind(outRem_ms)
  
# shuffle
shuffleid <- sample(nrow(covs_ms), nrow(covs_ms))

covs_ms <- covs_ms[shuffleid, ]
covs_ms_noprep <- covs_ms_noprep[shuffleid, ]
out_ms <- out_ms[shuffleid, ]

grd_xwalk <- grd.xwalk %>%
  filter(CAMPUS %in% out_ms$CAMPUS)
grd_xwalk <- grd_xwalk[match(out_ms$CAMPUS, grd_xwalk$CAMPUS),]

save(covs_ms, out_ms, covs_ms_noprep, grd_xwalk, file = "../data/MS_data_public.Rdata")
