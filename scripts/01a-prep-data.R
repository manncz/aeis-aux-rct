# script: 01a-prep-data
# author: Charlotte Mann
# purpose: Pre-processing of AEIS data for analysis


library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

#-----------------------------------------------------------------------------------#

load("../data/HS_MSdata.Rdata")
load("../data/xwalk/grade_xwalk.Rdata")

#==========================================================================================#
                        ##  INDICATE SCHOOL START YEAR IN DATA ##
#==========================================================================================#

joincols <-colnames(grd.xwalk)[colnames(grd.xwalk) %in% colnames(covsE_noscale)]

# scale all schools together but HS and MS data separately
allcovs <- rbind(covsE_noscale, covsRem_noscale) %>%
  left_join(grd.xwalk, by = joincols) %>%
  select(-outtype08, -outtype09, -schooltype09, -DISTRICT, -COUNTY,
         -REGION, -GRDTYPE, -CFLCHART, -DTYPE, -C_RATING_67)

# adapted from code by Jaiying Wang (https://github.com/ashhhleywang/Stats-Research-Project)
exist_inds <- allcovs %>%
  mutate(nomiss34 = rowSums(!is.na(select(.,  matches("_34$")))),
         nomiss45 = rowSums(!is.na(select(.,  matches("_45$")))),
         nomiss56 = rowSums(!is.na(select(.,  matches("_56$")))),
         nomiss67 = rowSums(!is.na(select(.,  matches("_67$")))),
         nomiss78 = rowSums(!is.na(select(.,  matches("_78$"))))) %>%
  select(CAMPUS, starts_with("nomiss")) %>%
  mutate(exist34 = case_when(nomiss34 == 0 ~ 0,
                             TRUE ~ 1),
         exist45 = case_when(nomiss45 == 0 ~ 0,
                             TRUE ~ 1),
         exist56 = case_when(nomiss56 == 0 ~ 0,
                             TRUE ~ 1),
         exist67 = case_when(nomiss67 == 0 ~ 0,
                             TRUE ~ 1),
         exist78 = case_when(nomiss78 == 0 ~ 0,
                             TRUE ~ 1),
         ) %>%
  select(CAMPUS, starts_with("exist")) %>%
  mutate(keep = rowSums(select(., starts_with("exist"))) > 0)

# join year exist data and remove campuses that have no covariate information
allcovs <- exist_inds %>%
  left_join(allcovs, by = "CAMPUS") %>%
  filter(keep) %>%
  select(-keep)

#==========================================================================================#
                                  ## HS v MS Data ##
#==========================================================================================#

# there are 137 schools that don't have 2008 outcomes, for which `schooltype08` is missing

hscovs <- allcovs %>%
  filter(schooltype08 == "HS") %>%
  select(-schooltype08)

mscovs <- allcovs %>%
  filter(schooltype08 == "MS") %>%
  select(-schooltype08)

#==========================================================================================#
                                      ## SCALING ##
#==========================================================================================#

# create own function because base R function creates weird column names below
vecScale <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

covsHS_scaled <- hscovs %>%
  mutate(across(.cols = where(is.numeric) & !starts_with("exist"), vecScale))

covsMS_scaled <- mscovs %>%
  mutate(across(.cols = where(is.numeric) & !starts_with("exist"), vecScale))

#==========================================================================================#
                              ## ADDRESS MISSING VALUES ##
#==========================================================================================#

# based on work by Jiaying Wang, we determined that simple mean imputation plus adding indicators
# for whether a value was missing performed well in modeling


#-----------------------------       High Schools     ------------------------------------#

# mean imputation (since already scaled, replace with 0)
covsHS <- covsHS_scaled %>%
  mutate(across(where(is.numeric) & !starts_with("exist"), 
                ~case_when(is.na(.x) ~ 0,
                           TRUE ~ .x)))

# missing value indicators for every column
all_miss_inds <- covsHS_scaled %>%
  mutate(across(where(is.numeric) & !starts_with("exist"), 
                ~case_when(is.na(.x) ~ 1,
                           TRUE ~ 0),
                .names = "{.col}_mis"))

# columns that are missing for > 60% of high schools
all_miss <- all_miss_inds %>%
  select(ends_with("mis")) %>%
  summarize_all(mean)

hsmisscols <- str_replace(colnames(all_miss)[all_miss[1,] > .6], "_mis", "")

# unique missing indicator columns
miss_cols <-all_miss_inds %>%
  select(CAMPUS, ends_with("mis")) %>%
  select(!contains(hsmisscols)) %>%
  column_to_rownames(var = "CAMPUS") %>%
  as.matrix() %>%
  t() %>%
  data.frame() %>%
  distinct() %>% 
  as.matrix() %>%
  t() %>%
  data.frame() %>%
  rownames_to_column(var = "CAMPUS") %>%
  mutate(CAMPUS = str_remove(CAMPUS, "X"))

covsHS <- covsHS %>%
  select(!contains(hsmisscols)) %>%
  left_join(miss_cols, by = "CAMPUS")

#-----------------------------      Middle Schools     ------------------------------------#

# mean imputation (since already scaled, replace with 0)
covsMS <- covsMS_scaled %>%
  mutate(across(where(is.numeric) & !starts_with("exist"), 
                ~case_when(is.na(.x) ~ 0,
                           TRUE ~ .x)))

# missing value indicators for every column
all_miss_indsm <- covsMS_scaled %>%
  mutate(across(where(is.numeric) & !starts_with("exist"), 
                ~case_when(is.na(.x) ~ 1,
                           TRUE ~ 0),
                .names = "{.col}_mis"))

# columns that are missing for > 60% of high schools
all_missm <- all_miss_indsm %>%
  select(ends_with("mis")) %>%
  summarize_all(mean)
msmisscols <- str_replace(colnames(all_missm)[all_missm[1,] > .6], "_mis", "")

# unique missing indicator columns
miss_colsm <-all_miss_indsm %>%
  select(CAMPUS, ends_with("mis")) %>%
  select(!contains(msmisscols)) %>%
  column_to_rownames(var = "CAMPUS") %>%
  as.matrix() %>%
  t() %>%
  data.frame() %>%
  distinct() %>% 
  as.matrix() %>%
  t() %>%
  data.frame() %>%
  rownames_to_column(var = "CAMPUS") %>%
  mutate(CAMPUS = str_remove(CAMPUS, "X"))

covsMS <- covsMS %>%
  select(!contains(msmisscols)) %>%
  left_join(miss_colsm, by = "CAMPUS")

#==========================================================================================#
                            ## ONE HOT ENCODE CATEGORIAL VARS ##
#==========================================================================================#

gradeDummyvars_hs <- covsHS %>%
  select(CAMPUS, GRDSPAN) %>%
  model.matrix(CAMPUS ~ -1+GRDSPAN, data = .) %>%
  data.frame() %>%
  select_all(~str_replace_all(.,"\\.", "")) %>%
  mutate(CAMPUS = covsHS$CAMPUS)

covsHS <- covsHS %>%
  left_join(gradeDummyvars_hs, by = "CAMPUS") %>%
  select(CAMPUS, starts_with("exist"), starts_with("GRDSPAN"), everything())

gradeDummyvars_ms <- covsMS %>%
  select(CAMPUS, GRDSPAN) %>%
  model.matrix(CAMPUS ~ -1+GRDSPAN, data = .) %>%
  data.frame() %>%
  select_all(~str_replace_all(.,"\\.", "")) %>%
  mutate(CAMPUS = covsMS$CAMPUS)

covsMS <- covsMS %>%
  left_join(gradeDummyvars_ms, by = "CAMPUS") %>%
  select(CAMPUS, starts_with("exist"), starts_with("GRDSPAN"), everything())


#==========================================================================================#
                                      ## SAVE DATA ##
#==========================================================================================#

covsE_hs = covsHS %>%
  filter(CAMPUS %in% schools$CAMPUS)
covsE_ms = covsMS %>%
  filter(CAMPUS %in% schools$CAMPUS)

covsRem_hs = covsHS %>%
  filter(!(CAMPUS %in% schools$CAMPUS))
covsRem_ms = covsMS %>%
  filter(!(CAMPUS %in% schools$CAMPUS))

outRem_hs = outRem %>%
  filter(CAMPUS %in% covsRem_hs$CAMPUS)
outRem_ms = outRem %>%
  filter(CAMPUS %in% covsRem_ms$CAMPUS)

## save all of the data
save(covsE_hs, covsE_ms,
     covsRem_hs, covsRem_ms,
     outRem_hs, outRem_ms,
     file='../data/HS_MSdata_prep.RData')
