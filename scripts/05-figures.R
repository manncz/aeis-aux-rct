# script: 05-figures.R
# author: Charlotte Mann
# purpose: CTAI study analysis figures and paper calculations

library(tidyr)
library(dplyr)
library(xtable)
library(ggplot2)
library(tibble)
library(xlsx)
library(tikzDevice)
library(stringr)
library(randomForest)

# MASS package causes problems for dplyr::select()
# detach("package:experiment", unload = TRUE)
# detach("package:MASS", unload = TRUE)

#-----------------------------------------------------------------------------------------#
load("../data/HS_MSdata.Rdata")
load("../data/HS_MSdata_prep.Rdata")

load("../output/cta_result_dat.Rdata")
load("../output/models/ms_rem_mod.Rdata")
load("../output/models/hs_rem_mod.Rdata")
load("../output/aux_cv.Rdata")

#==========================================================================================#
                    ## TREATMENT EFFECT ESTIMATION RESULT TABLE ##
#==========================================================================================#
result.dat$est
methods <- c("dm", "pre", "ploopall","reploop")
labs <- c("None", "Pretest", "All Covs (RCT)", "Auxiliary Prediction")

pre.var <- result.dat$var[result.dat$est == "pre"]
tab.dat <- result.dat %>%
  mutate(est = factor(est, levels = methods, labels = labs)) %>%
  mutate(re_pre =  pre.var/var) %>%
  select(est, tau, var, re, re_pre)

# ests and variance ratio compared to dm
xtab <- xtable(tab.dat %>% select(-re_pre), caption = "", table.placement = "ht", digits = 2)
print(xtab,  comment = F, include.rownames = F, only.contents = T,
      include.colnames = F, hline.after = c(nrow(xtab)),
      file='../output/figures/cta_results_all.tex')

# ests
xtab <- xtable(tab.dat %>% select(-starts_with("re")), caption = "", table.placement = "ht", digits = 2)
print(xtab,  comment = F, include.rownames = F, only.contents = T,
      include.colnames = F, hline.after = c(nrow(xtab)),
      file='../output/figures/cta_results_est_only.tex')


# variance ratio compared to dm and pretest
xtab <- xtable(tab.dat %>% select(-var, -tau), caption = "", table.placement = "ht", digits = 2)
print(xtab,  comment = F, include.rownames = F, only.contents = T,
      include.colnames = F, hline.after = c(nrow(xtab)),
      file='../output/figures/cta_results_re.tex')

#==========================================================================================#
                                ## VARIABLE IMPORTANCE ##
#==========================================================================================#

msImp <- importance(msRF) %>%
  data.frame() %>%
  select(perc_mse = X.IncMSE) %>%
  rownames_to_column(var = "var") %>%
  arrange(-perc_mse) %>%
  slice(1:30)

write.xlsx(msImp, file = "../data/temp/var_imp.xlsx", sheetName = "ms", append = T, row.names = F)


hsImp <- importance(hsRF) %>%
  data.frame() %>%
  select(perc_mse = X.IncMSE) %>%
  rownames_to_column(var = "var") %>%
  arrange(-perc_mse) %>%
  slice(1:30)

write.xlsx(hsImp, file = "../data/temp/var_imp.xlsx", sheetName = "hs", append = T, row.names = F)

# load back in sheets for which I manually added the variable descriptions based on information in
# the AEIS data dictionary and glossary

msImpClean <- read.xlsx(file = "../data/temp/var_imp.xlsx", sheetName = "ms_desc")
hsImpClean <- read.xlsx(file = "../data/temp/var_imp.xlsx", sheetName = "hs_desc")


# plots
ms.plotdat <- msImpClean %>%
  mutate(pre = case_when(var == "premA" ~ "Pretest", TRUE ~ "Other")) %>%
  mutate(desc = paste0(description, " (", year, ")")) %>%
  mutate(desc = str_remove(str_remove(desc, "Passing Rate "),"20"))

hs.plotdat <- hsImpClean %>%
  mutate(pre = case_when(var == "prehA" ~ "Pretest", TRUE ~ "Other")) %>%
  mutate(desc = paste0(description, " (", year, ")")) %>%
  mutate(desc = str_remove(str_remove(desc, "Passing Rate "),"20"))

# MS plot
gm <- ggplot(aes(x = perc_mse, y = reorder(desc, perc_mse), fill = pre), data = ms.plotdat) + 
  geom_col() +
  scale_fill_manual(values = c("#00441b", "#238b45")) +
  scale_y_discrete(position = "right") +
  scale_x_reverse() + 
  xlab("") + ylab("") +
  theme( panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      axis.text.y = element_text(size = 7.5, color = "black"),
      axis.text.x = element_text(size = 6),
      #plot.margin = margin(0,0,0,0),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())
              
gm 

tikz(file = "../output/figures/ms-var-imp.tex", width = 3.2, height = 2)
gm
dev.off()

# HS plot
gh <- ggplot(aes(x = perc_mse, y = reorder(desc, perc_mse), fill = pre), data = hs.plotdat) + 
  geom_col() +
  scale_fill_manual(values = c("#00441b", "#238b45")) +
  scale_y_discrete(position = "right") +
  scale_x_reverse() + 
  xlab("") + ylab("") +
  theme( panel.border = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         legend.position = "none",
         axis.text.y = element_text(size = 7.5, color = "black"),
         axis.text.x = element_text(size = 6),
         #plot.margin = margin(0,0,0,0),
         axis.title.x=element_blank(),
         axis.title.y=element_blank())

gh

tikz(file = "../output/figures/hs-var-imp.tex", width = 3.2, height = 2)
gh
dev.off()

#==========================================================================================#
                                ## CROSS-VALIDATION ##
#==========================================================================================#


ms <- ms.cv %>%
  mutate(type = "MS")

cv.dat <- hs.cv %>% 
  mutate(type = "HS") %>%
  rbind(ms) %>%
  ungroup() %>%
  group_by(type, mod) %>%
  summarize(mse = mean(mse), r2 = mean(r2)) %>%
  pivot_wider(names_from = type, values_from = c("mse","r2"), names_glue = "{type}_{.value}") %>%
  mutate(spacer = NA) %>%
  select(mod, starts_with("MS"), spacer, starts_with("HS")) %>%
  filter(mod != "prerf")%>%
  mutate(mod = factor(mod, levels = c("preols", "allrf"), labels = c("Pretest (OLS)", "All Predictors (RF)"))) %>%
  arrange(mod)

xtab <- xtable(cv.dat, caption = "", table.placement = "ht", digits = c(0,0,1,2,0,1,2))
print(xtab,  comment = F, include.rownames = F, only.contents = T,
      include.colnames = F, hline.after = c(nrow(xtab)),
      file='../output/figures/aux_cv_tab.tex')

#==========================================================================================#
                    ## RF IF ONLY FIT ON TRAIL SCHOOLS ##
#==========================================================================================#

var(outRem_hs$outhA08)
var(outRem_ms$outmA08)

var(schools$outhA08[schools$grdlvl=="H"])
var(schools$outmA08[schools$grdlvl=="M"])

e.dat.hs <- schools %>%
  filter(grdlvl == "H") %>%
  select(CAMPUS, outhA08) %>%
  left_join(covsE_hs, by = "CAMPUS") %>%
  select(!starts_with("GRDSPAN"))

start.time <- Sys.time()
set.seed(1845)
hsRFe <- randomForest(outhA08 ~ . -CAMPUS, data = e.dat.hs, importance = T)
end.time <- Sys.time()
end.time - start.time

print(hsRFe)
check <- importance(hsRFe)

e.dat.ms <- schools %>%
  filter(grdlvl == "M") %>%
  select(CAMPUS, outmA08) %>%
  left_join(covsE_ms, by = "CAMPUS") %>%
  select(!starts_with("GRDSPAN"))

start.time <- Sys.time()
set.seed(1845)
msRFe <- randomForest(outmA08 ~ . -CAMPUS, data = e.dat.ms, importance = T)
end.time <- Sys.time()
end.time - start.time

print(msRFe)
check <- importance(msRFe)