## Data Build and Analysis for "Using Publicly Available Auxiliary Data to Improve Precision of Treatment Effect Estimation in a Randomized Efficacy Trial"

This repository contains the code used to complete the analyses discussed in "Using Publicly Available Auxiliary Data to Improve Precision of Treatment Effect Estimation in a Randomized Efficacy Trial."

The code cannot be fully run because the identities of schools included in the CTAI trial are protected. However, we have included as much information as possible here for full transparency.

#### Contents:
* `raw-data/`
  * location for raw AEIS data - see `README.md` in this folder for instructions on how to populate the folders
* `data/`
  * data products generated with scripts
  * `temp/`
    * temporary files used for data build
  * `xwalk/`
    * files used to create keys for grade types
* `output/`
  * `figures/`
    * all paper figures
  * `models/`
    * auxiliary random forest models
* `scripts/`
  * `01-make-data.R`
    * combines data from `raw-data/`.
    * outputs: `data/HS_MS.Rdata`
    * creates covariate data for the years 2003-2008 and outcome data for 2008-2009 for RCT and auxiliary schools in Texas.
  * `01a-prep-data.R`
    * processes AEIS data build for analysis
    * inputs: `data/HS_MS.Rdata`
    * outputs: `data/HS_MSdata_prep.Rdata`
    * Scales covariates, addresses missing values, and splits data into middle and high schools
  * `02-aux-cv.R`
    * inputs: `data/HS_MSdata_prep.Rdata`
    * outputs: `output/aux_cv.Rdata`
    * 10-fold cross validation fitting different models on the auxiliary schools  
    * Takes ~30 minutes running in parallel across 10 nodes with 1.5 GB of memory per node on the [Great Lakes Cluster](https://arc.umich.edu/greatlakes/).
  * `03-aux-mod.R`
    * inputs: `data/HS_MSdata_prep.Rdata`
    * outputs: `output/models/hs_rem_mod.Rdata`,  `output/models/ms_rem_mod.Rdata`
    * fits final auxiliary model on auxiliary middle and high schools
    * each model takes 15-25 minutes to run locally on a Macbook Pro with a 1.4 GHz Quad-Core Intel Core i5 processor and 16 GB of memory
  * `04-effect-est.R`
    * inputs: `data/HS_MSdata_prep.Rdata`, `data/HS_MS.Rdata`, `output/models/hs_rem_mod.Rdata`,  `output/models/ms_rem_mod.Rdata`
    * outputs: `output/cta_result_dat.Rdata`
    * calculates point and variance estimates for the effect of CTAI on 2008 math TAKS passing rates for 44 trial schools in Texas
  * `05-figures.R`
    * inputs: `data/HS_MSdata.Rdata`, `data/HS_MSdata_prep.Rdata`, `output/cta_result_dat.Rdata`, `output/models/ms_rem_mod.Rdata`, `output/models/hs_rem_mod.Rdata`, `output/aux_cv.Rdata`
    * outputs: all files in `output/figures/`
    * generates paper figures and calculations

#### Data Build Outputs:
* `HS_MS.Rdata`
  * `covsE` - covariates for experimental schools
  * `covsRem` - covariates for auxiliary schools
  * `schools` - info on trial schools including treatment condition and pairs. Also includes outcomes, and pretest scores (2007 TAKS passing rates) from AEIS data
  * `outRem` - outcomes (2008 and 2009 TAKS passing rates) for auxiliary schools
* `HS_MSdata_prep.Rdata`
  * `covsE_hs` - processed covariates for trial high schools
  * `covsE_ms` - processed covariates for trial middle schools
  * `covsRem_hs` - processed covariates for auxiliary high schools
  * `covsRem_ms` - processed covariates for auxiliary middle schools
  * `outRem_hs` - outcomes for auxiliary high schools
  * `outRem_ms`-: outcomes for auxiliary middle schools


#### Sources:

* Texas [AEIS](https://rptsvr1.tea.texas.gov/perfreport/aeis/2008/DownloadData.html) (publicly available)
  * data [reference](https://rptsvr1.tea.texas.gov/perfreport/aeis/2008/xplore/aeisref.html)
  * [masking explanation](https://rptsvr1.tea.texas.gov/perfreport/aeis/2008/masking.htm)


#### Notes on data build:
* The AEIS data for the 2003-4 and 2004-5 school years does not have any column names, although there are format files which contain labels and descriptions for the columns. Therefore, we follow the following naming convention: `[data-file-name]_[column-number]_[data-year]`. For example, the 5th column in the 2003-4 campus financial dataset `y34/cfin3.dat` is labeled `cfin3_5_34`.
* All covariates from the datasets in the `y34/` ... `y78/` folders are named with a suffix that aligns with the school year because column names can be the same between years. For example, all covariates read in from the .dat files in `y78` have a suffix `_78`.
* There is a considerable amount of missing data. Data is missing for various reasons:
  * For covariates from earlier years, a school didn't exist in the data until later years.
  * The value is masked because the number of students it represents is too small.
  * The value was not gathered
* A couple of variables are not found in the AEIS data dictionary:
  * `OUTTYPE` is created in the data build and indicates whether only high school (9th grade) "H", only middle school (8th grade) "M", or both "B" outcomes are available for a school.
* '(C)([ABHFE])(009TM)(0[89])(R)' is the regex to pull the TAKS math scores (009TM indicates 9th grade and 0[89] are the years of the scores pulled). A, B, H, F, and E indicate subgroups of students that the passing rate applies to.
