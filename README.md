# stat-praktikum-protein-intake-mv
# CONTENTS

1. Introduction
2. Overview
3. Requirements
4. Step by step execution  
  
## Introduction

The series of the sripts provided here allow for reproducibility and further
evaluation of the results, presented as part of the 5th Semester "Statistisches
Praktikum" project of Ludwigs-Maximillians-University. <br/>
The main goal hereby was the analysis of the influence of the lenght of the 
protein intake on the surviabilty and the length of stay at the ICU. 


Previously the association between protein intake and the risk of death or
discharge of ICU patients in the hospital was studied. For more information please
take a look at the following repository by Dr. Andreas Bender: [analysis-protein-intake-2021](https://github.com/adibender/analysis-protein-intake-2021.git) 


## Overview

Stat-Praktikum-protein-intake-mv <br/>
Version 0.1<br/>
Date: 25.02.2022<br/>
The code was tested for mac and windows systems.

## Requirements

Current R version 4.1.1<br/>
The list of the packages and their versions can be found under `1_packages.R`<br/>
Optional: current python version and the `manim` library (including its dependecies)

## Step by step execution

Run the following files in the folder **code_abgabe** in the given order:<br/>

### Preparation:
`1_packages.R`<br/>
`2_function_helpers.R`<br/>
`3_dir_create.R` (run only once)

### Model:
`4_model_main.R`<br/>

### Visuals:
`5_cif_hazard.R`<br/>
`6_kaplan_meier_curves.R`<br/>
`7_plots_confounder.R`<br/>
`8_lag_lead_partial.R`<br/>

### Optional:
**Statistical Description:**<br/>
`data_description.R` <br/>
**PYTHON:**<br/>
`lag_lead_anim.py`<br/>
`lag_lead_2.py`<br/>
`Transformation_lag.py`<br/>

Or simply execute the `source_rerun.R` file to achieve the same results as us.


## For more information please see below:


### Step 1 - 2  Prepation

`1_packages.R` list all of the necessary packages and installs them in case
they are not installed. <br/>

`2_function_helpers.R` creates the necessary fonts and helper function for
further use (mainly visualization helpers). 

### Step 3 Directory Setup

`3_dir_create.R` sets up the neccesary directories in the **!working path** of user.<br/>
This step has to be executed only once during the first run of the code.

### Step 4 Model

This script generates the models as a large BAM file, used for further
analysis

### Step 5 - 8 Visualization 

Generation of the visual graphs used for the interpretation of the data and 
results. Here the folder name corresponds with the script name for easier 
usage. 

**Step 5: CIF and Hazard plots**

This Step generates the CIF and Hazard plots based on previuosly fitted
model and the data available.

**Step 6: Kaplan-Meier-curve plots**

Generates the plots for the Kaplan-Meier-Curve for different admissions
reasons.

**Step 7: Confounder**

This step generates visualizations for some of the confounder variables,
used as an example.


**Step 8: Lag Lead window**

Visualization of the selected lag-lead window influence on the partial 
effects for easier understanding of the model

### Step 9 - 10 (optional)

**Step 9: Statistical Description**

First analysis to get familiar with the data and exploratory data analysis for further questions.

**Step 10: PYTHON**

This is an optional step used for the visualization of the lag-lead 
concept with help of the `manim` python library, primarly developed by Grant 
Sanderson for visualizations of math concepts. <br/>
It requires the installation of python, `manim` library and its necessary 
dependencies, which can be found here: 
https://azarzadavila-manim.readthedocs.io/en/latest/animation.html <br/>

As it is a supplementary step we do not specify any details here. However the
user can execute the poorly written python code (by us) and see the magic.

```r
> devtools::session_info()
??? Session info ??????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
 setting  value
 version  R version 4.1.1 (2021-08-10)
 os       macOS Monterey 12.1
 system   x86_64, darwin17.0
 ui       RStudio
 language (EN)
 collate  de_DE.UTF-8
 ctype    de_DE.UTF-8
 tz       Europe/Berlin
 date     2022-03-03
 rstudio  2021.09.0+351 Ghost Orchid (desktop)
 pandoc   2.14.0.3 @ /Applications/RStudio.app/Contents/MacOS/pandoc/ (via rmarkdown)

??? Packages ??????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
 package      * version    date (UTC) lib source
 abind          1.4-5      2016-07-21 [1] CRAN (R 4.1.0)
 assertthat     0.2.1      2019-03-21 [1] CRAN (R 4.1.0)
 backports      1.2.1      2020-12-09 [1] CRAN (R 4.1.0)
 brio           1.1.3      2021-11-30 [1] CRAN (R 4.1.0)
 broom          0.7.9      2021-07-27 [1] CRAN (R 4.1.0)
 cachem         1.0.6      2021-08-19 [1] CRAN (R 4.1.0)
 callr          3.7.0      2021-04-20 [1] CRAN (R 4.1.0)
 car            3.0-12     2021-11-06 [1] CRAN (R 4.1.0)
 carData        3.0-4      2020-05-22 [1] CRAN (R 4.1.0)
 cellranger     1.1.0      2016-07-27 [1] CRAN (R 4.1.0)
 checkmate      2.0.0      2020-02-06 [1] CRAN (R 4.1.0)
 cli            3.1.1      2022-01-20 [1] CRAN (R 4.1.2)
 codetools      0.2-18     2020-11-04 [1] CRAN (R 4.1.1)
 colorspace     2.0-2      2021-06-24 [1] CRAN (R 4.1.0)
 cowplot      * 1.1.1      2020-12-30 [1] CRAN (R 4.1.0)
 crayon         1.4.1      2021-02-08 [1] CRAN (R 4.1.0)
 data.table   * 1.14.2     2021-09-27 [1] CRAN (R 4.1.0)
 DataExplorer * 0.8.2      2020-12-15 [1] CRAN (R 4.1.0)
 DBI            1.1.1      2021-01-15 [1] CRAN (R 4.1.0)
 dbplyr         2.1.1      2021-04-06 [1] CRAN (R 4.1.0)
 desc           1.4.0      2021-09-28 [1] CRAN (R 4.1.0)
 devtools       2.4.3      2021-11-30 [1] CRAN (R 4.1.0)
 digest         0.6.28     2021-09-23 [1] CRAN (R 4.1.0)
 dplyr        * 1.0.8      2022-02-08 [1] CRAN (R 4.1.2)
 ellipsis       0.3.2      2021-04-29 [1] CRAN (R 4.1.0)
 evaluate       0.14       2019-05-28 [1] CRAN (R 4.1.0)
 extrafont    * 0.17       2014-12-08 [1] CRAN (R 4.1.0)
 extrafontdb    1.0        2012-06-11 [1] CRAN (R 4.1.0)
 fansi          0.5.0      2021-05-25 [1] CRAN (R 4.1.0)
 farver         2.1.0      2021-02-28 [1] CRAN (R 4.1.0)
 fastmap        1.1.0      2021-01-25 [1] CRAN (R 4.1.0)
 forcats      * 0.5.1      2021-01-27 [1] CRAN (R 4.1.0)
 foreach        1.5.1      2020-10-15 [1] CRAN (R 4.1.0)
 Formula        1.2-4      2020-10-16 [1] CRAN (R 4.1.0)
 fs             1.5.0      2020-07-31 [1] CRAN (R 4.1.0)
 future         1.23.0     2021-10-31 [1] CRAN (R 4.1.0)
 future.apply   1.8.1      2021-08-10 [1] CRAN (R 4.1.0)
 generics       0.1.1      2021-10-25 [1] CRAN (R 4.1.1)
 ggplot2      * 3.3.5      2021-06-25 [1] CRAN (R 4.1.0)
 ggpubr       * 0.4.0      2020-06-27 [1] CRAN (R 4.1.0)
 ggsignif       0.6.3      2021-09-09 [1] CRAN (R 4.1.0)
 globals        0.14.0     2020-11-22 [1] CRAN (R 4.1.0)
 glue           1.4.2      2020-08-27 [1] CRAN (R 4.1.0)
 gridExtra    * 2.3        2017-09-09 [1] CRAN (R 4.1.0)
 gtable         0.3.0      2019-03-25 [1] CRAN (R 4.1.0)
 haven          2.4.3      2021-08-04 [1] CRAN (R 4.1.0)
 hms            1.1.1      2021-09-26 [1] CRAN (R 4.1.0)
 htmltools      0.5.2      2021-08-25 [1] CRAN (R 4.1.0)
 htmlwidgets    1.5.4      2021-09-08 [1] CRAN (R 4.1.0)
 httr           1.4.2      2020-07-20 [1] CRAN (R 4.1.0)
 igraph         1.2.7      2021-10-15 [1] CRAN (R 4.1.0)
 iterators      1.0.13     2020-10-15 [1] CRAN (R 4.1.0)
 jsonlite       1.7.2      2020-12-09 [1] CRAN (R 4.1.0)
 km.ci          0.5-2      2009-08-30 [1] CRAN (R 4.1.0)
 KMsurv         0.1-5      2012-12-03 [1] CRAN (R 4.1.0)
 knitr          1.36       2021-09-29 [1] CRAN (R 4.1.0)
 labeling       0.4.2      2020-10-20 [1] CRAN (R 4.1.0)
 lattice        0.20-44    2021-05-02 [1] CRAN (R 4.1.1)
 lava           1.6.10     2021-09-02 [1] CRAN (R 4.1.0)
 lazyeval       0.2.2      2019-03-15 [1] CRAN (R 4.1.0)
 lifecycle      1.0.1      2021-09-24 [1] CRAN (R 4.1.0)
 listenv        0.8.0      2019-12-05 [1] CRAN (R 4.1.0)
 lubridate      1.8.0      2021-10-07 [1] CRAN (R 4.1.0)
 magrittr     * 2.0.2      2022-01-26 [1] CRAN (R 4.1.2)
 Matrix         1.3-4      2021-06-01 [1] CRAN (R 4.1.1)
 memoise        2.0.1      2021-11-26 [1] CRAN (R 4.1.0)
 mgcv         * 1.8-39     2022-02-24 [1] CRAN (R 4.1.2)
 modelr         0.1.8      2020-05-19 [1] CRAN (R 4.1.0)
 munsell        0.5.0      2018-06-12 [1] CRAN (R 4.1.0)
 mvtnorm        1.1-3      2021-10-08 [1] CRAN (R 4.1.0)
 networkD3      0.4        2017-03-18 [1] CRAN (R 4.1.0)
 nlme         * 3.1-152    2021-02-04 [1] CRAN (R 4.1.1)
 numDeriv       2016.8-1.1 2019-06-06 [1] CRAN (R 4.1.0)
 pammtools    * 0.5.8      2022-01-09 [1] CRAN (R 4.1.2)
 parallelly     1.28.1     2021-09-09 [1] CRAN (R 4.1.0)
 patchwork    * 1.1.1      2020-12-17 [1] CRAN (R 4.1.0)
 pec            2021.10.11 2021-10-11 [1] CRAN (R 4.1.0)
 pillar         1.6.4      2021-10-18 [1] CRAN (R 4.1.0)
 pkgbuild       1.3.1      2021-12-20 [1] CRAN (R 4.1.0)
 pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.1.0)
 pkgload        1.2.4      2021-11-30 [1] CRAN (R 4.1.0)
 prettyunits    1.1.1      2020-01-24 [1] CRAN (R 4.1.0)
 processx       3.5.2      2021-04-30 [1] CRAN (R 4.1.0)
 prodlim        2019.11.13 2019-11-17 [1] CRAN (R 4.1.0)
 ps             1.6.0      2021-02-28 [1] CRAN (R 4.1.0)
 purrr        * 0.3.4      2020-04-17 [1] CRAN (R 4.1.0)
 R6             2.5.1      2021-08-19 [1] CRAN (R 4.1.0)
 Rcpp           1.0.7      2021-07-07 [1] CRAN (R 4.1.0)
 readr        * 2.0.2      2021-09-27 [1] CRAN (R 4.1.0)
 readxl         1.3.1      2019-03-13 [1] CRAN (R 4.1.0)
 remotes        2.4.2      2021-11-30 [1] CRAN (R 4.1.0)
 reprex         2.0.1      2021-08-05 [1] CRAN (R 4.1.0)
 rlang          1.0.1      2022-02-03 [1] CRAN (R 4.1.2)
 rmarkdown      2.11       2021-09-14 [1] CRAN (R 4.1.0)
 rprojroot      2.0.2      2020-11-15 [1] CRAN (R 4.1.0)
 rstatix        0.7.0      2021-02-13 [1] CRAN (R 4.1.0)
 rstudioapi     0.13       2020-11-12 [1] CRAN (R 4.1.0)
 Rttf2pt1       1.3.10     2022-02-07 [1] CRAN (R 4.1.2)
 rvest          1.0.2      2021-10-16 [1] CRAN (R 4.1.0)
 scales         1.1.1      2020-05-11 [1] CRAN (R 4.1.0)
 sessioninfo    1.2.2      2021-12-06 [1] CRAN (R 4.1.0)
 stringi        1.7.5      2021-10-04 [1] CRAN (R 4.1.0)
 stringr      * 1.4.0      2019-02-10 [1] CRAN (R 4.1.0)
 survival     * 3.2-13     2021-08-24 [1] CRAN (R 4.1.0)
 survminer    * 0.4.9      2021-03-09 [1] CRAN (R 4.1.0)
 survMisc       0.5.5      2018-07-05 [1] CRAN (R 4.1.0)
 svglite      * 2.1.0      2022-02-03 [1] CRAN (R 4.1.2)
 systemfonts    1.0.2      2021-05-11 [1] CRAN (R 4.1.0)
 testthat       3.1.2      2022-01-20 [1] CRAN (R 4.1.2)
 tibble       * 3.1.5      2021-09-30 [1] CRAN (R 4.1.0)
 tidyr        * 1.2.0      2022-02-01 [1] CRAN (R 4.1.2)
 tidyselect     1.1.1      2021-04-30 [1] CRAN (R 4.1.0)
 tidyverse    * 1.3.1      2021-04-15 [1] CRAN (R 4.1.0)
 timereg        2.0.1      2021-10-13 [1] CRAN (R 4.1.0)
 tzdb           0.1.2      2021-07-20 [1] CRAN (R 4.1.0)
 usethis        2.1.5      2021-12-09 [1] CRAN (R 4.1.0)
 utf8           1.2.2      2021-07-24 [1] CRAN (R 4.1.0)
 vctrs          0.3.8      2021-04-29 [1] CRAN (R 4.1.0)
 viridisLite    0.4.0      2021-04-13 [1] CRAN (R 4.1.0)
 withr          2.4.3      2021-11-30 [1] CRAN (R 4.1.0)
 xfun           0.27       2021-10-18 [1] CRAN (R 4.1.0)
 xml2           1.3.2      2020-04-23 [1] CRAN (R 4.1.0)
 xtable         1.8-4      2019-04-21 [1] CRAN (R 4.1.0)
 zoo            1.8-9      2021-03-09 [1] CRAN (R 4.1.0)

```



