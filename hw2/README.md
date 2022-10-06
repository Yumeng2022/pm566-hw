566-hw2
================
Yumeng Gao
2022-10-06

### Prepare the library

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ stringr 1.4.1
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.2
    ## ✔ readr   2.1.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library(R.utils)
```

    ## Loading required package: R.oo
    ## Loading required package: R.methodsS3
    ## R.methodsS3 v1.8.2 (2022-06-13 22:00:14 UTC) successfully loaded. See ?R.methodsS3 for help.
    ## R.oo v1.25.0 (2022-06-12 02:20:02 UTC) successfully loaded. See ?R.oo for help.
    ## 
    ## Attaching package: 'R.oo'
    ## 
    ## The following object is masked from 'package:R.methodsS3':
    ## 
    ##     throw
    ## 
    ## The following objects are masked from 'package:methods':
    ## 
    ##     getClasses, getMethods
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     attach, detach, load, save
    ## 
    ## R.utils v2.12.0 (2022-06-28 03:20:05 UTC) successfully loaded. See ?R.utils for help.
    ## 
    ## Attaching package: 'R.utils'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     cat, commandArgs, getOption, isOpen, nullfile, parse, warnings

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

# Data Wrangling

### Download and read in csv.

``` r
if (!file.exists("individual.csv")) {
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv", "individual.csv", method = "libcurl", timeout  = 60)
}
ind= data.table::fread("individual.csv")

if (!file.exists("regional.csv")) {
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv", "regional.csv", method = "libcurl", timeout  = 60)
}
reg= data.table::fread("regional.csv")
```

### Merge the individual and regional CHS datasets using the location variable.

``` r
chs= merge(ind, reg, by= "townname", all=T)
```

## 1.After merging the data, make sure you don’t have any duplicates by counting the number of rows. Make sure it matches.

``` r
str(chs)
```

    ## Classes 'data.table' and 'data.frame':   1200 obs. of  49 variables:
    ##  $ townname     : chr  "Alpine" "Alpine" "Alpine" "Alpine" ...
    ##  $ sid          : int  835 838 839 840 841 842 843 844 847 849 ...
    ##  $ male         : int  0 0 0 0 1 1 1 1 1 1 ...
    ##  $ race         : chr  "W" "O" "M" "W" ...
    ##  $ hispanic     : int  0 1 1 0 1 1 0 1 0 0 ...
    ##  $ agepft       : num  10.1 9.49 10.05 9.97 10.55 ...
    ##  $ height       : int  143 133 142 146 150 139 149 143 137 147 ...
    ##  $ weight       : int  69 62 86 78 78 65 98 65 69 112 ...
    ##  $ bmi          : num  15.3 15.9 19.4 16.6 15.8 ...
    ##  $ asthma       : int  0 0 0 0 0 0 0 NA 0 0 ...
    ##  $ active_asthma: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ father_asthma: int  0 0 0 0 0 0 0 NA 0 1 ...
    ##  $ mother_asthma: int  0 0 1 0 0 0 0 NA 0 0 ...
    ##  $ wheeze       : int  0 0 1 0 0 1 1 NA 0 1 ...
    ##  $ hayfever     : int  0 0 1 0 0 0 0 NA 0 0 ...
    ##  $ allergy      : int  1 0 1 0 0 0 1 NA 0 1 ...
    ##  $ educ_parent  : int  3 4 3 NA 5 1 3 NA 5 3 ...
    ##  $ smoke        : int  0 NA 1 NA 0 1 0 NA 0 0 ...
    ##  $ pets         : int  1 1 1 0 1 1 1 0 1 1 ...
    ##  $ gasstove     : int  0 0 0 NA 0 0 1 NA 1 0 ...
    ##  $ fev          : num  2529 1738 2122 2467 2252 ...
    ##  $ fvc          : num  2826 1964 2327 2638 2595 ...
    ##  $ mmef         : num  3407 2133 2835 3466 2445 ...
    ##  $ pm25_mass    : num  8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 ...
    ##  $ pm25_so4     : num  1.73 1.73 1.73 1.73 1.73 1.73 1.73 1.73 1.73 1.73 ...
    ##  $ pm25_no3     : num  1.59 1.59 1.59 1.59 1.59 1.59 1.59 1.59 1.59 1.59 ...
    ##  $ pm25_nh4     : num  0.88 0.88 0.88 0.88 0.88 0.88 0.88 0.88 0.88 0.88 ...
    ##  $ pm25_oc      : num  2.54 2.54 2.54 2.54 2.54 2.54 2.54 2.54 2.54 2.54 ...
    ##  $ pm25_ec      : num  0.48 0.48 0.48 0.48 0.48 0.48 0.48 0.48 0.48 0.48 ...
    ##  $ pm25_om      : num  3.04 3.04 3.04 3.04 3.04 3.04 3.04 3.04 3.04 3.04 ...
    ##  $ pm10_oc      : num  3.25 3.25 3.25 3.25 3.25 3.25 3.25 3.25 3.25 3.25 ...
    ##  $ pm10_ec      : num  0.49 0.49 0.49 0.49 0.49 0.49 0.49 0.49 0.49 0.49 ...
    ##  $ pm10_tc      : num  3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.75 ...
    ##  $ formic       : num  1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 ...
    ##  $ acetic       : num  2.49 2.49 2.49 2.49 2.49 2.49 2.49 2.49 2.49 2.49 ...
    ##  $ hcl          : num  0.41 0.41 0.41 0.41 0.41 0.41 0.41 0.41 0.41 0.41 ...
    ##  $ hno3         : num  1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 ...
    ##  $ o3_max       : num  65.8 65.8 65.8 65.8 65.8 ...
    ##  $ o3106        : num  55 55 55 55 55 ...
    ##  $ o3_24        : num  41.2 41.2 41.2 41.2 41.2 ...
    ##  $ no2          : num  12.2 12.2 12.2 12.2 12.2 ...
    ##  $ pm10         : num  24.7 24.7 24.7 24.7 24.7 ...
    ##  $ no_24hr      : num  2.48 2.48 2.48 2.48 2.48 2.48 2.48 2.48 2.48 2.48 ...
    ##  $ pm2_5_fr     : num  10.3 10.3 10.3 10.3 10.3 ...
    ##  $ iacid        : num  2.39 2.39 2.39 2.39 2.39 2.39 2.39 2.39 2.39 2.39 ...
    ##  $ oacid        : num  3.52 3.52 3.52 3.52 3.52 3.52 3.52 3.52 3.52 3.52 ...
    ##  $ total_acids  : num  5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5 ...
    ##  $ lon          : num  -117 -117 -117 -117 -117 ...
    ##  $ lat          : num  32.8 32.8 32.8 32.8 32.8 ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "sorted")= chr "townname"

``` r
nrow(chs)
```

    ## [1] 1200

``` r
nrow(ind)
```

    ## [1] 1200

``` r
nrow(chs)== nrow(ind)
```

    ## [1] TRUE

Since the combined dataset and individual dataset had the same number of
rows, there was no duplicate.

Check for weird or missing values of key variables.

``` r
#numeric
summary(chs$bmi)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   11.30   15.78   17.48   18.50   20.35   41.27      89

``` r
summary(chs$fev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   984.8  1809.0  2022.7  2031.3  2249.7  3323.7      95

``` r
summary(chs$pm25_mass)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   5.960   7.615  10.545  14.362  20.988  29.970

``` r
#categorical
sum(is.na(chs$smoke))
```

    ## [1] 40

``` r
sum(is.na(chs$gasstove))
```

    ## [1] 33

``` r
sum(is.na(chs$asthma))
```

    ## [1] 31

``` r
sum(is.na(chs$townname))
```

    ## [1] 0

``` r
sum(is.na(chs$male))
```

    ## [1] 0

For numeric variables, there were no weird values. BMI(89) and FEV(95)
had missing values. For categorical variables, smoke(40), gasstove(33),
and asthma(31) had missing values, but as they were all binary, we
cannot assign any “average” to them.

Proportions of NA:

``` r
mean(is.na(chs$bmi))
```

    ## [1] 0.07416667

``` r
mean(is.na(chs$fev))
```

    ## [1] 0.07916667

``` r
mean(is.na(chs$smoke))
```

    ## [1] 0.03333333

``` r
mean(is.na(chs$gasstove))
```

    ## [1] 0.0275

``` r
mean(is.na(chs$asthma))
```

    ## [1] 0.02583333

Since all the missing values of categorical variables took only about 3%
of the whole, we can drop them for further analysis needs:

``` r
chs= chs[!is.na(smoke) & !is.na(gasstove) & !is.na(asthma)]
```

Noted that there were 78 observations dropped from the dataset to
eliminate missing values of categorical variables.

### So for BMI and FEV’s missing data, impute data using the average within the variables “male” and “hispanic”.

``` r
sum(is.na(chs$bmi))
```

    ## [1] 79

``` r
sum(is.na(chs$fev))
```

    ## [1] 85

``` r
bmi_m= mean(chs[male=="1" & hispanic=="1", bmi], na.rm=T)
fev_m= mean(chs[male=="1" & hispanic=="1", fev], na.rm=T)

chs$bmi[is.na(chs$bmi)]= bmi_m
chs$fev[is.na(chs$fev)]= fev_m
```

After data cleaning, 79 missing values of BMI and 85 of FEV were also
replaced with the corresponding average values.

## 2.Create a new categorical variable named “obesity_level” using the BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight BMI 22-24; obese BMI\>24).

``` r
chs[, obesity_level := fifelse(
  bmi < 14, "underweight", 
  fifelse(bmi < 22, "normal", 
  fifelse(bmi < 24, "overweight","obese")))
  ]
```

### To make sure the variable is rightly coded, create a summary table that contains the minimum BMI, maximum BMI, and the total number of observations per category.

``` r
summary(chs$bmi)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.30   15.92   17.71   18.52   19.94   41.27

``` r
table(chs$obesity_level)
```

    ## 
    ##      normal       obese  overweight underweight 
    ##         912          94          82          34

Summary table:

``` r
obe= chs[, .(
  BMI_Min= min(bmi),
  `# of Underweight(<14)`= 34,
  `# of Normal(14-22)`= 912,
  `# of Overweight(22-24)`= 82,
  `# of Obese(>24)`= 94,
  BMI_Max= max(bmi)
  )]
knitr::kable(obe)
```

| BMI_Min | \# of Underweight(\<14) | \# of Normal(14-22) | \# of Overweight(22-24) | \# of Obese(\>24) |  BMI_Max |
|--------:|------------------------:|--------------------:|------------------------:|------------------:|---------:|
| 11.2964 |                      34 |                 912 |                      82 |                94 | 41.26613 |

From the table, we can find that most children in this study had normal
BMI level, yet the number of overweight and obese is also alarming.

## 3.Create another categorical variable named “smoke_gas_exposure” that summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have four categories in total.

``` r
chs[, smoke_gas_exposure := fifelse(smoke == 1 & gasstove==1, "both",
                fifelse(smoke==1 & gasstove== 0, "smoke_only",
                fifelse(smoke==0 & gasstove ==1, "gas_only","neither")))
    ]
table(chs$smoke_gas_exposure)
```

    ## 
    ##       both   gas_only    neither smoke_only 
    ##        146        731        210         35

## 4.Create four summary tables showing the average (or proportion, if binary) and sd of “Forced expiratory volume in 1 second (ml)” and asthma indicator by town, sex, obesity level, and “smoke_gas_exposure.”

### Townname table:

``` r
to= chs[, .(
  N= .N,    
`Average FEV1`= mean(fev),  
`SD FEV1`= sd(fev),     
`%Asthma`= sum(asthma==1)/sum(asthma==1 | asthma==0)
  ), by= townname]

knitr::kable(to)
```

| townname      |   N | Average FEV1 |  SD FEV1 |   %Asthma |
|:--------------|----:|-------------:|---------:|----------:|
| Alpine        |  91 |     2083.947 | 283.5611 | 0.1208791 |
| Atascadero    |  94 |     2074.841 | 325.1329 | 0.2553191 |
| Lake Elsinore |  90 |     2041.311 | 308.7082 | 0.1333333 |
| Lake Gregory  |  97 |     2100.609 | 318.3725 | 0.1546392 |
| Lancaster     |  92 |     2030.623 | 308.8579 | 0.1521739 |
| Lompoc        |  95 |     2047.279 | 352.1169 | 0.1157895 |
| Long Beach    |  91 |     1996.517 | 321.7598 | 0.1318681 |
| Mira Loma     |  92 |     2006.531 | 328.7296 | 0.1521739 |
| Riverside     |  94 |     1998.685 | 280.4443 | 0.1063830 |
| San Dimas     |  97 |     2029.803 | 318.3693 | 0.1649485 |
| Santa Maria   |  91 |     2030.422 | 322.1983 | 0.1428571 |
| Upland        |  98 |     2034.059 | 343.5017 | 0.1224490 |

### Sex(male) table:

``` r
se= chs[, .(
  N= .N,    
`Average FEV1`= mean(fev),  
`SD FEV1`= sd(fev),     
`%Asthma`= sum(asthma==1)/sum(asthma==1 | asthma==0)
  ), by= male]

knitr::kable(se)
```

| male |   N | Average FEV1 |  SD FEV1 |   %Asthma |
|-----:|----:|-------------:|---------:|----------:|
|    0 | 572 |     1972.404 | 312.8069 | 0.1188811 |
|    1 | 550 |     2109.822 | 309.3886 | 0.1745455 |

### Obesity_level table:

``` r
ob= chs[, .(
  N= .N,    
`Average FEV1`= mean(fev),  
`SD FEV1`= sd(fev),     
`%Asthma`= sum(asthma==1)/sum(asthma==1 | asthma==0)
  ), by= obesity_level]

knitr::kable(ob)
```

| obesity_level |   N | Average FEV1 |  SD FEV1 |   %Asthma |
|:--------------|----:|-------------:|---------:|----------:|
| normal        | 912 |     2010.148 | 297.1568 | 0.1381579 |
| overweight    |  82 |     2235.991 | 311.5836 | 0.1707317 |
| obese         |  94 |     2271.598 | 332.3952 | 0.2234043 |
| underweight   |  34 |     1720.027 | 283.3457 | 0.0882353 |

### Smoke_gas_exposure table:

``` r
sge= chs[, .(
  N= .N,    
`Average FEV1`= mean(fev),  
`SD FEV1`= sd(fev),     
`%Asthma`= sum(asthma==1)/sum(asthma==1 | asthma==0)
  ), by= smoke_gas_exposure]

knitr::kable(sge)
```

| smoke_gas_exposure |   N | Average FEV1 |  SD FEV1 |   %Asthma |
|:-------------------|----:|-------------:|---------:|----------:|
| neither            | 210 |     2065.263 | 331.1657 | 0.1476190 |
| smoke_only         |  35 |     2076.610 | 297.9902 | 0.1714286 |
| gas_only           | 731 |     2033.615 | 318.9099 | 0.1477428 |
| both               | 146 |     2025.058 | 302.7463 | 0.1301370 |

# Looking at the Data (EDA)

> The primary questions of interest are: 1. What is the association
> between BMI and FEV (forced expiratory volume)? 2. What is the
> association between smoke and gas exposure and FEV? 3. What is the
> association between PM2.5 exposure and FEV?

## 1.Facet plot showing scatterplots with regression lines of BMI vs FEV by “townname”.

## 2.Stacked histograms of FEV by BMI category and FEV by smoke/gas exposure. Use different color schemes than the ggplot default.

## 3.Barchart of BMI by smoke/gas exposure.

## 4.Statistical summary graphs of FEV by BMI and FEV by smoke/gas exposure category.

## 5.A leaflet map showing the concentrations of PM2.5 mass in each of the CHS communities.

## 6.Choose a visualization to examine whether PM2.5 mass is associated with FEV.
