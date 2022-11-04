566-hw3
================
Yumeng Gao
2022-11-04

> Prepare the library

``` r
library(R.utils)
```

    ## Loading required package: R.oo

    ## Loading required package: R.methodsS3

    ## R.methodsS3 v1.8.2 (2022-06-13 22:00:14 UTC) successfully loaded. See ?R.methodsS3 for help.

    ## R.oo v1.25.0 (2022-06-12 02:20:02 UTC) successfully loaded. See ?R.oo for help.

    ## 
    ## Attaching package: 'R.oo'

    ## The following object is masked from 'package:R.methodsS3':
    ## 
    ##     throw

    ## The following objects are masked from 'package:methods':
    ## 
    ##     getClasses, getMethods

    ## The following objects are masked from 'package:base':
    ## 
    ##     attach, detach, load, save

    ## R.utils v2.12.0 (2022-06-28 03:20:05 UTC) successfully loaded. See ?R.utils for help.

    ## 
    ## Attaching package: 'R.utils'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

    ## The following objects are masked from 'package:base':
    ## 
    ##     cat, commandArgs, getOption, isOpen, nullfile, parse, warnings

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ tidyr::extract()         masks R.utils::extract()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
library(tidytext)
library(dplyr)
library(dtplyr)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

``` r
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(xml2)
library(httr)
```

# APIs

## 1. Using the NCBI API, look for papers that show up under the term “sars-cov-2 trial vaccine.” Look for the data in the pubmed database, and then retrieve the details of the paper as shown in lab 7. How many papers were you able to find?

``` r
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine")

counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]")

counts <- as.character(counts)

stringr::str_extract(counts, "[0-9,]+")
```

    ## [1] "4,009"

There were totally 4009 papers on the website when searching for
“sars-cov-2 trial vaccine”.

## 2. Using the list of pubmed ids you retrieved, download each papers’ details using the query parameter rettype = abstract. If you get more than 250 ids, just keep the first 250.

``` r
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db= "pubmed",
    term= "sars-cov-2 trial vaccine",
    retmax= 250
  ), 
)

# Extracting the content of the response of GET
ids <- httr::content(query_ids)
```

``` r
# Turn the result into a character vector
ids <- as.character(ids)

# Find all the ids 
ids <- stringr::str_extract_all(ids, "<Id>[[:digit:]]+</Id>")[[1]]

# Remove all the leading and trailing <Id> </Id>. Make use of "|"
ids <- stringr::str_remove_all(ids, "</?Id>")

head(ids)
```

    ## [1] "36328399" "36327352" "36322837" "36320825" "36314847" "36307830"

Grab first 250 publications with pubmed id

``` r
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
  query = list(
    db= "pubmed",
    id= paste(ids,collapse = ","),
    retmax= 250,
    rettype= "abstract"
    )
)

# Turning the output into character vector
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```

## 3. As we did in lab 7. Create a dataset containing the following:

-   Pubmed ID number,
-   Title of the paper,
-   Name of the journal where it was published,
-   Publication date, and
-   Abstract of the paper (if any).

``` r
pub_char_list <- xml2::xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)
```

### Get the titles:

``` r
titles <- str_extract(pub_char_list, "<ArticleTitle>[[:print:][:space:]]+</ArticleTitle>")
titles[[1]]
```

    ## [1] "<ArticleTitle>Immunogenicity and safety of a three-dose SARS-CoV-2 vaccination strategy in patients with immune-mediated inflammatory diseases on immunosuppressive therapy.</ArticleTitle>"

``` r
titles <- str_remove_all(titles, "</?[[:alnum:]- =\"]+>")
titles[[1]]
```

    ## [1] "Immunogenicity and safety of a three-dose SARS-CoV-2 vaccination strategy in patients with immune-mediated inflammatory diseases on immunosuppressive therapy."

### Get the journals:

``` r
journals <- str_extract(pub_char_list, "<Title>[[:print:][:space:]]+</Title>")
journals[[1]]
```

    ## [1] "<Title>RMD open</Title>"

``` r
journals <- str_remove_all(journals, "</?[[:alnum:]- =\"]+>")
journals[[1]]
```

    ## [1] "RMD open"

### Get the publication dates:

``` r
dates <- str_extract(pub_char_list, "<PubDate>[[:print:][:space:]]+</PubDate>")
dates[[1]]
```

    ## [1] "<PubDate>\n            <Year>2022</Year>\n            <Month>Nov</Month>\n          </PubDate>"

``` r
dates <- str_remove_all(dates, "</?[[:alnum:]- =\"]+>")
dates[[1]]
```

    ## [1] "\n            2022\n            Nov\n          "

``` r
dates <- str_replace_all(dates, "[[:space:]]+"," ")
dates[[1]]
```

    ## [1] " 2022 Nov "

### Get the abstracts:

``` r
abstracts <- str_extract(pub_char_list, "<Abstract>[[:print:][:space:]]+</Abstract>")
abstracts[[1]]
```

    ## [1] "<Abstract>\n        <AbstractText Label=\"OBJECTIVES\" NlmCategory=\"OBJECTIVE\">Humoral vaccine responses to SARS-CoV-2 vaccines are impaired and short lasting in patients with immune-mediated inflammatory diseases (IMID) following two vaccine doses. To protect these vulnerable patients against severe COVID-19 disease, a three-dose primary vaccination strategy has been implemented in many countries. The aim of this study was to evaluate humoral response and safety of primary vaccination with three doses in patients with IMID.</AbstractText>\n        <AbstractText Label=\"METHODS\" NlmCategory=\"METHODS\">Patients with IMID on immunosuppressive therapy and healthy controls receiving three-dose and two-dose primary SARS-CoV-2 vaccination, respectively, were included in this prospective observational cohort study. Anti-Spike antibodies were assessed 2-4 weeks, and 12 weeks following each dose. The main outcome was anti-Spike antibody levels 2-4 weeks following three doses in patients with IMID and two doses in controls. Additional outcomes were the antibody decline rate and adverse events.</AbstractText>\n        <AbstractText Label=\"RESULTS\" NlmCategory=\"RESULTS\">1100 patients and 303 controls were included. Following three-dose vaccination, patients achieved median (IQR) antibody levels of 5720 BAU/mL (2138-8732) compared with 4495 (1591-6639) in controls receiving two doses, p=0.27. Anti-Spike antibody levels increased with median 1932 BAU/mL (IQR 150-4978) after the third dose. The interval between the vaccine doses and vaccination with mRNA-1273 or a combination of vaccines were associated with antibody levels following the third dose. Antibody levels had a slower decline-rate following the third than the second vaccine dose, p&lt;0.001. Adverse events were reported by 464 (47%) patients and by 196 (78%) controls. Disease flares were reported by 70 (7%) patients.</AbstractText>\n        <AbstractText Label=\"CONCLUSIONS\" NlmCategory=\"CONCLUSIONS\">This study shows that additional vaccine doses to patients with IMID contribute to strong and sustained immune-responses comparable to healthy persons vaccinated twice, and supports repeated vaccination of patients with IMID.</AbstractText>\n        <AbstractText Label=\"TRIAL REGISTRATION NUMBER\" NlmCategory=\"BACKGROUND\">NCT04798625.</AbstractText>\n        <CopyrightInformation>© Author(s) (or their employer(s)) 2022. Re-use permitted under CC BY-NC. No commercial re-use. See rights and permissions. Published by BMJ.</CopyrightInformation>\n      </Abstract>"

``` r
abstracts <- str_remove_all(abstracts, "</?[[:alnum:]- =\"]+>") 
abstracts[[1]]
```

    ## [1] "\n        Humoral vaccine responses to SARS-CoV-2 vaccines are impaired and short lasting in patients with immune-mediated inflammatory diseases (IMID) following two vaccine doses. To protect these vulnerable patients against severe COVID-19 disease, a three-dose primary vaccination strategy has been implemented in many countries. The aim of this study was to evaluate humoral response and safety of primary vaccination with three doses in patients with IMID.\n        Patients with IMID on immunosuppressive therapy and healthy controls receiving three-dose and two-dose primary SARS-CoV-2 vaccination, respectively, were included in this prospective observational cohort study. Anti-Spike antibodies were assessed 2-4 weeks, and 12 weeks following each dose. The main outcome was anti-Spike antibody levels 2-4 weeks following three doses in patients with IMID and two doses in controls. Additional outcomes were the antibody decline rate and adverse events.\n        1100 patients and 303 controls were included. Following three-dose vaccination, patients achieved median (IQR) antibody levels of 5720 BAU/mL (2138-8732) compared with 4495 (1591-6639) in controls receiving two doses, p=0.27. Anti-Spike antibody levels increased with median 1932 BAU/mL (IQR 150-4978) after the third dose. The interval between the vaccine doses and vaccination with mRNA-1273 or a combination of vaccines were associated with antibody levels following the third dose. Antibody levels had a slower decline-rate following the third than the second vaccine dose, p&lt;0.001. Adverse events were reported by 464 (47%) patients and by 196 (78%) controls. Disease flares were reported by 70 (7%) patients.\n        This study shows that additional vaccine doses to patients with IMID contribute to strong and sustained immune-responses comparable to healthy persons vaccinated twice, and supports repeated vaccination of patients with IMID.\n        NCT04798625.\n        © Author(s) (or their employer(s)) 2022. Re-use permitted under CC BY-NC. No commercial re-use. See rights and permissions. Published by BMJ.\n      "

``` r
abstracts <- str_replace_all(abstracts, "[[:space:]]+"," ")
abstracts[[1]]
```

    ## [1] " Humoral vaccine responses to SARS-CoV-2 vaccines are impaired and short lasting in patients with immune-mediated inflammatory diseases (IMID) following two vaccine doses. To protect these vulnerable patients against severe COVID-19 disease, a three-dose primary vaccination strategy has been implemented in many countries. The aim of this study was to evaluate humoral response and safety of primary vaccination with three doses in patients with IMID. Patients with IMID on immunosuppressive therapy and healthy controls receiving three-dose and two-dose primary SARS-CoV-2 vaccination, respectively, were included in this prospective observational cohort study. Anti-Spike antibodies were assessed 2-4 weeks, and 12 weeks following each dose. The main outcome was anti-Spike antibody levels 2-4 weeks following three doses in patients with IMID and two doses in controls. Additional outcomes were the antibody decline rate and adverse events. 1100 patients and 303 controls were included. Following three-dose vaccination, patients achieved median (IQR) antibody levels of 5720 BAU/mL (2138-8732) compared with 4495 (1591-6639) in controls receiving two doses, p=0.27. Anti-Spike antibody levels increased with median 1932 BAU/mL (IQR 150-4978) after the third dose. The interval between the vaccine doses and vaccination with mRNA-1273 or a combination of vaccines were associated with antibody levels following the third dose. Antibody levels had a slower decline-rate following the third than the second vaccine dose, p&lt;0.001. Adverse events were reported by 464 (47%) patients and by 196 (78%) controls. Disease flares were reported by 70 (7%) patients. This study shows that additional vaccine doses to patients with IMID contribute to strong and sustained immune-responses comparable to healthy persons vaccinated twice, and supports repeated vaccination of patients with IMID. NCT04798625. © Author(s) (or their employer(s)) 2022. Re-use permitted under CC BY-NC. No commercial re-use. See rights and permissions. Published by BMJ. "

Final dataset:

``` r
database <- data.frame(
  PubMedId = ids,
  Title    = titles,
  Journal  = journals,
  PublicationDate= dates,
  Abstract = abstracts
)
knitr::kable(database[1:5,], caption = "Some papers about Sars-Cov-2 Trial Vaccine")
```

| PubMedId | Title                                                                                                                                                                                                                 | Journal                             | PublicationDate | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|:---------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------------------------------|:----------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 36328399 | Immunogenicity and safety of a three-dose SARS-CoV-2 vaccination strategy in patients with immune-mediated inflammatory diseases on immunosuppressive therapy.                                                        | RMD open                            | 2022 Nov        | Humoral vaccine responses to SARS-CoV-2 vaccines are impaired and short lasting in patients with immune-mediated inflammatory diseases (IMID) following two vaccine doses. To protect these vulnerable patients against severe COVID-19 disease, a three-dose primary vaccination strategy has been implemented in many countries. The aim of this study was to evaluate humoral response and safety of primary vaccination with three doses in patients with IMID. Patients with IMID on immunosuppressive therapy and healthy controls receiving three-dose and two-dose primary SARS-CoV-2 vaccination, respectively, were included in this prospective observational cohort study. Anti-Spike antibodies were assessed 2-4 weeks, and 12 weeks following each dose. The main outcome was anti-Spike antibody levels 2-4 weeks following three doses in patients with IMID and two doses in controls. Additional outcomes were the antibody decline rate and adverse events. 1100 patients and 303 controls were included. Following three-dose vaccination, patients achieved median (IQR) antibody levels of 5720 BAU/mL (2138-8732) compared with 4495 (1591-6639) in controls receiving two doses, p=0.27. Anti-Spike antibody levels increased with median 1932 BAU/mL (IQR 150-4978) after the third dose. The interval between the vaccine doses and vaccination with mRNA-1273 or a combination of vaccines were associated with antibody levels following the third dose. Antibody levels had a slower decline-rate following the third than the second vaccine dose, p\<0.001. Adverse events were reported by 464 (47%) patients and by 196 (78%) controls. Disease flares were reported by 70 (7%) patients. This study shows that additional vaccine doses to patients with IMID contribute to strong and sustained immune-responses comparable to healthy persons vaccinated twice, and supports repeated vaccination of patients with IMID. NCT04798625. © Author(s) (or their employer(s)) 2022. Re-use permitted under CC BY-NC. No commercial re-use. See rights and permissions. Published by BMJ.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| 36327352 | S-217622, a SARS-CoV-2 main protease inhibitor, decreases viral load and ameliorates COVID-19 severity in hamsters.                                                                                                   | Science translational medicine      | 2022 Nov 03     | In parallel with vaccination, oral antiviral agents are highly anticipated to act as countermeasures for the treatment of the coronavirus disease 2019 (COVID-19) pandemic caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). Oral antiviral medication demands not only high antiviral activity, but also target specificity, favorable oral bioavailability, and high metabolic stability. Although a large number of compounds have been identified as potential inhibitors of SARS-CoV-2 infection in vitro, few have proven to be effective in vivo. Here, we show that oral administration of S-217622 (ensitrelvir), an inhibitor of SARS-CoV-2 main protease (Mpro, also known as 3C-like protease), decreases viral load and ameliorates disease severity in SARS-CoV-2-infected hamsters. S-217622 inhibited viral proliferation at low nanomolar to sub-micromolar concentrations in cells. Oral administration of S-217622 demonstrated favorable pharmacokinetic properties and accelerated recovery from acute SARS-CoV-2 infection in hamster recipients. Moreover, S-217622 exerted antiviral activity against SARS-CoV-2 variants of concern (VOCs), including the highly pathogenic Delta variant and the recently emerged Omicron BA.5 and BA.2.75 variants. Overall, our study provides evidence that S-217622, an antiviral agent that is under evaluation in a phase 3 clinical trial (clinical trial registration no. jRCT2031210350), possesses remarkable antiviral potency and efficacy against SARS-CoV-2 and is a prospective oral therapeutic option for COVID-19.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| 36322837 | Covid-19 Vaccine Protection among Children and Adolescents in Qatar.                                                                                                                                                  | The New England journal of medicine | 2022 Nov 02     | The BNT162b2 vaccine against coronavirus disease 2019 (Covid-19) has been authorized for use in children 5 to 11 years of age and adolescents 12 to 17 years of age but in different antigen doses. We assessed the real-world effectiveness of the BNT162b2 vaccine against infection with severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) among children and adolescents in Qatar. To compare the incidence of SARS-CoV-2 infection in the national cohort of vaccinated participants with the incidence in the national cohort of unvaccinated participants, we conducted three matched, retrospective, target-trial, cohort studies - one assessing data obtained from children 5 to 11 years of age after the B.1.1.529 (omicron) variant became prevalent and two assessing data from adolescents 12 to 17 years of age before the emergence of the omicron variant (pre-omicron study) and after the omicron variant became prevalent. Associations were estimated with the use of Cox proportional-hazards regression models. Among children, the overall effectiveness of the 10-μg primary vaccine series against infection with the omicron variant was 25.7% (95% confidence interval \[CI\], 10.0 to 38.6). Effectiveness was highest (49.6%; 95% CI, 28.5 to 64.5) right after receipt of the second dose but waned rapidly thereafter and was negligible after 3 months. Effectiveness was 46.3% (95% CI, 21.5 to 63.3) among children 5 to 7 years of age and 16.6% (95% CI, -4.2 to 33.2) among those 8 to 11 years of age. Among adolescents, the overall effectiveness of the 30-μg primary vaccine series against infection with the omicron variant was 30.6% (95% CI, 26.9 to 34.1), but many adolescents had been vaccinated months earlier. Effectiveness waned over time since receipt of the second dose. Effectiveness was 35.6% (95% CI, 31.2 to 39.6) among adolescents 12 to 14 years of age and 20.9% (95% CI, 13.8 to 27.4) among those 15 to 17 years of age. In the pre-omicron study, the overall effectiveness of the 30-μg primary vaccine series against SARS-CoV-2 infection among adolescents was 87.6% (95% CI, 84.0 to 90.4) and waned relatively slowly after receipt of the second dose. Vaccination in children was associated with modest, rapidly waning protection against omicron infection. Vaccination in adolescents was associated with stronger, more durable protection, perhaps because of the larger antigen dose. (Funded by Weill Cornell Medicine-Qatar and others.). Copyright © 2022 Massachusetts Medical Society.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| 36320825 | Withholding methotrexate after vaccination with ChAdOx1 nCov19 in patients with rheumatoid or psoriatic arthritis in India (MIVAC I and II): results of two, parallel, assessor-masked, randomised controlled trials. | The Lancet. Rheumatology            | 2022 Nov        | There is a necessity for an optimal COVID-19 vaccination strategy for vulnerable population groups, including people with autoimmune inflammatory arthritis on immunosuppressants such as methotrexate, which inhibit vaccine-induced immunity against SARS-CoV-2. Thus, we aimed to assess the effects of withholding methotrexate for 2 weeks after each dose of ChAdOx1 nCov-19 (Oxford-AstraZeneca) vaccine (MIVAC I) or only after the second dose of vaccine (MIVAC II) compared with continuation of methotrexate, in terms of post-vaccination antibody titres and disease flare rates. MIVAC I and II were two parallel, independent, assessor-masked, randomised trials. The trials were done at a single centre (Dr Shenoy’s Centre for Arthritis and Rheumatism Excellence; Kochi, India) in people with either rheumatoid arthritis or psoriatic arthritis with stable disease activity, who had been on a fixed dose of methotrexate for the preceding 6 weeks. Those with previous COVID-19 or who were positive for anti-SARS-CoV-2 nucleocapsid antibodies were excluded from the trials. People on high-dose corticosteroids and rituximab were also excluded, whereas other disease-modifying antirheumatic drugs were allowed. In MIVAC I, participants were randomly assigned (1:1) to stop methotrexate treatment for 2 weeks after each vaccine dose or to continue methotrexate treatment. In MIVAC II, participants who had continued methotrexate during the first dose of vaccine were randomly assigned (1:1) to withhold methotrexate for 2 weeks after the second dose of vaccine or to continue to take methotrexate. The treating physician was masked to the group assignments. The primary outcome for both MIVAC I and MIVAC II was the titre (absolute value) of anti-receptor binding domain (RBD) antibody measured 4 weeks after the second dose of vaccine. All analyses were done per protocol. The trials were registered with the Clinical Trials Registry- India, number CTRI/2021/07/034639 (MIVAC I) and CTRI/2021/07/035307 (MIVAC II). Between July 6 and Dec 15, 2021, participants were recruited to the trials. In MIVAC I, 250 participants were randomly assigned and 158 completed the study as per the protocol (80 in the methotrexate hold group and 78 in the control group; 148 \[94%\] were women and 10 \[6%\] were men). The median post-vaccination antibody titres in the methotrexate hold group were significantly higher compared with the control group (2484·0 IU/mL, IQR 1050·0-4388·8 vs 1147·5 IU/mL, 433·5-2360·3; p=0·0014). In MIVAC II, 178 participants were randomly assigned and 157 completed the study per protocol (76 in the methotrexate hold group and 81 in the control group; 135 \[86%\] were women and 22 \[14%\] were men). The methotrexate hold group had higher post-vaccination antibody titres compared with the control group (2553·5 IU/ml, IQR 1792·5-4823·8 vs 990·5, 356·1-2252·5; p\<0·0001). There were no reports of any serious adverse events during the trial period. Withholding methotrexate after both ChAdOx1 nCov-19 vaccine doses and after only the second dose led to higher anti-RBD antibody titres compared with continuation of methotrexate. However, withholding methotrexate only after the second vaccine dose resulted in a similar humoral response to holding methotrexate after both vaccine doses, without an increased risk of arthritis flares. Hence, interruption of methotrexate during the second dose of ChAdOx1 nCov-19 vaccine appears to be a safe and effective strategy to improve the antibody response in patients with rheumatoid or psoriatic arthritis. Indian Rheumatology Association. © 2022 Elsevier Ltd. All rights reserved. |
| 36314847 | An online community peer support intervention to promote COVID-19 vaccine information among essential workers: a randomized trial.                                                                                    | Annals of medicine                  | 2022 Dec        | Vaccine hesitancy is still rampant in the United States, including health care personnel. Vaccination of frontline essential workers (e.g. health care workers) is very important, especially during a pandemic. We tested the efficacy of a 4-week online, peer-led intervention (Harnessing Online Peer Education) to promote requests for COVID-19 vaccine information among essential workers. Participants (N = 120) and peer leaders (N = 12) were recruited through online advertisements from July 23 to August 20, 2021. Eligibility criteria included: 18 years or older, U.S. resident, English speaker, part of phase 1a or 1 b of COVID-19 vaccine rollout (e.g. frontline essential workers), hadn’t received a COVID-19 vaccine but able to receive one. This was a parallel assignment randomised trial. STATA was used to create a randomisation using a random number generator so that all possible assignments of participants and peer leaders to groups were equally likely. Participants were randomly assigned to intervention or control arms that consisted of two private, hidden Facebook groups, each with 30 participants. Peer leaders were randomly assigned to an intervention group, each with six peer leaders. Participants in the intervention arm were randomly assigned to three peer leaders. Participants were blinded after assignment. Peer leaders were tasked with reaching out to their assigned participants at least three times each week. Participants completed a baseline and a post intervention survey. The study is registered on ClinicalTrials.org under identifier NCT04376515 and is no longer recruiting. This work was supported by the NIAID under grant 5R01AI132030-05. A total of 101 participants analysed (50 intervention and 51 control). Six people in the intervention group and 0 people in the control group requested vaccine information. Ten people in the intervention group and six people in the control group provided proof of vaccination. The odds of requesting vaccine information in the intervention group was 13 times that in the control group (95% confidence interval: (1.5, 1772), p-value = 0.015). Thirty-seven participants in the intervention group and 31 in the control group were engaged at some point during the study. Results suggest peer-led online community groups may help to disseminate health information, aid public health efforts, and combat vaccine hesitancy. Key MessagesThe odds of requesting vaccine information was 13 times in the intervention group.Peer-led online communities may help to disseminate information and aid public health efforts to combat vaccine hesitancy.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

Some papers about Sars-Cov-2 Trial Vaccine

# Text Mining

## 1. Tokenize the abstracts and count the number of each token. Do you see anything interesting? Does removing stop words change what tokens appear as the most frequent? What are the 5 most common tokens for each search term after removing stopwords?

The dataset contains 3241 abstracts from articles across 5 search terms.

``` r
if (!file.exists("abstracts.csv")) {
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/03_pubmed/pubmed.csv", "abstracts.csv", method = "libcurl", timeout  = 60)
}
abs= read.csv("abstracts.csv")

str(abs)
```

    ## 'data.frame':    3241 obs. of  2 variables:
    ##  $ abstract: chr  "Background and aims: Many patients with coronavirus disease 2019 (COVID-19) have underlying cardiovascular (CV)"| __truncated__ "Introduction: Contradictory data have been reported on the incidence of stroke in patients with COVID-19 and th"| __truncated__ "This article aims at collecting all information needed for dentists regarding the COVID-19 pandemic throughout "| __truncated__ "OBJECTIVE. The objective of our study was to determine the misdiagnosis rate of radiologists for coronavirus di"| __truncated__ ...
    ##  $ term    : chr  "covid" "covid" "covid" "covid" ...

``` r
table(abs$term)
```

    ## 
    ##           covid cystic fibrosis      meningitis    preeclampsia prostate cancer 
    ##             981             376             317             780             787

``` r
abs= as_tibble(abs)
```

``` r
abs %>%
  unnest_tokens(word, abstract) %>%
  count(word, sort = TRUE) %>%
  top_n(15, n) %>%
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col()
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

For the top 15 common tokens, only “covid”, “19”, “patients”, “cancer”,
and “prostate” are meaningful. Others are stop words and do not convey
any interesting information.

### Remove stop words and numbers.

Since the only number we got above is “19” and it usually comes up with
“covid” under this circumstance, we can filter it out.

``` r
abs %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = c("word")) %>%
  count(word, sort = TRUE) %>%
  filter( !grepl(pattern = "^[0-9]+$", x = word)) %>%
  top_n(15, n) %>%
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col()
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

After removing stop words and numbers, the top 15 common tokens changed
drastically. Now all those tokens have meanings and can help us to know
these abstracts better. The most frequent word is “covid”, indicating
that most of these researches were about COVID-19.

### For each term:

``` r
abs %>%
  unnest_tokens(word, abstract) %>%
  group_by(term) %>%
  count(word, sort = TRUE) %>%
  filter( !(word %in% stop_words$word) & !grepl(pattern = "^[0-9]+$", x = word)) %>%
  top_n(5, n) %>%
  arrange(term, desc(n)) %>%
knitr::kable()
```

| term            | word         |    n |
|:----------------|:-------------|-----:|
| covid           | covid        | 7275 |
| covid           | patients     | 2293 |
| covid           | disease      |  943 |
| covid           | pandemic     |  800 |
| covid           | coronavirus  |  647 |
| covid           | health       |  647 |
| cystic fibrosis | fibrosis     |  867 |
| cystic fibrosis | cystic       |  862 |
| cystic fibrosis | cf           |  625 |
| cystic fibrosis | patients     |  586 |
| cystic fibrosis | disease      |  400 |
| meningitis      | patients     |  446 |
| meningitis      | meningitis   |  429 |
| meningitis      | meningeal    |  219 |
| meningitis      | csf          |  206 |
| meningitis      | clinical     |  187 |
| preeclampsia    | pre          | 2038 |
| preeclampsia    | eclampsia    | 2005 |
| preeclampsia    | preeclampsia | 1863 |
| preeclampsia    | women        | 1196 |
| preeclampsia    | pregnancy    |  969 |
| prostate cancer | cancer       | 3840 |
| prostate cancer | prostate     | 3832 |
| prostate cancer | patients     |  934 |
| prostate cancer | treatment    |  926 |
| prostate cancer | disease      |  652 |

1)  covid: covid, patients, disease, pandemic, coronavirus / health
    (both appeared 647 times)

2)  cystic fibrosis: fibrosis, cystic, cf, patients, disease

3)  meningitis: patients, meningitis, meningeal, csf, clinical

4)  preeclampsia: pre, eclampsia, preeclampsia, women, pregnancy

5)  prostate cancer: cancer, prostate, patients, treatment, disease

## 2. Tokenize the abstracts into bigrams. Find the 10 most common bigram and visualize them with ggplot2.

``` r
abs %>%
  unnest_ngrams(bigram, abstract, n=2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(n, fct_reorder(bigram, n))) +
  geom_col()
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Here we can observe that most common bigrams were about COVID-19,
Prostate cancer, and Preeclampsia.

## 3. Calculate the TF-IDF value for each word-search term combination. (here you want the search term to be the “document”) What are the 5 tokens from each search term with the highest TF-IDF value? How are the results different from the answers you got in question 1?

``` r
abs %>%
  unnest_tokens(word, abstract) %>%
  group_by(term) %>%
  count(word, term, sort = TRUE) %>%
  top_n(5, n) %>%
  bind_tf_idf(word, term, n) %>%
  arrange(term, desc(tf_idf)) %>%
knitr::kable()
```

| term            | word     |    n |        tf |       idf |    tf_idf |
|:----------------|:---------|-----:|----------:|----------:|----------:|
| covid           | covid    | 7275 | 0.1885692 | 1.6094379 | 0.3034904 |
| covid           | 19       | 7035 | 0.1823484 | 1.6094379 | 0.2934784 |
| covid           | the      | 9741 | 0.2524883 | 0.0000000 | 0.0000000 |
| covid           | of       | 7668 | 0.1987558 | 0.0000000 | 0.0000000 |
| covid           | and      | 6861 | 0.1778383 | 0.0000000 | 0.0000000 |
| cystic fibrosis | to       | 1011 | 0.1091086 | 0.9162907 | 0.0999752 |
| cystic fibrosis | in       | 1497 | 0.1615584 | 0.5108256 | 0.0825282 |
| cystic fibrosis | the      | 2677 | 0.2889057 | 0.0000000 | 0.0000000 |
| cystic fibrosis | of       | 2304 | 0.2486510 | 0.0000000 | 0.0000000 |
| cystic fibrosis | and      | 1777 | 0.1917764 | 0.0000000 | 0.0000000 |
| meningitis      | a        |  830 | 0.0965902 | 1.6094379 | 0.1554560 |
| meningitis      | in       | 1322 | 0.1538462 | 0.5108256 | 0.0785886 |
| meningitis      | the      | 2489 | 0.2896544 | 0.0000000 | 0.0000000 |
| meningitis      | of       | 2269 | 0.2640521 | 0.0000000 | 0.0000000 |
| meningitis      | and      | 1683 | 0.1958571 | 0.0000000 | 0.0000000 |
| preeclampsia    | to       | 2651 | 0.1030275 | 0.9162907 | 0.0944031 |
| preeclampsia    | in       | 3869 | 0.1503634 | 0.5108256 | 0.0768095 |
| preeclampsia    | the      | 7119 | 0.2766702 | 0.0000000 | 0.0000000 |
| preeclampsia    | of       | 6837 | 0.2657106 | 0.0000000 | 0.0000000 |
| preeclampsia    | and      | 5255 | 0.2042284 | 0.0000000 | 0.0000000 |
| prostate cancer | cancer   | 3840 | 0.1608646 | 1.6094379 | 0.2589017 |
| prostate cancer | prostate | 3832 | 0.1605295 | 1.6094379 | 0.2583623 |
| prostate cancer | the      | 6100 | 0.2555402 | 0.0000000 | 0.0000000 |
| prostate cancer | of       | 5682 | 0.2380294 | 0.0000000 | 0.0000000 |
| prostate cancer | and      | 4417 | 0.1850362 | 0.0000000 | 0.0000000 |

There were lots of meaningless and redundant stop words in the table, so
better get rid of them.

### Remove stop words and numbers:

``` r
abs %>%
  unnest_tokens(word, abstract) %>%
  group_by(term) %>%
  count(word, term, sort = TRUE) %>%
  filter( !(word %in% stop_words$word) & !grepl(pattern = "^[0-9]+$", x = word)) %>%
  top_n(5, n) %>%
  bind_tf_idf(word, term, n) %>%
  arrange(term, desc(tf_idf)) %>%
knitr::kable()
```

| term            | word         |    n |        tf |       idf |    tf_idf |
|:----------------|:-------------|-----:|----------:|----------:|----------:|
| covid           | covid        | 7275 | 0.5771519 | 1.6094379 | 0.9288902 |
| covid           | pandemic     |  800 | 0.0634669 | 1.6094379 | 0.1021460 |
| covid           | coronavirus  |  647 | 0.0513288 | 1.6094379 | 0.0826106 |
| covid           | health       |  647 | 0.0513288 | 1.6094379 | 0.0826106 |
| covid           | patients     | 2293 | 0.1819119 | 0.2231436 | 0.0405925 |
| covid           | disease      |  943 | 0.0748116 | 0.5108256 | 0.0382157 |
| cystic fibrosis | fibrosis     |  867 | 0.2595808 | 1.6094379 | 0.4177792 |
| cystic fibrosis | cystic       |  862 | 0.2580838 | 1.6094379 | 0.4153699 |
| cystic fibrosis | cf           |  625 | 0.1871257 | 1.6094379 | 0.3011673 |
| cystic fibrosis | disease      |  400 | 0.1197605 | 0.5108256 | 0.0611767 |
| cystic fibrosis | patients     |  586 | 0.1754491 | 0.2231436 | 0.0391503 |
| meningitis      | meningitis   |  429 | 0.2885003 | 1.6094379 | 0.4643234 |
| meningitis      | meningeal    |  219 | 0.1472764 | 1.6094379 | 0.2370322 |
| meningitis      | csf          |  206 | 0.1385340 | 1.6094379 | 0.2229618 |
| meningitis      | clinical     |  187 | 0.1257566 | 1.6094379 | 0.2023974 |
| meningitis      | patients     |  446 | 0.2999328 | 0.2231436 | 0.0669281 |
| preeclampsia    | pre          | 2038 | 0.2525090 | 1.6094379 | 0.4063975 |
| preeclampsia    | eclampsia    | 2005 | 0.2484203 | 1.6094379 | 0.3998170 |
| preeclampsia    | preeclampsia | 1863 | 0.2308264 | 1.6094379 | 0.3715008 |
| preeclampsia    | women        | 1196 | 0.1481849 | 1.6094379 | 0.2384943 |
| preeclampsia    | pregnancy    |  969 | 0.1200595 | 1.6094379 | 0.1932283 |
| prostate cancer | cancer       | 3840 | 0.3770621 | 1.6094379 | 0.6068580 |
| prostate cancer | prostate     | 3832 | 0.3762765 | 1.6094379 | 0.6055937 |
| prostate cancer | treatment    |  926 | 0.0909269 | 1.6094379 | 0.1463413 |
| prostate cancer | disease      |  652 | 0.0640220 | 0.5108256 | 0.0327041 |
| prostate cancer | patients     |  934 | 0.0917125 | 0.2231436 | 0.0204651 |

1)  covid: covid, pandemic, coronavirus / health (both TF-IDF values
    equal 0.0826106), patients, disease

2)  cystic fibrosis: fibrosis, cystic, cf, disease, patients

3)  meningitis: meningitis, meningeal, csf, clinial, patients

4)  preeclampsia: pre, eclampsia, preeclampsia, women, pregnancy

5)  prostate cancer: cancer，prostate, treatment, disease, patients

The order of top 5 common tokens were changed for each term. TF-IDF not
only focuses on the frequency of words but also provides the importance
of the words. For example, although “patients” were appearing very
frequently but its ranking here was lower than before for each term.
Thus, the important tokens were ranked higher and the key information
could be better presented by this method.
