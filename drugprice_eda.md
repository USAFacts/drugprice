Prescription Drug Price EDA
================
USAFactsTyler
22 May, 2019

-   [Introduction](#introduction)
    -   [Preliminary Cleaning/Summary](#preliminary-cleaningsummary)
-   [Body](#body)
    -   [Aggregate Trends](#aggregate-trends)
        -   [Medicare Part D](#medicare-part-d)
    -   [Univariatie Distribution Trends - Brand Level](#univariatie-distribution-trends---brand-level)
        -   [Medicare Part D](#medicare-part-d-1)
        -   [Medicare Part B](#medicare-part-b)
        -   [Medicaid](#medicaid)
    -   [Bivariate Change - Brand Level](#bivariate-change---brand-level)
        -   [Medicare Part D](#medicare-part-d-2)
        -   [Medicare Part B](#medicare-part-b-1)
        -   [Medicaid](#medicaid-1)
    -   [Manufacturer Level](#manufacturer-level)
    -   [Insulin](#insulin)
-   [Conclusion](#conclusion)

Introduction
============

This document reflects my exploration of the datasets found at the [CMS Drug Spending](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Information-on-Prescription-Drugs/index.html) site. This document is broken up into three different sections with the following research questions driving each phase of data exploration:

1.  What is the relationship between spend and claims at a brand level and has that relationship changed over the past five years?
2.  Is there a material difference in the relationships discovered in Research Question 1 between the three programs for which CMS has provided data, namely Medicare Part B, Medicare Part D and Medicaid?
3.  Where there is manufacturer level information available, does the relationship between spend and claims vary by the manufacturer, and if so, are certain manufacturer's exhibiting higher costs generally across brands?

Preliminary Cleaning/Summary
----------------------------

Theree are two things that need to be done with these datasets before we can start exploring them:

1.  General data cleaning tasks such as changing variable types to the appropriate values and omitting missing values.
2.  Convert the total spend metrics across each of the three programs to be inflation adjusted. Given that we are generally interested in comparing how things have changed over time, I will be using the inflation adjusted spend values as a default.

``` r
rm(list=ls(all=T))

library(tidyverse)
library(tidyselect)
library(viridis)
library(ggthemes)
library(ggridges)
```

``` r
medicaid <- read_csv("medicaidsrc.csv")
partb <- read_csv("medicarepartb.csv")
partd <- read_csv("medicarepartd.csv")
```

``` r
infrate <- tibble(Year = as_factor(c(2013, 2014, 2015, 2016, 2017)), inf_rate = c(1.049922855, 1.033187717, 1.029987669, 1.020514977, 1))

partd <- partd %>%
  mutate_at(c("Brand Name", "Generic Name", "Manufacturer", "Year"), as_factor) %>%
  na.omit(.) %>%
  left_join(infrate, by = "Year") %>%
  mutate(`Inf Total Spend` = signif(`Total Spending` * inf_rate, 3)) %>%
  select(-inf_rate)

colnames(partd)[6] <- "Total Claims"

partb <- partb %>%
  mutate_at(c("Medicare Billing Code (HCPCS Code)", "Brand Name", "Generic Name", "Year"), as_factor) %>%
  na.omit(.) %>%
  left_join(infrate, by = "Year") %>%
  mutate(`Inf Total Spend` = signif(`Total Spending` * inf_rate, 3)) %>%
  select(-inf_rate)

medicaid <- medicaid %>%
  mutate_at(c("Brand Name", "Generic Name", "Manufacturer", "Year"), as_factor) %>%
  na.omit(.) %>%
  left_join(infrate, by = "Year") %>%
  mutate(`Inf Total Spend` = signif(`Total Spending` * inf_rate, 3)) %>%
  select(-inf_rate)
```

``` r
summary(partd)
```

    ##                Brand Name                            Generic Name  
    ##  Insulin Syringe    :  205   Pen Needle, Diabetic          :  211  
    ##  Levetiracetam*     :  131   Syringe And Needle,Insulin,1ml:  207  
    ##  Amlodipine Besylate:  122   Metformin HCl                 :  188  
    ##  Gabapentin         :  120   Alcohol Antiseptic Pads       :  182  
    ##  Montelukast Sodium :  110   Syringe-Needle,Insulin,0.5 Ml :  179  
    ##  Lisinopril         :  103   Syring-Needl,Disp,Insul,0.3 Ml:  165  
    ##  (Other)            :27034   (Other)                       :26693  
    ##           Manufacturer   Total Spending      Total Dosage Units 
    ##  Mylan          : 1383   Min.   :5.000e+01   Min.   :2.500e+01  
    ##  Sandoz         : 1192   1st Qu.:1.083e+05   1st Qu.:7.139e+04  
    ##  Teva USA       : 1128   Median :1.164e+06   Median :5.946e+05  
    ##  Mylan Instituti:  770   Mean   :2.213e+07   Mean   :1.541e+07  
    ##  AHP            :  769   3rd Qu.:7.244e+06   3rd Qu.:5.485e+06  
    ##  Apotex Corp    :  541   Max.   :7.031e+09   Max.   :1.665e+09  
    ##  (Other)        :22042                                          
    ##   Total Claims      Total Beneficiaries
    ##  Min.   :      11   Min.   :     11    
    ##  1st Qu.:    1488   1st Qu.:    542    
    ##  Median :   11020   Median :   3817    
    ##  Mean   :  228747   Mean   :  71155    
    ##  3rd Qu.:   97029   3rd Qu.:  33925    
    ##  Max.   :22488587   Max.   :5273526    
    ##                                        
    ##  Average Spending Per Dosage Unit (Weighted) Average Spending Per Claim
    ##  Min.   :    0.00                            Min.   :     0.42         
    ##  1st Qu.:    0.31                            1st Qu.:    18.48         
    ##  Median :    1.07                            Median :    55.90         
    ##  Mean   :   74.05                            Mean   :   600.94         
    ##  3rd Qu.:    5.12                            3rd Qu.:   222.06         
    ##  Max.   :39026.76                            Max.   :119126.15         
    ##                                                                        
    ##  Average Spending Per Beneficiary   Year      Inf Total Spend    
    ##  Min.   :      0.0                2013:4380   Min.   :5.200e+01  
    ##  1st Qu.:     48.5                2014:5024   1st Qu.:1.100e+05  
    ##  Median :    151.1                2015:5609   Median :1.190e+06  
    ##  Mean   :   3298.0                2016:5621   Mean   :2.266e+07  
    ##  3rd Qu.:    659.3                2017:7191   3rd Qu.:7.410e+06  
    ##  Max.   :1218965.3                            Max.   :7.240e+09  
    ## 

Looking at the summary for Medicare Part D, there are four things that I want to highlight:

1.  The distance between the first two quartiles and the max value is heavily right skewed for each of the "Total" metrics. This likely implies there are some large outliers for each of the univariate distributions
2.  There are many more drugs on the market in 2017 than in 2013. The jump between 2016 and 2017 was especially large, at a 27% increase in one year.
3.  There top 6 manufacturers supplied 26% of all brands in this time frame, implying the market is characterized by several large players but also consists of many smaller and mid-level firms.
4.  The products with the most brand nad manufacturer combinations seem to be diabetic insulin needles.

``` r
summary(partb)
```

    ##  Medicare Billing Code (HCPCS Code) Drug Description  
    ##  90371  :   5                       Length:2177       
    ##  90375  :   5                       Class :character  
    ##  90376  :   5                       Mode  :character  
    ##  90586  :   5                                         
    ##  90632  :   5                                         
    ##  90670  :   5                                         
    ##  (Other):2147                                         
    ##                       Brand Name                        Generic Name 
    ##  HyperHEP B S-D*           :   5   MethylprednIsolone Acetate*:  15  
    ##  HyperRAB S-D              :   5   0.9 % Sodium Chloride      :  15  
    ##  Imogam Rabies-HT          :   5   Antihemophilic Factor/VWF  :  15  
    ##  BCG (Tice Strain) (90586)*:   5   Methotrexate Sodium*       :  15  
    ##  Havrix*                   :   5   Tacrolimus                 :  11  
    ##  Prevnar 13                :   5   Rabies Immune Globulin/PF  :  10  
    ##  (Other)                   :2147   (Other)                    :2096  
    ##  Total Spending      Total Dosage Units   Total Claims    
    ##  Min.   :1.100e+01   Min.   :       58   Min.   :     15  
    ##  1st Qu.:1.158e+05   1st Qu.:    15732   1st Qu.:   1341  
    ##  Median :1.267e+06   Median :   142686   Median :   7630  
    ##  Mean   :5.544e+07   Mean   :  5606214   Mean   : 108766  
    ##  3rd Qu.:1.910e+07   3rd Qu.:  1733469   3rd Qu.:  60414  
    ##  Max.   :2.466e+09   Max.   :428726351   Max.   :5743305  
    ##                                                           
    ##  Total Beneficiaries Average Spending Per Dosage Unit
    ##  Min.   :     11     Min.   :    0.01                
    ##  1st Qu.:    352     1st Qu.:    1.97                
    ##  Median :   2404     Median :    9.54                
    ##  Mean   :  45370     Mean   :  215.16                
    ##  3rd Qu.:  13656     3rd Qu.:   50.47                
    ##  Max.   :5693733     Max.   :38716.24                
    ##                                                      
    ##  Average Spending Per Claim Average Spending Per Beneficiary   Year    
    ##  Min.   :    0.10           Min.   :     0.2                 2013:407  
    ##  1st Qu.:   16.19           1st Qu.:    37.2                 2014:426  
    ##  Median :  189.97           Median :   637.2                 2015:435  
    ##  Mean   : 2635.10           Mean   : 19519.8                 2016:454  
    ##  3rd Qu.: 1992.93           3rd Qu.:  7374.7                 2017:455  
    ##  Max.   :58621.51           Max.   :792223.8                           
    ##                                                                        
    ##  Inf Total Spend    
    ##  Min.   :1.200e+01  
    ##  1st Qu.:1.190e+05  
    ##  Median :1.300e+06  
    ##  Mean   :5.681e+07  
    ##  3rd Qu.:1.970e+07  
    ##  Max.   :2.470e+09  
    ## 

Looking at the summary for Medicare Part B, there are three things that I want to highlight:

1.  There is no manufacturer level information for this particular dataset
2.  The Total metrics are again all heavily right skewed
3.  The introduction of drugs over each of the years has been much slower than Medicare Part D.

``` r
summary(medicaid)
```

    ##                Brand Name                Generic Name  
    ##  Levetiracetam*     :  164   Levetiracetam     :  204  
    ##  Ondansetron HCl*   :  127   Metformin HCl     :  170  
    ##  Promethazine HCl*  :  126   Promethazine HCl  :  144  
    ##  Montelukast Sodium :  114   Lamotrigine       :  141  
    ##  Gabapentin         :  113   Potassium Chloride:  140  
    ##  Amlodipine Besylate:  107   Diltiazem HCl     :  136  
    ##  (Other)            :33682   (Other)           :33498  
    ##           Manufacturer   Total Spending      Total Dosage Units 
    ##  Mylan          : 1369   Min.   :0.000e+00   Min.   :2.000e+00  
    ##  Teva USA       : 1295   1st Qu.:5.988e+04   1st Qu.:2.205e+04  
    ##  Sandoz         : 1260   Median :4.272e+05   Median :1.948e+05  
    ##  Actavis Pharma/: 1037   Mean   :7.678e+06   Mean   :5.265e+06  
    ##  Mylan Instituti:  868   3rd Qu.:2.416e+06   3rd Qu.:1.757e+06  
    ##  Ahp            :  686   Max.   :2.465e+09   Max.   :1.220e+09  
    ##  (Other)        :27918                                          
    ##   Total Claims      Average Spending Per Dosage Unit (Weighted)
    ##  Min.   :      11   Min.   :    0.00                           
    ##  1st Qu.:     852   1st Qu.:    0.39                           
    ##  Median :    5234   Median :    1.45                           
    ##  Mean   :   86611   Mean   :   73.21                           
    ##  3rd Qu.:   37905   3rd Qu.:    7.32                           
    ##  Max.   :10036481   Max.   :33809.93                           
    ##                                                                
    ##  Average Spending Per Claim   Year      Inf Total Spend    
    ##  Min.   :    0.00           2013:5555   Min.   :0.000e+00  
    ##  1st Qu.:   17.20           2014:6292   1st Qu.:6.110e+04  
    ##  Median :   52.26           2015:7066   Median :4.380e+05  
    ##  Mean   :  596.73           2016:7760   Mean   :7.859e+06  
    ##  3rd Qu.:  179.16           2017:7760   3rd Qu.:2.470e+06  
    ##  Max.   :99527.18                       Max.   :2.550e+09  
    ## 

Looking at the summary for Medicaid, there are three things I want to highlight:

1.  Surprisingly, there does not appear to be any change in the number of drugs offered between 2016 and 2017. There is still
2.  The Total Metrics are again heavily right skewed
3.  The medicaid information doesn't contain benificiary information, but does contain manufacturer level data.

Body
====

Aggregate Trends
----------------

### Medicare Part D

``` r
partd %>%
  group_by(Year) %>%
  summarise(`Total Spend` = sum(`Inf Total Spend`), `Total Claims` = sum(`Total Claims`), `Total Beneficiaries` = sum(`Total Beneficiaries`)) %>%
  gather(Metric, Value, -Year) %>% 
  ggplot(aes(x = Year, y = Value, colour = Metric)) +
    geom_line(aes(group=1)) +
    facet_grid(Metric ~ ., scales = "free_y") +
    scale_color_viridis(discrete = T, option = "D", end = 0.7) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(legend.position = "none", panel.spacing = unit(2, "lines"))
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_agg_totalmetrics-1.png)

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(Year) %>%
  summarise(`Total Spend` = sum(`Inf Total Spend`), `Total Claims` = sum(`Total Claims`), `Total Beneficiaries` = sum(`Total Beneficiaries`)) %>%
  gather(Metric, Value, -Year) %>% 
  ggplot(aes(x = Year, y = Value, colour = Metric)) +
    geom_line(aes(group=1)) +
    facet_grid(Metric ~ ., scales = "free_y") +
    scale_color_viridis(discrete = T, option = "D", end = 0.7) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(legend.position = "none", panel.spacing = unit(2, "lines"))
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_agg_totalmetrics-2.png)

``` r
partd %>%
  group_by(Year) %>%
  summarise(`Average Spend Per Claim` = sum(`Inf Total Spend`)/sum(`Total Claims`), `Average Spend Per Beneficiary` = sum(`Inf Total Spend`)/sum(`Total Beneficiaries`)) %>%
  gather(Metric, Value, -Year) %>% 
  ggplot(aes(x = Year, y = Value, colour = Metric)) +
    geom_line(aes(group=1)) +
    facet_grid(Metric ~ ., scales = "free_y") +
    scale_color_viridis(discrete = T, option = "D", end = 0.7) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(legend.position = "none", panel.spacing = unit(2, "lines"))
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_agg_avgper-1.png)

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(Year) %>%
  summarise(`Average Spend Per Claim` = sum(`Inf Total Spend`)/sum(`Total Claims`), `Average Spend Per Beneficiary` = sum(`Inf Total Spend`)/sum(`Total Beneficiaries`)) %>%
  gather(Metric, Value, -Year) %>% 
  ggplot(aes(x = Year, y = Value, colour = Metric)) +
    geom_line(aes(group=1)) +
    facet_grid(Metric ~ ., scales = "free_y") +
    scale_color_viridis(discrete = T, option = "D", end = 0.7) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(legend.position = "none", panel.spacing = unit(2, "lines"))
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_agg_avgper-2.png)

Univariatie Distribution Trends - Brand Level
---------------------------------------------

### Medicare Part D

Just to visually confirm what I was seeing in the quartile summaries in the introduction, I want to look at a ridgeplot of each of the three primary metrics that I'm interested in: Total Spend, Total Claims, and Total Beneficiaries.

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  ggplot(aes(x = `Inf Total Spend`, y = Year)) +
  geom_density_ridges() +
  theme_minimal(base_size = 14) + theme(axis.text.y = element_text(vjust = 0)) +
  xlim(0, 1e7)
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_univariate-1.png)

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  ggplot(aes(x = `Total Claims`, y = Year)) +
  geom_density_ridges() +
  theme_minimal(base_size = 14) + theme(axis.text.y = element_text(vjust = 0)) +
  xlim(0, 1e5)
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_univariate-2.png)

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  ggplot(aes(x = `Total Beneficiaries`, y = Year)) +
  geom_density_ridges() +
  theme_minimal(base_size = 14) + theme(axis.text.y = element_text(vjust = 0)) +
  xlim(0, 1e5)
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_univariate-3.png)

These metrics are so heavily right skewed that it's not fruitful to analyze if the raw distributions have shown any meaningful change over time. As such I want to look at the "total" metrics from a log-transformed univariate perspective across each of the three datasets. Ridgeplots are my preferred way to look at univariate distributions separated by a factor level so I'll continue to use them heavily through this part of the EDA.

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`), 
            brandbens = sum(`Total Beneficiaries`)) %>%
  ungroup(.) %>%
  mutate(logspend = log(brandspend)) %>%
  ggplot(aes(x = logspend, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Spend", y = "Year", title = "Total Spend of Medicare Part D on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_univariate_log_spend-1.png)

Looking at Medicare Part D's log transformed Total Spend variabile by year,

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`), 
            brandbens = sum(`Total Beneficiaries`)) %>%
  ungroup(.) %>%
  mutate(logclaims = log(brandclaims)) %>%
  ggplot(aes(x = logclaims, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Claims", y = "Year", title = "Total Claims of Medicare Part D on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_univariate_log_claims-1.png)

``` r
partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`), 
            brandbens = sum(`Total Beneficiaries`)) %>%
  ungroup(.) %>%
  mutate(logbens = log(brandbens)) %>%
  ggplot(aes(x = logbens, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Beneficiaries", y = "Year", title = "Total Beneficiaries of Medicare Part D on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/partd_eda_univariate_log_bens-1.png)

### Medicare Part B

``` r
partb %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partb, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  mutate(logspend = log(`Inf Total Spend`)) %>%
  ggplot(aes(x = logspend, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Spend", y = "Year", title = "Total Spend of Medicare Part B on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/partb_eda_univariate_log_spend-1.png)

``` r
partb %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partb, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  mutate(logclaims = log(`Total Claims`)) %>%
  ggplot(aes(x = logclaims, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Claims", y = "Year", title = "Total Claims of Medicare Part B on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/partb_eda_univariate_log_claims-1.png)

``` r
partb %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partb, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  mutate(logbens = log(`Total Beneficiaries`)) %>%
  ggplot(aes(x = logbens, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Beneficiaries", y = "Year", title = "Total Beneficiaries of Medicare Part B on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/partb_eda_univariate_log_bens-1.png)

### Medicaid

``` r
medicaid %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(medicaid, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`)) %>%
  ungroup(.) %>%
  mutate(logspend = log1p(brandspend)) %>%
  ggplot(aes(x = logspend, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Spend", y = "Year", title = "Total Spend of Medicaid on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/medicaid_eda_univariate_log_spend-1.png)

``` r
medicaid %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(medicaid, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`)) %>%
  ungroup(.) %>%
  mutate(logclaims = log1p(brandclaims)) %>%
  ggplot(aes(x = logclaims, y = fct_relevel(Year, rev), fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles", option = "E") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0)) +
  labs(x = "Log of Total Claims", y = "Year", title = "Total Claims of Medicaid on prescription drugs\nfor only brands available all five years")
```

![](drugprice_eda_files/figure-markdown_github/medicaid_eda_univariate_log_claims-1.png)

Bivariate Change - Brand Level
------------------------------

### Medicare Part D

``` r
chngyr <- 2017 - 2013

partd_brandchng <- partd %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partd, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`), 
            brandbens = sum(`Total Beneficiaries`)) %>%
  ungroup(.) %>%
  group_by(`Brand Name`) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(nomchngspend = brandspend - lag(brandspend, n = chngyr), 
         nomchngdose = branddose - lag(branddose, n = chngyr), 
         nomchngclaims = brandclaims - lag(brandclaims, n = chngyr), 
         nomchngbens = brandbens - lag(brandbens, n = chngyr),
         perchngspend = (brandspend-lag(brandspend, n = chngyr))/lag(brandspend, n = chngyr),
         perchngdose = (branddose-lag(branddose, n = chngyr))/lag(branddose, n = chngyr),
         perchngclaims = (brandclaims-lag(brandclaims, n = chngyr))/lag(brandclaims, n = chngyr),
         perchngbens = (brandbens-lag(brandbens, n = chngyr))/lag(brandbens, n = chngyr)
         ) %>%
  fill(nomchngspend, 
       nomchngdose, 
       nomchngclaims, 
       nomchngbens,
       perchngspend,
       perchngdose,
       perchngclaims,
       perchngbens,
       .direction = "up")

partd_brandchng %>%
  select(everything(), -Year, -brandspend, -branddose, -brandclaims, -brandbens) %>%
  distinct() %>%
  ggplot(aes(x = nomchngspend, y = nomchngclaims)) +
  geom_point(stat = "identity", position = "jitter") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/partd_change-1.png)

``` r
partd_brandchng %>%
  select(everything(), -Year, -brandspend, -branddose, -brandclaims, -brandbens) %>%
  distinct() %>%
  ggplot(aes(x = perchngspend, y = perchngclaims)) +
  geom_point(stat = "identity", position = "jitter") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/partd_change-2.png)

### Medicare Part B

``` r
partb_brandchng <- partb %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(partb, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`), 
            brandbens = sum(`Total Beneficiaries`)) %>%
  ungroup(.) %>%
  group_by(`Brand Name`) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(nomchngspend = brandspend - lag(brandspend, n = chngyr), 
         nomchngdose = branddose - lag(branddose, n = chngyr), 
         nomchngclaims = brandclaims - lag(brandclaims, n = chngyr), 
         nomchngbens = brandbens - lag(brandbens, n = chngyr),
         perchngspend = (brandspend/lag(brandspend, n = chngyr)) - 1,
         perchngdose = (branddose/lag(branddose, n = chngyr)) - 1,
         perchngclaims = (brandclaims/lag(brandclaims, n = chngyr)) - 1,
         perchngbens = (brandbens/lag(brandbens, n = chngyr)) - 1) %>%
  fill(nomchngspend, 
       nomchngdose, 
       nomchngclaims, 
       nomchngbens,
       perchngspend,
       perchngdose,
       perchngclaims,
       perchngbens,
       .direction = "up")

partb_brandchng %>%
  select(everything(), -Year, -brandspend, -branddose, -brandclaims, -brandbens) %>%
  distinct() %>%
  ggplot(aes(x = nomchngspend, y = nomchngclaims)) +
  geom_point(stat = "identity") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/partb_change-1.png)

``` r
partb_brandchng %>%
  select(everything(), -Year, -brandspend, -branddose, -brandclaims, -brandbens) %>%
  distinct() %>%
  ggplot(aes(x = perchngspend, y = perchngclaims)) +
  geom_point(stat = "identity") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/partb_change-2.png)

### Medicaid

``` r
medicaid_brandchng <- medicaid %>%
  group_by(`Brand Name`) %>%
  summarise(numyrs = n_distinct(Year)) %>%
  ungroup(.) %>%
  right_join(medicaid, by = "Brand Name") %>%
  filter(numyrs == 5) %>%
  group_by(`Brand Name`, Year) %>%
  summarise(brandspend = sum(`Inf Total Spend`), 
            branddose = sum(`Total Dosage Units`), 
            brandclaims = sum(`Total Claims`)) %>%
  ungroup(.) %>%
  group_by(`Brand Name`) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(nomchngspend = brandspend - lag(brandspend, n = chngyr), 
         nomchngdose = branddose - lag(branddose, n = chngyr), 
         nomchngclaims = brandclaims - lag(brandclaims, n = chngyr),
         perchngspend = (brandspend/lag(brandspend, n = chngyr)) - 1,
         perchngdose = (branddose/lag(branddose, n = chngyr)) - 1,
         perchngclaims = (brandclaims/lag(brandclaims, n = chngyr)) - 1) %>%
  fill(nomchngspend, 
       nomchngdose, 
       nomchngclaims,
       perchngspend,
       perchngdose,
       perchngclaims,
       .direction = "up")

medicaid_brandchng %>%
  select(everything(), -Year, -brandspend, -branddose, -brandclaims) %>%
  distinct() %>%
  ggplot(aes(x = nomchngspend, y = nomchngclaims)) +
  geom_point(stat = "identity") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/medicaid_change-1.png)

``` r
medicaid_brandchng %>%
  select(everything(), -Year, -brandspend, -branddose, -brandclaims) %>%
  distinct() %>%
  ggplot(aes(x = perchngspend, y = perchngclaims)) +
  geom_point(stat = "identity") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/medicaid_change-2.png)

Manufacturer Level
------------------

``` r
partd_manufac <- partd %>%
  group_by(`Brand Name`, Year) %>%
  mutate(avg_spend_per_claim_brand = signif(sum(`Inf Total Spend`)/sum(`Total Claims`), 3)) %>%
  group_by(Manufacturer, add = TRUE) %>%
  mutate(avg_spend_per_claim_manufac = sum(`Inf Total Spend`)/sum(`Total Claims`)) %>%
  mutate(avg_spend_per_claim_resid = signif(avg_spend_per_claim_manufac, 3) - signif(avg_spend_per_claim_brand, 3), 
         avg_spend_per_claim_status = ifelse(avg_spend_per_claim_resid == 0, "at_average", ifelse(avg_spend_per_claim_resid < 0, "below_average", "above_average")), 
         market_diff = avg_spend_per_claim_resid * `Total Claims`) %>%
  ungroup(.) %>%
  group_by(Manufacturer, Year, avg_spend_per_claim_status) %>%
  summarise(manufacturer_diff = sum(market_diff), price_status_drugs = n()) 
```

``` r
medicaid_manufac <- medicaid %>%
  group_by(`Brand Name`, Year) %>%
  mutate(avg_spend_per_claim_brand = signif(sum(`Inf Total Spend`)/sum(`Total Claims`), 3)) %>%
  group_by(Manufacturer, add = TRUE) %>%
  mutate(avg_spend_per_claim_manufac = sum(`Inf Total Spend`)/sum(`Total Claims`)) %>%
  mutate(avg_spend_per_claim_resid = signif(avg_spend_per_claim_manufac, 3) - signif(avg_spend_per_claim_brand, 3), 
         avg_spend_per_claim_status = ifelse(avg_spend_per_claim_resid == 0, "at_average", ifelse(avg_spend_per_claim_resid < 0, "below_average", "above_average")), 
         market_diff = avg_spend_per_claim_resid * `Total Claims`) %>%
  ungroup(.) %>%
  group_by(Manufacturer, Year, avg_spend_per_claim_status) %>%
  summarise(manufacturer_diff = sum(market_diff), price_status_drugs = n()) 
```

Insulin
-------

``` r
partd_insulin <- partd %>% 
  filter(str_detect(`Generic Name`, "Insulin")) %>%
  group_by(Year) %>%
  mutate(totalspend = sum(`Inf Total Spend`), totalbens = sum(`Total Beneficiaries`))

partd_insulin %>%
  ggplot(aes(x = Year, y = totalspend, fill = totalspend)) +
  geom_bar(stat = "identity") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/insulin%20exploration-1.png)

``` r
partd_insulin %>% 
  ggplot(aes(x = Year, y = totalbens, fill = totalbens)) +
  geom_bar(stat = "identity") +
  theme_minimal()
```

![](drugprice_eda_files/figure-markdown_github/insulin%20exploration-2.png)

Conclusion
==========
