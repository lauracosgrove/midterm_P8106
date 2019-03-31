Stepwise and GAM
================
Alyssa Vanderbeek (amv2187)
3/31/2019

Data import

``` r
heart = readr::read_csv("data/train_noNA.csv") %>%
  mutate(population_ruccs = as.factor(population_ruccs),
         urban_influence = as.factor(urban_influence),
         metro_ruccs = as.factor(metro_ruccs)) %>% 
  select(-row_id, -yr)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   metro_ruccs = col_character(),
    ##   population_ruccs = col_character(),
    ##   economic_typology = col_character(),
    ##   yr = col_character(),
    ##   pure_population = col_character(),
    ##   metro_adjacency = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
test = readr::read_csv("data/test.csv") %>% 
  mutate(population_ruccs = as.factor(population_ruccs),
         urban_influence = as.factor(urban_influence),
         metro_ruccs = as.factor(metro_ruccs)) %>% 
  select(-row_id, -yr) %>%
  drop_na()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   metro_ruccs = col_character(),
    ##   population_ruccs = col_character(),
    ##   economic_typology = col_character(),
    ##   yr = col_character(),
    ##   pure_population = col_character(),
    ##   metro_adjacency = col_character()
    ## )
    ## See spec(...) for full column specifications.

Stepwise model

``` r
# Stepwise model selection
step.fit = step(lm(heart_disease_mortality_per_100k ~ ., data = heart))
```

    ## Start:  AIC=11104.9
    ## heart_disease_mortality_per_100k ~ metro_ruccs + population_ruccs + 
    ##     urban_influence + economic_typology + econ__pct_civilian_labor + 
    ##     econ__pct_unemployment + econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__pct_adults_with_some_college + demo__pct_adults_bachelors_or_higher + 
    ##     demo__birth_rate_per_1k + demo__death_rate_per_1k + health__pct_adult_obesity + 
    ##     health__pct_adult_smoking + health__pct_diabetes + health__pct_low_birthweight + 
    ##     health__pct_physical_inacticity + health__air_pollution_particulate_matter + 
    ##     health__motor_vehicle_crash_deaths_per_100k + health__pop_per_dentist + 
    ##     health__pop_per_primary_care_physician + demo__pct_nonwhite + 
    ##     pure_population + metro_adjacency
    ## 
    ## 
    ## Step:  AIC=11104.9
    ## heart_disease_mortality_per_100k ~ metro_ruccs + population_ruccs + 
    ##     urban_influence + economic_typology + econ__pct_civilian_labor + 
    ##     econ__pct_unemployment + econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__pct_adults_with_some_college + demo__pct_adults_bachelors_or_higher + 
    ##     demo__birth_rate_per_1k + demo__death_rate_per_1k + health__pct_adult_obesity + 
    ##     health__pct_adult_smoking + health__pct_diabetes + health__pct_low_birthweight + 
    ##     health__pct_physical_inacticity + health__air_pollution_particulate_matter + 
    ##     health__motor_vehicle_crash_deaths_per_100k + health__pop_per_dentist + 
    ##     health__pop_per_primary_care_physician + demo__pct_nonwhite + 
    ##     pure_population
    ## 
    ## 
    ## Step:  AIC=11104.9
    ## heart_disease_mortality_per_100k ~ metro_ruccs + population_ruccs + 
    ##     urban_influence + economic_typology + econ__pct_civilian_labor + 
    ##     econ__pct_unemployment + econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__pct_adults_with_some_college + demo__pct_adults_bachelors_or_higher + 
    ##     demo__birth_rate_per_1k + demo__death_rate_per_1k + health__pct_adult_obesity + 
    ##     health__pct_adult_smoking + health__pct_diabetes + health__pct_low_birthweight + 
    ##     health__pct_physical_inacticity + health__air_pollution_particulate_matter + 
    ##     health__motor_vehicle_crash_deaths_per_100k + health__pop_per_dentist + 
    ##     health__pop_per_primary_care_physician + demo__pct_nonwhite
    ## 
    ## 
    ## Step:  AIC=11104.9
    ## heart_disease_mortality_per_100k ~ metro_ruccs + population_ruccs + 
    ##     urban_influence + economic_typology + econ__pct_civilian_labor + 
    ##     econ__pct_unemployment + econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__pct_adults_with_some_college + demo__birth_rate_per_1k + 
    ##     demo__death_rate_per_1k + health__pct_adult_obesity + health__pct_adult_smoking + 
    ##     health__pct_diabetes + health__pct_low_birthweight + health__pct_physical_inacticity + 
    ##     health__air_pollution_particulate_matter + health__motor_vehicle_crash_deaths_per_100k + 
    ##     health__pop_per_dentist + health__pop_per_primary_care_physician + 
    ##     demo__pct_nonwhite
    ## 
    ## 
    ## Step:  AIC=11104.9
    ## heart_disease_mortality_per_100k ~ population_ruccs + urban_influence + 
    ##     economic_typology + econ__pct_civilian_labor + econ__pct_unemployment + 
    ##     econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__pct_adults_with_some_college + demo__birth_rate_per_1k + 
    ##     demo__death_rate_per_1k + health__pct_adult_obesity + health__pct_adult_smoking + 
    ##     health__pct_diabetes + health__pct_low_birthweight + health__pct_physical_inacticity + 
    ##     health__air_pollution_particulate_matter + health__motor_vehicle_crash_deaths_per_100k + 
    ##     health__pop_per_dentist + health__pop_per_primary_care_physician + 
    ##     demo__pct_nonwhite
    ## 
    ##                                                    Df Sum of Sq     RSS
    ## - population_ruccs                                  5      1918 1532780
    ## - economic_typology                                 5      6449 1537311
    ## - econ__pct_unemployment                            1       202 1531063
    ## - demo__pct_adults_with_some_college                1      1082 1531944
    ## <none>                                                          1530861
    ## - demo__pct_below_18_years_of_age                   1      2035 1532896
    ## - health__pct_adult_smoking                         1      2177 1533038
    ## - demo__pct_adults_less_than_a_high_school_diploma  1      2938 1533800
    ## - health__pop_per_primary_care_physician            1      3131 1533992
    ## - demo__pct_female                                  1      4029 1534891
    ## - demo__birth_rate_per_1k                           1      4268 1535130
    ## - health__pop_per_dentist                           1      4900 1535761
    ## - econ__pct_uninsured_children                      1      6389 1537250
    ## - health__pct_diabetes                              1      6399 1537260
    ## - health__motor_vehicle_crash_deaths_per_100k       1      7027 1537888
    ## - demo__pct_adults_with_high_school_diploma         1      8049 1538910
    ## - health__pct_adult_obesity                         1      8376 1539238
    ## - health__air_pollution_particulate_matter          1     12645 1543506
    ## - econ__pct_civilian_labor                          1     14838 1545699
    ## - econ__pct_uninsured_adults                        1     17034 1547895
    ## - demo__pct_nonwhite                                1     18521 1549383
    ## - urban_influence                                   8     32695 1563556
    ## - health__pct_low_birthweight                       1     35928 1566790
    ## - health__pct_physical_inacticity                   1     52298 1583159
    ## - demo__pct_aged_65_years_and_older                 1    132436 1663297
    ## - demo__death_rate_per_1k                           1    195824 1726686
    ##                                                      AIC
    ## - population_ruccs                                 11097
    ## - economic_typology                                11102
    ## - econ__pct_unemployment                           11103
    ## - demo__pct_adults_with_some_college               11104
    ## <none>                                             11105
    ## - demo__pct_below_18_years_of_age                  11105
    ## - health__pct_adult_smoking                        11105
    ## - demo__pct_adults_less_than_a_high_school_diploma 11106
    ## - health__pop_per_primary_care_physician           11106
    ## - demo__pct_female                                 11107
    ## - demo__birth_rate_per_1k                          11107
    ## - health__pop_per_dentist                          11108
    ## - econ__pct_uninsured_children                     11110
    ## - health__pct_diabetes                             11110
    ## - health__motor_vehicle_crash_deaths_per_100k      11110
    ## - demo__pct_adults_with_high_school_diploma        11111
    ## - health__pct_adult_obesity                        11112
    ## - health__air_pollution_particulate_matter         11116
    ## - econ__pct_civilian_labor                         11118
    ## - econ__pct_uninsured_adults                       11121
    ## - demo__pct_nonwhite                               11122
    ## - urban_influence                                  11123
    ## - health__pct_low_birthweight                      11140
    ## - health__pct_physical_inacticity                  11157
    ## - demo__pct_aged_65_years_and_older                11236
    ## - demo__death_rate_per_1k                          11296
    ## 
    ## Step:  AIC=11096.91
    ## heart_disease_mortality_per_100k ~ urban_influence + economic_typology + 
    ##     econ__pct_civilian_labor + econ__pct_unemployment + econ__pct_uninsured_adults + 
    ##     econ__pct_uninsured_children + demo__pct_female + demo__pct_below_18_years_of_age + 
    ##     demo__pct_aged_65_years_and_older + demo__pct_adults_less_than_a_high_school_diploma + 
    ##     demo__pct_adults_with_high_school_diploma + demo__pct_adults_with_some_college + 
    ##     demo__birth_rate_per_1k + demo__death_rate_per_1k + health__pct_adult_obesity + 
    ##     health__pct_adult_smoking + health__pct_diabetes + health__pct_low_birthweight + 
    ##     health__pct_physical_inacticity + health__air_pollution_particulate_matter + 
    ##     health__motor_vehicle_crash_deaths_per_100k + health__pop_per_dentist + 
    ##     health__pop_per_primary_care_physician + demo__pct_nonwhite
    ## 
    ##                                                    Df Sum of Sq     RSS
    ## - economic_typology                                 5      6241 1539021
    ## - econ__pct_unemployment                            1       154 1532934
    ## - demo__pct_adults_with_some_college                1      1062 1533842
    ## <none>                                                          1532780
    ## - demo__pct_below_18_years_of_age                   1      2017 1534797
    ## - health__pct_adult_smoking                         1      2271 1535050
    ## - demo__pct_adults_less_than_a_high_school_diploma  1      2937 1535716
    ## - health__pop_per_primary_care_physician            1      3558 1536338
    ## - demo__pct_female                                  1      3752 1536531
    ## - demo__birth_rate_per_1k                           1      4230 1537009
    ## - health__pop_per_dentist                           1      4329 1537109
    ## - health__pct_diabetes                              1      6326 1539105
    ## - econ__pct_uninsured_children                      1      6641 1539421
    ## - health__motor_vehicle_crash_deaths_per_100k       1      6731 1539511
    ## - demo__pct_adults_with_high_school_diploma         1      8317 1541097
    ## - health__pct_adult_obesity                         1      8365 1541144
    ## - health__air_pollution_particulate_matter          1     12474 1545253
    ## - econ__pct_civilian_labor                          1     14604 1547383
    ## - econ__pct_uninsured_adults                        1     17806 1550585
    ## - demo__pct_nonwhite                                1     18801 1551581
    ## - urban_influence                                  11     51020 1583800
    ## - health__pct_low_birthweight                       1     35388 1568167
    ## - health__pct_physical_inacticity                   1     52276 1585056
    ## - demo__pct_aged_65_years_and_older                 1    136989 1669769
    ## - demo__death_rate_per_1k                           1    199949 1732729
    ##                                                      AIC
    ## - economic_typology                                11093
    ## - econ__pct_unemployment                           11095
    ## - demo__pct_adults_with_some_college               11096
    ## <none>                                             11097
    ## - demo__pct_below_18_years_of_age                  11097
    ## - health__pct_adult_smoking                        11097
    ## - demo__pct_adults_less_than_a_high_school_diploma 11098
    ## - health__pop_per_primary_care_physician           11099
    ## - demo__pct_female                                 11099
    ## - demo__birth_rate_per_1k                          11099
    ## - health__pop_per_dentist                          11099
    ## - health__pct_diabetes                             11102
    ## - econ__pct_uninsured_children                     11102
    ## - health__motor_vehicle_crash_deaths_per_100k      11102
    ## - demo__pct_adults_with_high_school_diploma        11104
    ## - health__pct_adult_obesity                        11104
    ## - health__air_pollution_particulate_matter         11108
    ## - econ__pct_civilian_labor                         11110
    ## - econ__pct_uninsured_adults                       11114
    ## - demo__pct_nonwhite                               11114
    ## - urban_influence                                  11128
    ## - health__pct_low_birthweight                      11132
    ## - health__pct_physical_inacticity                  11149
    ## - demo__pct_aged_65_years_and_older                11232
    ## - demo__death_rate_per_1k                          11292
    ## 
    ## Step:  AIC=11093.44
    ## heart_disease_mortality_per_100k ~ urban_influence + econ__pct_civilian_labor + 
    ##     econ__pct_unemployment + econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__pct_adults_with_some_college + demo__birth_rate_per_1k + 
    ##     demo__death_rate_per_1k + health__pct_adult_obesity + health__pct_adult_smoking + 
    ##     health__pct_diabetes + health__pct_low_birthweight + health__pct_physical_inacticity + 
    ##     health__air_pollution_particulate_matter + health__motor_vehicle_crash_deaths_per_100k + 
    ##     health__pop_per_dentist + health__pop_per_primary_care_physician + 
    ##     demo__pct_nonwhite
    ## 
    ##                                                    Df Sum of Sq     RSS
    ## - econ__pct_unemployment                            1       446 1539467
    ## - demo__pct_adults_with_some_college                1      1169 1540190
    ## <none>                                                          1539021
    ## - health__pct_adult_smoking                         1      2075 1541096
    ## - demo__pct_below_18_years_of_age                   1      2433 1541454
    ## - demo__pct_female                                  1      3288 1542309
    ## - demo__pct_adults_less_than_a_high_school_diploma  1      3425 1542446
    ## - health__pop_per_primary_care_physician            1      3842 1542863
    ## - health__pop_per_dentist                           1      4128 1543149
    ## - demo__birth_rate_per_1k                           1      4549 1543570
    ## - econ__pct_uninsured_children                      1      6334 1545355
    ## - health__pct_diabetes                              1      6402 1545423
    ## - health__motor_vehicle_crash_deaths_per_100k       1      7317 1546338
    ## - health__pct_adult_obesity                         1      8179 1547200
    ## - demo__pct_adults_with_high_school_diploma         1      9405 1548426
    ## - health__air_pollution_particulate_matter          1     13802 1552823
    ## - econ__pct_civilian_labor                          1     15465 1554486
    ## - econ__pct_uninsured_adults                        1     16447 1555468
    ## - demo__pct_nonwhite                                1     18341 1557362
    ## - urban_influence                                  11     49041 1588062
    ## - health__pct_low_birthweight                       1     37561 1576582
    ## - health__pct_physical_inacticity                   1     57175 1596196
    ## - demo__pct_aged_65_years_and_older                 1    173606 1712627
    ## - demo__death_rate_per_1k                           1    212097 1751118
    ##                                                      AIC
    ## - econ__pct_unemployment                           11092
    ## - demo__pct_adults_with_some_college               11093
    ## <none>                                             11093
    ## - health__pct_adult_smoking                        11094
    ## - demo__pct_below_18_years_of_age                  11094
    ## - demo__pct_female                                 11095
    ## - demo__pct_adults_less_than_a_high_school_diploma 11095
    ## - health__pop_per_primary_care_physician           11095
    ## - health__pop_per_dentist                          11096
    ## - demo__birth_rate_per_1k                          11096
    ## - econ__pct_uninsured_children                     11098
    ## - health__pct_diabetes                             11098
    ## - health__motor_vehicle_crash_deaths_per_100k      11099
    ## - health__pct_adult_obesity                        11100
    ## - demo__pct_adults_with_high_school_diploma        11101
    ## - health__air_pollution_particulate_matter         11106
    ## - econ__pct_civilian_labor                         11108
    ## - econ__pct_uninsured_adults                       11108
    ## - demo__pct_nonwhite                               11110
    ## - urban_influence                                  11122
    ## - health__pct_low_birthweight                      11130
    ## - health__pct_physical_inacticity                  11150
    ## - demo__pct_aged_65_years_and_older                11263
    ## - demo__death_rate_per_1k                          11299
    ## 
    ## Step:  AIC=11091.91
    ## heart_disease_mortality_per_100k ~ urban_influence + econ__pct_civilian_labor + 
    ##     econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__pct_adults_with_some_college + demo__birth_rate_per_1k + 
    ##     demo__death_rate_per_1k + health__pct_adult_obesity + health__pct_adult_smoking + 
    ##     health__pct_diabetes + health__pct_low_birthweight + health__pct_physical_inacticity + 
    ##     health__air_pollution_particulate_matter + health__motor_vehicle_crash_deaths_per_100k + 
    ##     health__pop_per_dentist + health__pop_per_primary_care_physician + 
    ##     demo__pct_nonwhite
    ## 
    ##                                                    Df Sum of Sq     RSS
    ## - demo__pct_adults_with_some_college                1      1341 1540808
    ## <none>                                                          1539467
    ## - health__pct_adult_smoking                         1      1999 1541466
    ## - demo__pct_below_18_years_of_age                   1      2322 1541789
    ## - demo__pct_female                                  1      2999 1542467
    ## - demo__pct_adults_less_than_a_high_school_diploma  1      3112 1542579
    ## - health__pop_per_primary_care_physician            1      3831 1543299
    ## - health__pop_per_dentist                           1      4186 1543653
    ## - demo__birth_rate_per_1k                           1      4901 1544369
    ## - econ__pct_uninsured_children                      1      6002 1545470
    ## - health__pct_diabetes                              1      6578 1546045
    ## - health__motor_vehicle_crash_deaths_per_100k       1      7067 1546534
    ## - health__pct_adult_obesity                         1      8202 1547670
    ## - demo__pct_adults_with_high_school_diploma         1      9017 1548485
    ## - health__air_pollution_particulate_matter          1     13970 1553437
    ## - econ__pct_civilian_labor                          1     16400 1555868
    ## - econ__pct_uninsured_adults                        1     16528 1555995
    ## - demo__pct_nonwhite                                1     21027 1560495
    ## - urban_influence                                  11     49222 1588689
    ## - health__pct_low_birthweight                       1     37723 1577191
    ## - health__pct_physical_inacticity                   1     59525 1598992
    ## - demo__pct_aged_65_years_and_older                 1    173200 1712667
    ## - demo__death_rate_per_1k                           1    211746 1751213
    ##                                                      AIC
    ## - demo__pct_adults_with_some_college               11091
    ## <none>                                             11092
    ## - health__pct_adult_smoking                        11092
    ## - demo__pct_below_18_years_of_age                  11092
    ## - demo__pct_female                                 11093
    ## - demo__pct_adults_less_than_a_high_school_diploma 11093
    ## - health__pop_per_primary_care_physician           11094
    ## - health__pop_per_dentist                          11094
    ## - demo__birth_rate_per_1k                          11095
    ## - econ__pct_uninsured_children                     11096
    ## - health__pct_diabetes                             11097
    ## - health__motor_vehicle_crash_deaths_per_100k      11097
    ## - health__pct_adult_obesity                        11098
    ## - demo__pct_adults_with_high_school_diploma        11099
    ## - health__air_pollution_particulate_matter         11104
    ## - econ__pct_civilian_labor                         11107
    ## - econ__pct_uninsured_adults                       11107
    ## - demo__pct_nonwhite                               11112
    ## - urban_influence                                  11120
    ## - health__pct_low_birthweight                      11129
    ## - health__pct_physical_inacticity                  11151
    ## - demo__pct_aged_65_years_and_older                11261
    ## - demo__death_rate_per_1k                          11297
    ## 
    ## Step:  AIC=11091.3
    ## heart_disease_mortality_per_100k ~ urban_influence + econ__pct_civilian_labor + 
    ##     econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__birth_rate_per_1k + demo__death_rate_per_1k + health__pct_adult_obesity + 
    ##     health__pct_adult_smoking + health__pct_diabetes + health__pct_low_birthweight + 
    ##     health__pct_physical_inacticity + health__air_pollution_particulate_matter + 
    ##     health__motor_vehicle_crash_deaths_per_100k + health__pop_per_dentist + 
    ##     health__pop_per_primary_care_physician + demo__pct_nonwhite
    ## 
    ##                                                    Df Sum of Sq     RSS
    ## - health__pct_adult_smoking                         1      1595 1542403
    ## <none>                                                          1540808
    ## - demo__pct_below_18_years_of_age                   1      3597 1544405
    ## - health__pop_per_dentist                           1      4053 1544861
    ## - demo__pct_female                                  1      4243 1545051
    ## - health__pop_per_primary_care_physician            1      4310 1545118
    ## - demo__birth_rate_per_1k                           1      4626 1545435
    ## - econ__pct_uninsured_children                      1      6425 1547233
    ## - health__motor_vehicle_crash_deaths_per_100k       1      6519 1547327
    ## - demo__pct_adults_less_than_a_high_school_diploma  1      6578 1547386
    ## - health__pct_diabetes                              1      7041 1547849
    ## - health__pct_adult_obesity                         1      7118 1547926
    ## - health__air_pollution_particulate_matter          1     13877 1554685
    ## - econ__pct_civilian_labor                          1     15623 1556431
    ## - demo__pct_adults_with_high_school_diploma         1     16592 1557400
    ## - econ__pct_uninsured_adults                        1     17085 1557893
    ## - demo__pct_nonwhite                                1     20814 1561622
    ## - urban_influence                                  11     51574 1592382
    ## - health__pct_low_birthweight                       1     38691 1579499
    ## - health__pct_physical_inacticity                   1     60861 1601669
    ## - demo__pct_aged_65_years_and_older                 1    186541 1727349
    ## - demo__death_rate_per_1k                           1    210749 1751558
    ##                                                      AIC
    ## - health__pct_adult_smoking                        11091
    ## <none>                                             11091
    ## - demo__pct_below_18_years_of_age                  11093
    ## - health__pop_per_dentist                          11094
    ## - demo__pct_female                                 11094
    ## - health__pop_per_primary_care_physician           11094
    ## - demo__birth_rate_per_1k                          11094
    ## - econ__pct_uninsured_children                     11096
    ## - health__motor_vehicle_crash_deaths_per_100k      11096
    ## - demo__pct_adults_less_than_a_high_school_diploma 11096
    ## - health__pct_diabetes                             11097
    ## - health__pct_adult_obesity                        11097
    ## - health__air_pollution_particulate_matter         11104
    ## - econ__pct_civilian_labor                         11106
    ## - demo__pct_adults_with_high_school_diploma        11106
    ## - econ__pct_uninsured_adults                       11107
    ## - demo__pct_nonwhite                               11111
    ## - urban_influence                                  11122
    ## - health__pct_low_birthweight                      11129
    ## - health__pct_physical_inacticity                  11152
    ## - demo__pct_aged_65_years_and_older                11273
    ## - demo__death_rate_per_1k                          11295
    ## 
    ## Step:  AIC=11090.96
    ## heart_disease_mortality_per_100k ~ urban_influence + econ__pct_civilian_labor + 
    ##     econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__birth_rate_per_1k + demo__death_rate_per_1k + health__pct_adult_obesity + 
    ##     health__pct_diabetes + health__pct_low_birthweight + health__pct_physical_inacticity + 
    ##     health__air_pollution_particulate_matter + health__motor_vehicle_crash_deaths_per_100k + 
    ##     health__pop_per_dentist + health__pop_per_primary_care_physician + 
    ##     demo__pct_nonwhite
    ## 
    ##                                                    Df Sum of Sq     RSS
    ## <none>                                                          1542403
    ## - demo__pct_female                                  1      3968 1546371
    ## - health__pop_per_primary_care_physician            1      4295 1546698
    ## - demo__pct_below_18_years_of_age                   1      4356 1546760
    ## - health__pop_per_dentist                           1      4596 1546999
    ## - demo__birth_rate_per_1k                           1      5375 1547778
    ## - econ__pct_uninsured_children                      1      6488 1548891
    ## - health__motor_vehicle_crash_deaths_per_100k       1      6679 1549082
    ## - demo__pct_adults_less_than_a_high_school_diploma  1      6715 1549119
    ## - health__pct_adult_obesity                         1      6960 1549364
    ## - health__pct_diabetes                              1      7479 1549882
    ## - health__air_pollution_particulate_matter          1     14070 1556473
    ## - econ__pct_civilian_labor                          1     17031 1559434
    ## - econ__pct_uninsured_adults                        1     17948 1560351
    ## - demo__pct_adults_with_high_school_diploma         1     18650 1561053
    ## - demo__pct_nonwhite                                1     24566 1566969
    ## - urban_influence                                  11     51502 1593905
    ## - health__pct_low_birthweight                       1     40243 1582646
    ## - health__pct_physical_inacticity                   1     63027 1605430
    ## - demo__pct_aged_65_years_and_older                 1    194715 1737118
    ## - demo__death_rate_per_1k                           1    222127 1764530
    ##                                                      AIC
    ## <none>                                             11091
    ## - demo__pct_female                                 11093
    ## - health__pop_per_primary_care_physician           11093
    ## - demo__pct_below_18_years_of_age                  11094
    ## - health__pop_per_dentist                          11094
    ## - demo__birth_rate_per_1k                          11095
    ## - econ__pct_uninsured_children                     11096
    ## - health__motor_vehicle_crash_deaths_per_100k      11096
    ## - demo__pct_adults_less_than_a_high_school_diploma 11096
    ## - health__pct_adult_obesity                        11096
    ## - health__pct_diabetes                             11097
    ## - health__air_pollution_particulate_matter         11104
    ## - econ__pct_civilian_labor                         11107
    ## - econ__pct_uninsured_adults                       11108
    ## - demo__pct_adults_with_high_school_diploma        11108
    ## - demo__pct_nonwhite                               11114
    ## - urban_influence                                  11122
    ## - health__pct_low_birthweight                      11130
    ## - health__pct_physical_inacticity                  11153
    ## - demo__pct_aged_65_years_and_older                11280
    ## - demo__death_rate_per_1k                          11305

``` r
summary(step.fit)
```

    ## 
    ## Call:
    ## lm(formula = heart_disease_mortality_per_100k ~ urban_influence + 
    ##     econ__pct_civilian_labor + econ__pct_uninsured_adults + econ__pct_uninsured_children + 
    ##     demo__pct_female + demo__pct_below_18_years_of_age + demo__pct_aged_65_years_and_older + 
    ##     demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma + 
    ##     demo__birth_rate_per_1k + demo__death_rate_per_1k + health__pct_adult_obesity + 
    ##     health__pct_diabetes + health__pct_low_birthweight + health__pct_physical_inacticity + 
    ##     health__air_pollution_particulate_matter + health__motor_vehicle_crash_deaths_per_100k + 
    ##     health__pop_per_dentist + health__pop_per_primary_care_physician + 
    ##     demo__pct_nonwhite, data = heart)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -118.543  -19.539   -2.053   18.128  143.192 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error
    ## (Intercept)                                       1.105e+02  2.794e+01
    ## urban_influence2                                 -1.098e+01  2.634e+00
    ## urban_influence3                                 -2.717e+00  4.152e+00
    ## urban_influence4                                  2.619e+00  4.436e+00
    ## urban_influence5                                 -1.374e+01  3.376e+00
    ## urban_influence6                                 -7.590e+00  3.634e+00
    ## urban_influence7                                 -8.725e+00  5.354e+00
    ## urban_influence8                                  9.298e-01  3.772e+00
    ## urban_influence9                                 -5.409e+00  4.286e+00
    ## urban_influence10                                -1.635e+00  5.780e+00
    ## urban_influence11                                -1.483e+01  4.910e+00
    ## urban_influence12                                -1.823e+01  6.021e+00
    ## econ__pct_civilian_labor                         -7.687e+01  1.843e+01
    ## econ__pct_uninsured_adults                        1.228e+02  2.869e+01
    ## econ__pct_uninsured_children                     -1.143e+02  4.440e+01
    ## demo__pct_female                                  1.129e+02  5.610e+01
    ## demo__pct_below_18_years_of_age                  -9.519e+01  4.513e+01
    ## demo__pct_aged_65_years_and_older                -6.202e+02  4.398e+01
    ## demo__pct_adults_less_than_a_high_school_diploma  5.952e+01  2.273e+01
    ## demo__pct_adults_with_high_school_diploma         7.977e+01  1.828e+01
    ## demo__birth_rate_per_1k                           1.290e+00  5.505e-01
    ## demo__death_rate_per_1k                           1.020e+01  6.774e-01
    ## health__pct_adult_obesity                         8.195e+01  3.074e+01
    ## health__pct_diabetes                              1.797e+02  6.502e+01
    ## health__pct_low_birthweight                       3.656e+02  5.704e+01
    ## health__pct_physical_inacticity                   2.228e+02  2.777e+01
    ## health__air_pollution_particulate_matter         -2.401e+00  6.334e-01
    ## health__motor_vehicle_crash_deaths_per_100k       3.403e-01  1.303e-01
    ## health__pop_per_dentist                           9.861e-04  4.552e-04
    ## health__pop_per_primary_care_physician           -1.073e-03  5.124e-04
    ## demo__pct_nonwhite                               -3.537e+01  7.062e+00
    ##                                                  t value Pr(>|t|)    
    ## (Intercept)                                        3.955 8.01e-05 ***
    ## urban_influence2                                  -4.166 3.26e-05 ***
    ## urban_influence3                                  -0.654 0.512928    
    ## urban_influence4                                   0.590 0.555053    
    ## urban_influence5                                  -4.070 4.93e-05 ***
    ## urban_influence6                                  -2.088 0.036933 *  
    ## urban_influence7                                  -1.630 0.103385    
    ## urban_influence8                                   0.247 0.805317    
    ## urban_influence9                                  -1.262 0.207122    
    ## urban_influence10                                 -0.283 0.777269    
    ## urban_influence11                                 -3.022 0.002556 ** 
    ## urban_influence12                                 -3.029 0.002498 ** 
    ## econ__pct_civilian_labor                          -4.170 3.21e-05 ***
    ## econ__pct_uninsured_adults                         4.281 1.97e-05 ***
    ## econ__pct_uninsured_children                      -2.574 0.010147 *  
    ## demo__pct_female                                   2.013 0.044285 *  
    ## demo__pct_below_18_years_of_age                   -2.109 0.035091 *  
    ## demo__pct_aged_65_years_and_older                -14.101  < 2e-16 ***
    ## demo__pct_adults_less_than_a_high_school_diploma   2.619 0.008913 ** 
    ## demo__pct_adults_with_high_school_diploma          4.364 1.36e-05 ***
    ## demo__birth_rate_per_1k                            2.343 0.019269 *  
    ## demo__death_rate_per_1k                           15.061  < 2e-16 ***
    ## health__pct_adult_obesity                          2.666 0.007755 ** 
    ## health__pct_diabetes                               2.763 0.005786 ** 
    ## health__pct_low_birthweight                        6.410 1.91e-10 ***
    ## health__pct_physical_inacticity                    8.022 2.01e-15 ***
    ## health__air_pollution_particulate_matter          -3.790 0.000156 ***
    ## health__motor_vehicle_crash_deaths_per_100k        2.612 0.009100 ** 
    ## health__pop_per_dentist                            2.166 0.030429 *  
    ## health__pop_per_primary_care_physician            -2.094 0.036395 *  
    ## demo__pct_nonwhite                                -5.009 6.10e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 31.29 on 1575 degrees of freedom
    ## Multiple R-squared:  0.7199, Adjusted R-squared:  0.7145 
    ## F-statistic: 134.9 on 30 and 1575 DF,  p-value: < 2.2e-16

``` r
# predict on test data
pred.step = predict.lm(step.fit, newdata = test)
mean((pred.step - test$heart_disease_mortality_per_100k)^2) # MSE
```

    ## [1] 1011.854

GAM

``` r
# GAM using predictors selected by the stepwise selection method
gam.fit = mgcv::gam(heart_disease_mortality_per_100k ~ urban_influence + s(econ__pct_civilian_labor) +
                      s(econ__pct_uninsured_adults) + s(econ__pct_uninsured_children) +
                      s(demo__pct_female) + (demo__pct_below_18_years_of_age) +
                      s(demo__pct_aged_65_years_and_older) + s(demo__pct_adults_less_than_a_high_school_diploma) +
                      s(demo__pct_adults_with_high_school_diploma) + s(demo__birth_rate_per_1k) +
                      s(demo__death_rate_per_1k) + s(health__pct_adult_obesity) +
                      s(health__pct_diabetes) + s(health__pct_low_birthweight) +
                      s(health__pct_physical_inacticity) + health__air_pollution_particulate_matter +
                      s(health__motor_vehicle_crash_deaths_per_100k) + s(health__pop_per_primary_care_physician) +
                      s(demo__pct_nonwhite),
                    data = heart)

# gam.fit = mgcv::gam(heart_disease_mortality_per_100k ~ metro_ruccs + population_ruccs + urban_influence + economic_typology + s(econ__pct_civilian_labor) + s(econ__pct_unemployment) + s(econ__pct_uninsured_adults) + s(econ__pct_uninsured_children) + s(demo__pct_female) + s(demo__pct_below_18_years_of_age) + s(demo__pct_aged_65_years_and_older) + s(demo__pct_adults_less_than_a_high_school_diploma) + s(demo__pct_adults_with_high_school_diploma) + s(demo__pct_adults_with_some_college) + s(demo__pct_adults_bachelors_or_higher) + s(demo__birth_rate_per_1k) + s(demo__death_rate_per_1k) + health__air_pollution_particulate_matter + s(health__pct_adult_obesity) + s(health__pct_diabetes) + s(health__pct_adult_smoking) + s(health__pct_low_birthweight) + s(health__pct_physical_inacticity) + s(health__motor_vehicle_crash_deaths_per_100k) + s(health__pop_per_dentist) + s(health__pop_per_primary_care_physician),
#                     data = heart)

summary(gam.fit)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## heart_disease_mortality_per_100k ~ urban_influence + s(econ__pct_civilian_labor) + 
    ##     s(econ__pct_uninsured_adults) + s(econ__pct_uninsured_children) + 
    ##     s(demo__pct_female) + (demo__pct_below_18_years_of_age) + 
    ##     s(demo__pct_aged_65_years_and_older) + s(demo__pct_adults_less_than_a_high_school_diploma) + 
    ##     s(demo__pct_adults_with_high_school_diploma) + s(demo__birth_rate_per_1k) + 
    ##     s(demo__death_rate_per_1k) + s(health__pct_adult_obesity) + 
    ##     s(health__pct_diabetes) + s(health__pct_low_birthweight) + 
    ##     s(health__pct_physical_inacticity) + health__air_pollution_particulate_matter + 
    ##     s(health__motor_vehicle_crash_deaths_per_100k) + s(health__pop_per_primary_care_physician) + 
    ##     s(demo__pct_nonwhite)
    ## 
    ## Parametric coefficients:
    ##                                          Estimate Std. Error t value
    ## (Intercept)                              334.5781    13.2344  25.281
    ## urban_influence2                         -10.3196     2.6010  -3.967
    ## urban_influence3                          -5.0767     4.0839  -1.243
    ## urban_influence4                           3.7196     4.3501   0.855
    ## urban_influence5                         -11.6754     3.3263  -3.510
    ## urban_influence6                          -3.2601     3.6425  -0.895
    ## urban_influence7                          -4.4634     5.2326  -0.853
    ## urban_influence8                           2.9034     3.8188   0.760
    ## urban_influence9                          -0.0426     4.2306  -0.010
    ## urban_influence10                          2.8599     5.8671   0.487
    ## urban_influence11                         -8.3315     4.8962  -1.702
    ## urban_influence12                        -12.3344     5.9827  -2.062
    ## demo__pct_below_18_years_of_age          -97.6410    46.0424  -2.121
    ## health__air_pollution_particulate_matter  -1.9176     0.6267  -3.060
    ##                                          Pr(>|t|)    
    ## (Intercept)                               < 2e-16 ***
    ## urban_influence2                          7.6e-05 ***
    ## urban_influence3                         0.214026    
    ## urban_influence4                         0.392657    
    ## urban_influence5                         0.000461 ***
    ## urban_influence6                         0.370906    
    ## urban_influence7                         0.393799    
    ## urban_influence8                         0.447209    
    ## urban_influence9                         0.991967    
    ## urban_influence10                        0.626011    
    ## urban_influence11                        0.089031 .  
    ## urban_influence12                        0.039407 *  
    ## demo__pct_below_18_years_of_age          0.034110 *  
    ## health__air_pollution_particulate_matter 0.002252 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                                       edf Ref.df       F
    ## s(econ__pct_civilian_labor)                         8.087  8.772   4.495
    ## s(econ__pct_uninsured_adults)                       5.998  7.182   3.584
    ## s(econ__pct_uninsured_children)                     3.087  3.931   5.153
    ## s(demo__pct_female)                                 1.000  1.000   6.180
    ## s(demo__pct_aged_65_years_and_older)                3.025  3.876  49.360
    ## s(demo__pct_adults_less_than_a_high_school_diploma) 7.182  8.222   2.378
    ## s(demo__pct_adults_with_high_school_diploma)        2.990  3.811   8.176
    ## s(demo__birth_rate_per_1k)                          3.614  4.505   4.287
    ## s(demo__death_rate_per_1k)                          1.000  1.000 227.867
    ## s(health__pct_adult_obesity)                        5.769  6.924   2.377
    ## s(health__pct_diabetes)                             1.698  2.175   3.186
    ## s(health__pct_low_birthweight)                      5.224  6.337   7.154
    ## s(health__pct_physical_inacticity)                  2.236  2.886  20.674
    ## s(health__motor_vehicle_crash_deaths_per_100k)      1.000  1.000   6.545
    ## s(health__pop_per_primary_care_physician)           8.407  8.895   3.180
    ## s(demo__pct_nonwhite)                               3.975  4.931  10.848
    ##                                                      p-value    
    ## s(econ__pct_civilian_labor)                         2.09e-05 ***
    ## s(econ__pct_uninsured_adults)                       0.000696 ***
    ## s(econ__pct_uninsured_children)                     0.000540 ***
    ## s(demo__pct_female)                                 0.013028 *  
    ## s(demo__pct_aged_65_years_and_older)                 < 2e-16 ***
    ## s(demo__pct_adults_less_than_a_high_school_diploma) 0.012291 *  
    ## s(demo__pct_adults_with_high_school_diploma)        3.47e-06 ***
    ## s(demo__birth_rate_per_1k)                          0.001147 ** 
    ## s(demo__death_rate_per_1k)                           < 2e-16 ***
    ## s(health__pct_adult_obesity)                        0.026488 *  
    ## s(health__pct_diabetes)                             0.035680 *  
    ## s(health__pct_low_birthweight)                      1.09e-07 ***
    ## s(health__pct_physical_inacticity)                  1.45e-12 ***
    ## s(health__motor_vehicle_crash_deaths_per_100k)      0.010610 *  
    ## s(health__pop_per_primary_care_physician)           0.001827 ** 
    ## s(demo__pct_nonwhite)                               4.33e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.744   Deviance explained = 75.7%
    ## GCV = 921.93  Scale est. = 876.99    n = 1606

``` r
# plot(gam.fit)

## Predict on test data

pred.gam = mgcv::predict.gam(gam.fit, newdata = test)
mean((pred.gam - test$heart_disease_mortality_per_100k)^2) # MSE
```

    ## [1] 964.0688
