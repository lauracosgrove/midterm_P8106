Project Proposal
================
Laura Cosgrove
3/7/2019

Project Title
-------------

Predicting Heart Disease in U.S. Counties

Data Sources (Charlotte)
------------------------

Description of Data (Alyssa)
----------------------------

    ## Skim summary statistics
    ##  n obs: 3080 
    ##  n variables: 11 
    ## 
    ## ── Variable type:integer ────────────────────────────────────────────────────────────────────────────────────────────────
    ##                          variable missing complete    n    mean      sd
    ##  air_pollution_particulate_matter      38     3042 3080   11.63    1.55
    ##                   pop_per_dentist     198     2882 3080 3308.51 2605.62
    ##    pop_per_primary_care_physician     184     2896 3080 2579.7  2171.18
    ##   p0  p25  p50     p75  p100     hist
    ##    7   10   12   13       15 ▁▂▅▅▇▇▂▁
    ##  269 1829 2589 3826.75 27249 ▇▂▁▁▁▁▁▁
    ##  340 1349 1910 2889.25 20939 ▇▂▁▁▁▁▁▁
    ## 
    ## ── Variable type:numeric ────────────────────────────────────────────────────────────────────────────────────────────────
    ##                             variable missing complete    n   mean     sd
    ##                   homicides_per_100k    1830     1250 3080  5.62   4.36 
    ##  motor_vehicle_crash_deaths_per_100k     345     2735 3080 20.44   9.99 
    ##                    pct_adult_obesity       8     3072 3080  0.3    0.043
    ##                    pct_adult_smoking     402     2678 3080  0.21   0.063
    ##                         pct_diabetes       8     3072 3080  0.11   0.024
    ##               pct_excessive_drinking     860     2220 3080  0.17   0.053
    ##                  pct_low_birthweight     147     2933 3080  0.082  0.021
    ##              pct_physical_inacticity       8     3072 3080  0.27   0.054
    ##      p0    p25    p50    p75  p100     hist
    ##  -0.1    2.7    4.5    7.27  30.4  ▇▇▂▁▁▁▁▁
    ##   2.99  13.32  18.46  26.02  96.63 ▆▇▃▁▁▁▁▁
    ##   0.12   0.28   0.31   0.33   0.48 ▁▁▂▇▇▂▁▁
    ##   0.031  0.17   0.2    0.25   0.5  ▁▃▇▆▃▁▁▁
    ##   0.035  0.091  0.11   0.12   0.21 ▁▂▇▇▅▁▁▁
    ##   0.026  0.13   0.16   0.2    0.56 ▂▇▇▂▁▁▁▁
    ##   0.025  0.067  0.079  0.093  0.18 ▁▃▇▅▂▁▁▁
    ##   0.089  0.24   0.28   0.31   0.45 ▁▁▃▆▇▅▁▁

There are variables in four general categories:

1.  Area

2.  Health

3.  Demography

4.  Economic

Of course, each category is related to one another, but we can split them up for the purposes of description.

Planned Analyses (Laura)
------------------------
