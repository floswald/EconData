EconData
========

R package containing a host of datasets useful for economic research. Complete with raw data and cleaning functions. 

You can rebuild all datasets with the function `makeAllData()`. Type `data(package="EconData")` for a list of datasets:

Dataset Name |  Description
-------------| -------------
CPIAUCSL                           |   CPI all urban consumers seasonally adjusted
CPIHOSSL                           |   CPI all urban consumers (Housing)
FHFA.msa50 (FHFA_msa50)            |   FHFA house price index for the 50 largest MSAs
FHFA.states (FHFA_states)          |   FHFA house price index by state
HomeValues                         |   Home, Structure and Land Values by state over time
State_distMat                      |   Haversine Distance Matrix between center of US states
State_distMat_agg                  |   Haversine Distance Matrix between center of US states (some states aggregated)
State_distTable                    |   Haversine Distance Table between centers of US states
State_distTable_agg                |   Haversine Distance Table between centers of US states (some states aggregated)
US_states                          |   Correspondance of US State with FIPS code, Census Region and Division
coordStates                        |   Coordinates of US state centers
coordStates_agg                    |   Coordinates of US state centers (some states aggregated)
medinc.current (US_medinc)         |   median income by state over time in current dollars
medinc.in2012 (US_medinc)          |   median income by state over time in 2012 dollars  

This is R package version of the repository at 
[https://github.com/floswald/Rdata](https://github.com/floswald/Rdata), which I will discontinue in due course.

