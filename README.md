
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epitsa

<!-- badges: start -->
<!-- badges: end -->

The {epitsa} package aims to simplify time series analysis and
forecasting using MSF OCA routine health facility data.

There are functions for merging old HIS excel data and new DHIS2 data
(currently only for IPD, but there is capacity to add in OPD, maternal &
child health, etc.). The functions include:

-   `clean_ipd()` — Clean IPD linelist data and have in a usable format
    (backend)
-   `clean_opd()` — Clean OPD counts data and have in a usable format
    (backend)
-   `his_ipd_beds()` — Pull IPD weekly bed counts from the MMR tab
    (backend)
-   `dhis_ipd_beds()` — Pull IPD weekly bed counts from DHIS2 output
    (sourced from pivot) (backend)
-   `recode_ipd()` — Reformat old excel IPD tools to fit DHIS2 data
    exports (backend)
-   `read_msf_data()` — Pull together data sets for OPD, IPD or IPD bed
    counts for old HIS, DHIS2 or combining both (frontend)
-   `periodogram()` — Produce spectral periodogram or peak weeks

There are templates for producing reports looking at surveillance
(outbreak detection) and impact (segmented regression) - one template is
for producing a report using multiple health facilities, and the other
for just one. There is also a shorter template which looks exclusively
at malaria forecasting. In addition there is a template for downloading
climate data.

## Installation

You can install the in-development version from GitHub using the
{remotes} package (but there’s no guarantee that it will be stable):

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/epitsa") 
```
