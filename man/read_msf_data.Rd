% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_msf_data.R
\name{read_msf_data}
\alias{read_msf_data}
\title{Function to read, clean and merge data sets from old HIS excel tools and DHIS2}
\usage{
read_msf_data(
  country_folder,
  site,
  data_source = "Both",
  reformat = TRUE,
  chronic_defs = TRUE,
  wards = "Medical"
)
}
\arguments{
\item{country_folder}{Path to folder of interest (usually containing all files
for a specific country/project)}

\item{site}{Which datasets to read options include "IPD", "OPD" or "IPD Beds"}

\item{data_source}{Which data source to use options include "HIS", "DHIS", or
"Both"(default)}

\item{reformat}{Logical (TRUE/FALSE) to specify if want to reformat data to
fit to DHIS2 exports. FALSE returns original dataset.
Must be TRUE if source == "Both"}

\item{chronic_defs}{Logical (TRUE/FALSE) to define if want to stick to the HIS
team definition for chronic conditions (default is TRUE).
This will put all old HIS chronic conditions in to new DHIS2 "Other chronic conditions".
Otherwise (if set to FALSE) leave as "Chronic diseases" to make available for
individually manipulating to combine with DHIS2 data.}

\item{wards}{Specify which wards want to keep
Default is "Medical" (strict definition without ITFC or maternal)
"Medical extension" includes all medical and ITFC + maternal
"All" also includes surgical. Only used if site == "IPD Beds".}
}
\description{
Function to read, clean and merge data sets from old HIS excel tools and DHIS2
}
\author{
Alex Spina
}
