#' Function to pull IPD weekly bed counts from DHIS output (sourced from pivot)
#' Not a user-facing function (called within read_msf_data())
#'
#' @param country_file Path to a single IPD file (.csv exported from DHIS2)
#'
#' @param wards Specify which wards want to keep
#' Default is "Medical" (strict definition without ITFC or maternal)
#' "Medical extension" includes all medical and ITFC + maternal
#' "All" also includes surgical
#'
#' @importFrom rio import
#' @importFrom epitrix clean_labels
#' @importFrom tsibble yearweek
#' @importFrom stringr str_which
#' @importFrom dplyr select
#'
#' @author Alex Spina



dhis_ipd_beds <- function(country_file, wards) {


  ## read in the IPD file
  check <- rio::import(country_file, na = c(""))

  ## pull the project from names rows
  project <- names(check)[1]

  ## fix up the names
  names(check) <- epitrix::clean_labels(check[2,])

  ## drop first two rows
  check <- check[c(-1, -2), ]

  ## fix the epi week
  check$periodname <- tsibble::yearweek(check$periodname)

  ## only keep bed count variables
  check <- check[ , str_which(names(check), "periodname|bed_count")]

  if (wards == "Medical extension") {
    check <- check[ , -str_which(names(check), "surgery")]
  }

  if (wards == "Medical") {
    check <- check[ , -str_which(names(check), "surgery|itfc_|maternity")]
  }

  ## change to numeric
  check[ , which(names(check) != "periodname")] <- sapply(check[ , which(names(check) != "periodname")],
                                                          FUN = as.numeric)

  ## get total counts
  check$beds <- rowSums(check[ , which(names(check) != "periodname")], na.rm = TRUE)

  ## set zero counts to NA
  check$beds[check$beds == 0] <- NA

  ## add in project variable
  check$project <- project

  ## add in country (empty cus dont have it from the download)
  check$country <- NA

  ## change week to date of the first day in that week
  dates <- as.Date(check$periodname)

  ## change date to month
  check$month <- format(dates, format = "%B")

  ## create year
  check$report_year <- format(dates, format = "%Y")

  ## drop empties
  check <- check[which(!is.na(check$beds)), ]


  ## return appropriate cols
  dplyr::select(check,
                country,
                project,
                report_year,
                week = periodname,
                month,
                beds
  )

}
