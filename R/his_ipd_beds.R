#' Function to pull IPD weekly bed counts from the MMR tab of old HIS excel-based
#' tools (.med files)
#' Not a user-facing function (called within read_msf_data())
#'
#' @param country_file Path to a single IPD file (.med)
#'
#' @importFrom stringr str_split str_sub str_remove_all str_c
#' @importFrom dplyr if_else
#' @importFrom epitrix clean_labels
#' @importFrom tsibble yearweek
#'
#' @author Alex Spina

his_ipd_beds <- function(country_file) {

  ############# THIS CODE IS DUPLICATED IN OLD_HIS_DATA_TOOLS


  ## read in the IPD file
  check <- read.delim(country_file, stringsAsFactors = FALSE,
                      header = FALSE, encoding = "UTF-8",
                      sep = "|")

  ## pull together identifiers
  identifiers <- str_split(check[3, 1], pattern = "\t")

  version_num <- identifiers[[1]][1]

  ## extract the first day of year (excel date origin)
  first_day <- as.Date(as.numeric(identifiers[[1]][4]), origin = "1899-12-30")

  ## extract the appropriate year depending on which month first day is in
  year <- if_else(as.numeric(format(first_day, format = "%m")) != 1,
                  as.numeric(format(first_day, format = "%Y")) + 1,
                  as.numeric(format(first_day, format = "%Y")))

  ## get country (and region sometimes)
  country <- identifiers[[1]][2]

  ## get project name
  project <- identifiers[[1]][3]

  ## create a TRUE/FALSE for subdividing tables
  # check$separator <- stringr::str_detect(check[,1], "\\[")
  check$separator <- str_sub(check[,1], 1, 1) == "["

  ## get the names of sheets
  sheets <- check[,1][which(check$separator)]

  ## remove brackets from names and "CodeStop 4 "
  sheets <- str_remove_all(sheets, "\\[|\\]|CodeStop 4 ")

  ## standardise names
  sheets <- epitrix::clean_labels(sheets)

  ## get row numbers where new tabs start
  spliters <- which(check$separator)

  ## pull each tab in to a seperate dataframe within a list
  sep_tabs <- list()

  for (i in 1:length(spliters)) {

    ## if it is the last one then take until the last row
    if (i == length(spliters)) {
      sep_tabs[[sheets[i]]] <- check[(spliters[i] + 1):nrow(check), 1]
      ## otherwise take until the row before next tabs name
    } else {
      sep_tabs[[sheets[i]]] <- check[(spliters[i] + 1):(spliters[(i + 1)] - 1), 1]
    }
  }

  ############# END OF DUPLICATION


  ## define the begining of the year
  start_week <- tsibble::yearweek(first_day)

  ## define the end of the year
  end_day <- as.Date(str_c(year, "-12-31"))
  end_week <- tsibble::yearweek(end_day)

  ## pull together all weeks in the year
  initial <- data.frame(
    country = country,
    project = project,
    report_year = year,
    week = seq(start_week, end_week, by = 1)
  )

  ## change week to a yearweek
  initial$week <- tsibble::yearweek(initial$week)

  ## change week to date of the first day in that week
  initial$month <- as.Date(initial$week)

  ## change date to month
  initial$month <- format(initial$month, format = "%B")

  ## identify weeks outside of reporting year
  outliers <- which(format(as.Date(initial$week), format = "%Y") !=
                      year)

  ## drop first week if in previous year
  if (length(outliers) >= 1) {
    initial <- initial[-outliers, ]
  }

  ## change report year to character
  initial$report_year <- as.character(initial$report_year)

  ## pull the bed counts from mmr_beds tab and label with month name
  mmr_beds <- data.frame(
    ## month names are hardcoded in base R
    month  = base::month.name,
    ## separate out by tabs and unlist
    beds = str_split(sep_tabs$mmr_beds, pattern = "\t")[[1]]
  )

  ## set blanks to NA
  mmr_beds$beds[mmr_beds$beds == ""] <- NA

  ## make counts numeric
  mmr_beds$beds <- as.numeric(mmr_beds$beds)

  ## join weeks and counts by month
  checkout <- left_join(initial, mmr_beds, by = "month")


  ## drop empties
  checkout <- checkout[which(!is.na(checkout$beds)), ]

  ## return
  checkout
}
