#' Function to clean old HIS excel-based OPD linelist data (.opd) and have in a
#' usable format.
#' Not a user-facing function (called within read_msf_data())
#'
#' @param country_file Path to a single IPD file (.opd)
#'
#' @importFrom stringr str_remove str_split str_remove_all str_sub str_detect str_c
#' @importFrom tsibble yearweek
#' @importFrom epitrix clean_labels
#' @importFrom dplyr bind_rows
#'
#' @author Alex Spina

clean_opd <- function(country_file) {

  ## read in the OPD file
  check <- read.delim(country_file, stringsAsFactors = FALSE)

  ## pull together identifiers
  version_num <- str_remove(names(check), "Clinic.details.")

  identifiers <- str_split(check[1, 1], ",")

  ## remove hashtags to extract the first day of year
  first_day <- str_remove_all(identifiers[[1]][1], "#")
  ## turn in to a date
  first_day <- as.Date(first_day)

  ## use week to get the appropriate year
  year <- tsibble::yearweek(first_day)
  ## extract only the year from epiweek above
  year <- str_sub(year, 1, 4)

  ## get country (and region sometimes)
  country <- identifiers[[1]][2]

  ## get project name
  project <- identifiers[[1]][3]

  ## get the site
  site <- identifiers[[1]][4]

  ## create a TRUE/FALSE for subdividing tables
  # check$separator <- stringr::str_detect(check[,1], "\\[")
  check$separator <- str_sub(check[,1], 1, 1) == "["

  ## get the names of sheets
  sheets <- check[,1][which(check$separator)]

  ## remove brackets from names
  sheets <- str_remove_all(sheets, "\\[|\\]")

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


  ## pull together the row labels for weeks
  row_labs <- c("consultants",
                "Population",
                "Acute watery diarrhoea",
                "Severe acute diarrhoea/cholera",
                "Acute bloody diarrhoea",
                "Fever without identified cause",
                "Confirmed malaria",
                "Typhoid fever",
                "Acute upper respiratory tract infection",
                "Acute lower respiratory tract infection",
                "Tuberculosis (suspected)",
                "Meningitis (suspected)",
                "Neonatal tetanus (suspected)",
                "Measles (suspected)",
                "Acute flaccid paralysis",
                "Sexually transmitted infection (male)",
                "Sexually transmitted infection (female)",
                "Urinary tract infection",
                "Eye infection",
                "Gynaecological disease",
                "Anaemia",
                "Severe acute malnutrition",
                "Chronic disease",
                "Common Psychiatric Disorders",
                "Severe Psychiatric Disorders",
                "Violence-related injury",
                str_remove(sep_tabs$opd_options[14:23], ",show|,hide"),
                "Others",
                "Malaria and malnutrition",
                "Malaria and lower respiratory tract infection",
                "Malaria and diarrhoea/dysentry",
                "Number of injections",
                "Number of dressings New case",
                "Number of dressings Follow-up",
                "MSF Hospital",
                str_remove(sep_tabs$opd_options[25:29], ",show|,hide"),
                "Sexual violences",
                "Deaths in OPD",
                "Number of malaria tests done",
                "Number of malaria positive tests",
                str_remove(sep_tabs$opd_options[89:92], ",show|,hide"),
                "Other follow-up"
  )

  ## pull together the variable groups for rowlabels
  var_types <- c("consultants",
                 "Population",
                 rep.int("Diseases", 35),
                 rep.int("Malaria concurrent", 3),
                 rep.int("Dressings/Injections", 3),
                 rep.int("References", 6),
                 "Sexual violence",
                 "Deaths",
                 rep.int("Malaria tests", 2),
                 rep.int("Follow-up", 5)
  )


  ## create an empty list to merge later
  week_counts <- list()

  ## for each of the weeks present
  for (i in sheets[str_detect(sheets, "week_")]) {

    ## seperate add two commas to the first row (to match rest)
    sep_tabs[[i]][1] <- str_c(sep_tabs[[i]][1], ",,")

    ## seperate in to columns based on commas (return as a matrix)
    intermediate <- str_split(sep_tabs[[i]], ",", simplify = TRUE)

    ## change to a dataframe
    intermediate <- data.frame(intermediate)

    ## add column names (under five and greater-equal to five)
    colnames(intermediate) <- c("pop1_u5", "pop1_o5", "pop2_u5", "pop2_o5")

    ## add a column for epiweek
    intermediate$week <- str_remove(i, "week_")

    ## add disease name
    intermediate$disease <- row_labs

    ## add count type
    intermediate$count_type <- var_types

    ## overwrite the original
    week_counts[[i]] <- intermediate
  }

  ## bind all the count tables together
  outer <- bind_rows(week_counts)

  ## add in year variable
  outer$year <- year

  ## add in country variable
  outer$country <- country

  ## add in project variable
  outer$project <- project

  ## add in site variable
  outer$site <- site

  ## add in version of opd tool used
  outer$version_num <- version_num

  ## change counts to numeric
  outer$pop1_u5 <- as.numeric(as.character(outer$pop1_u5))
  outer$pop1_o5 <- as.numeric(as.character(outer$pop1_o5))
  outer$pop2_u5 <- as.numeric(as.character(outer$pop2_u5))
  outer$pop2_o5 <- as.numeric(as.character(outer$pop2_o5))
  outer$week <- as.numeric(as.character(outer$week))

  ## add total number
  outer$total <- rowSums(outer[, c("pop1_u5", "pop1_o5", "pop2_u5", "pop2_o5")],
                         na.rm = TRUE)

  ## if all rows NA then set total to NA
  outer$total[which(
    rowSums(
      is.na(outer[, c("pop1_u5", "pop1_o5", "pop2_u5", "pop2_o5")])
    ) == 4)] <- NA


  ## change order of columns
  outer <- outer[ , c("country", "project", "site",
                      "year", "week", "disease",
                      "pop1_u5", "pop1_o5", "pop2_u5", "pop2_o5",
                      "total", "version_num", "count_type")]

  ## chuck in the source file at end too
  # outer$source_file <- country_file

  ## return dataframe as tibble
  data.frame(outer)
}
