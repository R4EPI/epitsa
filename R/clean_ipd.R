#' Function to clean old HIS excel-based IPD linelist data (.med) and have in a
#' usable format.
#' Not a user-facing function (called within read_msf_data())
#'
#' @param country_file Path to a single IPD file (.med)
#'
#' @importFrom stringr str_split str_sub str_remove_all str_which str_c str_glue str_split_fixed
#' @importFrom epitrix clean_labels
#' @importFrom matchmaker match_df
#'
#' @author Alex Spina


clean_ipd <- function(country_file) {

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


  ## pull together the recodes (to also include the options)

  ## get starts and ends of ward definitions
  ward_start <- min(
    ## find which row has ward header
    str_which(
      ## swap out french characters
      iconv(sep_tabs$options_and_definitions,
            from = "UTF-8", to = "ASCII//TRANSLIT"),
      ## search ward or salle version
      "Ward admitted to - OPTIONAL|Salle d'hopital admis a - OPTIONNEL")
    ## add one to get the first ward
  ) + 1

  ward_stop <- min(str_which(sep_tabs$options_and_definitions,
                             "True\t|False\t")) - 1

  ## diagnosis always seems to start at 12
  diagnosis_start <- 12
  ## get end of diagnosis definitions
  diagnosis_stop <- as.numeric(ward_start) - 7
  ## pull the defined diagnoses
  defined_diagnosis <- sep_tabs$options_and_definitions[diagnosis_start:diagnosis_stop]
  ## get the number of diagnoses defined
  num_diagnosis <- length(defined_diagnosis)

  ## pull together character string of diagnoses
  ## add on options if missing
  if (num_diagnosis < 10) {
    ## for those that have less than 10 diagnoses defined
    ## add the appropriate number of options to the end
    diagnosis_codes <- c(str_c(defined_diagnosis),
                         paste0("Option ", (num_diagnosis + 1):10))
  } else {
    ## otherwise just take the defined diagnoses
    diagnosis_codes <- str_c(defined_diagnosis)
  }




  ## get starts and ends of village defs
  village_start <- min(str_which(sep_tabs$options_and_definitions, "True\t|False\t"))
  village_stop  <- max(str_which(sep_tabs$options_and_definitions, "True\t|False\t"))


  ## table of definitions
  recode_defs <- data.frame(
    ## grouping variable to define variable of interest
    grp = c(rep.int("primary_diagnosis_code", 53),
            rep.int("location_admitted_from", 2),
            rep.int("ward_admitted_to", 10),
            rep.int("exit_code", 4),
            rep.int("time_between_admission_and_death", 3),
            rep.int("referred_to", 2),
            rep.int("village", 30)
    ),
    ## variable to define old coding
    old = c(
      ## numbers for diagnosis codes
      1:28, 30, str_glue("17.{c(1:9, 11:19, 21:26)}"),
      ## numbers for locations admitted from
      1:2,
      ## numbers for wards
      1:10,
      ## numbers for exits
      1:4,
      ## numbers for time to death
      1:3,
      ## numbers for referred to other structures
      1:2,
      ## numbers for villages
      1:30
    ),
    ## variable to define new coding
    new = c(
      ## standard diagnosis codes
      "Severe diarrhoea/Cholera",
      "Bloody diarrhoea",
      "Lower respiratory tract infections",
      "Tuberculosis",
      "Severe malaria (confirmed)",
      "Fever of unknown origin",
      "Severe malnutrition",
      "Anaemia",
      "Meningitis",
      "Neonatal tetanus (suspected)",
      "Measles",
      "Acute flacid paralysis",
      "AIDS related illness",
      "Jaundice",
      "Violence-related injury",
      "Chronic diseases",
      "Neonatal disease",
      "Suspected typhoid",

      ## diagnosis codes options
      diagnosis_codes,

      "Other",

      ## neonatal diagnosis subcodes
      "Prematurity/Dysmaturity",
      "Surfactant Deficiency",
      "Meconium Aspiration Syndrome",
      "Hypoglycemia",
      "Transient Tachypnea of the Newborn",
      "Hypoxic/Ischemic Encephalopathy",
      "Manifest Neonatal Infection",
      "Suspected Maternal-Fetal Infection",
      "Jaundice",
      "Prevention Mother To Child Trasmission (PMTCT)",
      "Ingestion of Maternal Blood/Amniotic Fluid",
      "Congenital malformation",
      "Macrosomia",
      "Obstetric trauma",
      "Hypothermia",
      "Infection >48 h after birth",
      "Anaemia (need of transfusion)",
      "Well baby",
      "Other (neonatal)",
      "Neonatal Option 1",
      "Neonatal Option 2",
      "Neonatal Option 3",
      "Neonatal Option 4",
      "Neonatal Option 5",
      ## addmitted from
      "Admissions from MSF structure",
      "Other admissions",
      ## Wards
      if (length(ward_start:ward_stop) < 10) {
        ## for those with less than ten wards defined
        ## pull those defined and then paste together the remaind options
        c(sep_tabs$options_and_definitions[ward_start:ward_stop],
          paste0("Ward", (length(ward_start:ward_stop) + 1):10)
        )
      } else {
        ## otherwise just pull the neccesaries
        str_c(sep_tabs$options_and_definitions[ward_start:ward_stop])
      } ,
      ## exits
      "Discharged",
      "Defaulters",
      "Referred to other structures",
      "Deaths",
      ## time between admission and death
      "<24 hours",
      ">=24 to <48 hours",
      ">=48 hours",
      ## referred to other structures
      "MSF Hospital",
      "Other",
      ## villages
      if (length(village_start:village_stop) < 30) {
        c(str_remove_all(
          sep_tabs$options_and_definitions[village_start:village_stop],
          "True\t|False\t"),
          paste0("Village", (length(village_start:village_stop) + 1):30)
        )
      } else {
        str_remove_all(
          sep_tabs$options_and_definitions[village_start:village_stop],
          "True\t|False\t")
      }
    )
  )


  ## pull the medical data
  meddata <- sep_tabs$meddata

  ## split in to columns by tab
  meddata <- str_split_fixed(meddata, pattern = "\t", 13)

  ## change to dataframe
  meddata <- data.frame(meddata)


  ## rename the columns
  colnames(meddata) <- c("pat_num",
                         "age",
                         "sex",
                         "village",
                         "date_of_admission",
                         "location_admitted_from",
                         "ward_admitted_to",
                         "primary_diagnosis_code",
                         "details_of_primary_diagnosis",
                         "date_of_exit",
                         "exit_code",
                         "time_between_admission_and_death",
                         "referred_to"
  )

  ## drop empty rows
  meddata <- meddata[which(meddata$pat_num != "" &
                             meddata$date_of_admission != ""), ]


  ## add in the identifiers as variables
  meddata$country <- country
  meddata$project <- project
  meddata$report_year <- year

  ## fix dates
  meddata$date_of_admission <- as.Date(
    as.numeric(as.character(meddata$date_of_admission)),
    origin = "1899-12-30")

  meddata$date_of_exit <- as.Date(
    as.numeric(as.character(meddata$date_of_exit)),
    origin = "1899-12-30")

  ## recode numeric to text for variables defined in recode_defs above
  meddata <- matchmaker::match_df(meddata,
                                  dictionary = recode_defs,
                                  from = "old",
                                  to = "new",
                                  by = "grp")


  ## add in that this data set came from HIS
  meddata$source <- "HIS"

  ## return dataset
  data.frame(meddata)
}
