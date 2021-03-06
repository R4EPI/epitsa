#' Function to recode old excel-based HIS IPD linelist data to DHIS2 format
#' Not a user-facing function (called within read_msf_data())
#'
#' @param x A dataset produced by [epitsa::clean_ipd]\cr function
#'
#' @param chronic_defs Logical (TRUE/FALSE) to define if want to stick to the HIS
#' team definition for chronic conditions (default is TRUE).
#' This will put all old HIS chronic conditions in to new DHIS2 "Other chronic conditions".
#' Otherwise (if set to FALSE) leave as "Chronic diseases" to make available for
#' individually manipulating to combine with DHIS2 data.
#'
#' @importFrom readxlsb read_xlsb
#' @importFrom dplyr select filter left_join arrange mutate recode
#' @importFrom epitrix clean_labels
#' @importFrom rio import
#' @importFrom matchmaker match_vec
#'
#' @author Alex Spina

recode_ipd <- function(x, chronic_defs = TRUE) {

  ## read in the data dictionary
  ipd_names_dict <- readxlsb::read_xlsb(path = system.file("extdata",
                                                           "Events_ImportFormat_IPDmed_Template20200403draft.xlsb",
                                                           package = "epitsa"),
                                        sheet = "IPD_DE_list")

  ## clean dictionary names
  names(ipd_names_dict) <- c("dhis2_names",
                             "uid",
                             "optionset_uid",
                             "optionset",
                             "notes1",
                             "excel_names",
                             "match_uid",
                             "notes2")

  ## separate the old and new names in to two different datasets

  ## pull the old excel names with corresponding dhis2 uids
  old_ipd_names_dict <- dplyr::select(ipd_names_dict,
                                      excel_names,
                                      match_uid)
  ## filter to have only non-missings
  old_ipd_names_dict <- dplyr::filter(old_ipd_names_dict,
                                      excel_names != "")

  ## make names in dictionary fit the (cleaned) old his dataset names
  old_ipd_names_dict$excel_names <- epitrix::clean_labels(
    old_ipd_names_dict$excel_names)

  old_ipd_names_dict$excel_names[
    old_ipd_names_dict$excel_names == "patient_na"
  ] <- "pat_num"

  old_ipd_names_dict$excel_names[
    old_ipd_names_dict$excel_names == "age_years"
  ] <- "age"


  ## read in the dictionary to get dhis shortnames with corresponding dhis2 uids
  dhis_shortnames <- rio::import(file = system.file("extdata",
                                             "cleaning_dict.xlsx",
                                             package = "epitsa"),
                                 sheet = "dhis_data_elements",
                                 col_types = c("text", "text", "text", "numeric"))

  ## merge for recode
  new_names_dict <- dplyr::left_join(dhis_shortnames, old_ipd_names_dict,
                                     by = c("Attribute:id" = "match_uid"))


  ## filter to only keep the variables in dhis2 exports
  new_names_dict <- dplyr::filter(new_names_dict, !is.na(export_order))

  ## sort the new names based on export order
  new_names_dict <- dplyr::arrange(new_names_dict, export_order)

  ## rename the columns of old excel file
  names(x) <- matchmaker::match_vec(names(x),
                                    dictionary = filter(new_names_dict, !is.na(excel_names)),
                                    from = "excel_names",
                                    to = "Attribute:shortName")

  ## get the names which are not in the old his datasets
  missing_names <- new_names_dict[which(is.na(new_names_dict$excel_names)),
                                  "Attribute:shortName"]

  ## add in names of variables generated by DHIS2
  generated_names <- c("Event",
                       "Program stage",
                       "Event date",
                       "Longitude",
                       "Latitude",
                       "Organisation unit name",
                       "Organisation unit code",
                       "Organisation unit"
  )

  ## add in empties for the columns not currently in old excel file
  x[ , c(generated_names, missing_names)] <- NA

  ## arrange columns correctly
  x <- select(x,
              all_of(generated_names),
              new_names_dict$`Attribute:shortName`,
              c("country",
                "project",
                "report_year",
                "source")
  )

  ## clean up the column names
  names(x) <- epitrix::clean_labels(names(x))

  ############## start recodes

  ## recode sex
  x$sex[x$sex %in% c("F", "f", "F ", "f ", " F", " f", " F ", " f ")] <- "F"
  x$sex[x$sex %in% c("M", "m", "M ", "m ", " M", " m", " M ", " m ")] <- "M"

  ## make age years numeric
  x$age_years <- as.numeric(x$age_years)

  ## make birth weight a character
  x$birthweight <- as.character(x$birthweight)


  ## change recode the exit code dataset
  x <- dplyr::mutate(x,
                     inpatient_exit_status = dplyr::recode(inpatient_exit_status,
                                                           ## anything that doesnt match below is set to "OTA"
                                                           .default = NA_character_,

                                                           "Discharged"                          = "DH",
                                                           "Defaulters"                          = "LA",
                                                           "Referred to other structures"        = "RF",
                                                           "Deaths"                              = "DD"
                     ))

  ## recoding diagnosis
  x <- dplyr::mutate(x,
                     diagnosis_at_exit_primary = dplyr::recode(diagnosis_at_exit_primary,
                                                               ## anything that doesnt match below is set to "OTA"
                                                               .default = "OTA",

                                                               ## standard diagnosis codes
                                                               "Severe diarrhoea/Cholera"                       = "AWD",
                                                               "Bloody diarrhoea"                               = "ABD",
                                                               "Lower respiratory tract infections"             = "LRT",
                                                               "Tuberculosis"                                   = "PTB",
                                                               "Severe malaria (confirmed)"                     = "MAL",
                                                               "Fever of unknown origin"                        = "OTA",
                                                               "Severe malnutrition"                            = "SAM",
                                                               "Anaemia"                                        = "ANE",
                                                               "Meningitis"                                     = "MEN",
                                                               "Neonatal tetanus (suspected)"                   = "NTT",
                                                               "Measles"                                        = "MSL",
                                                               "Acute flacid paralysis"                         = "AFP",
                                                               "AIDS related illness"                           = "HRI",
                                                               "Jaundice"                                       = "AJS",
                                                               "Violence-related injury"                        = "OTS",

                                                               "Chronic diseases"                               = "Chronic diseases",

                                                               "Neonatal disease"                               = "OND",
                                                               "Suspected typhoid"                              = "TPF",

                                                               "Other"                                          = "OTA",

                                                               ## neonatal diagnosis subcodes
                                                               "Prematurity/Dysmaturity"                        = "OND",
                                                               "Surfactant Deficiency"                          = "OND",
                                                               "Meconium Aspiration Syndrome"                   = "OND",
                                                               "Hypoglycemia"                                   = "OND",
                                                               "Transient Tachypnea of the Newborn"             = "OND",
                                                               "Hypoxic/Ischemic Encephalopathy"                = "OND",
                                                               "Manifest Neonatal Infection"                    = "OND",
                                                               "Suspected Maternal-Fetal Infection"             = "OND",
                                                               "Jaundice"                                       = "OND",
                                                               "Prevention Mother To Child Trasmission (PMTCT)" = "OND",
                                                               "Ingestion of Maternal Blood/Amniotic Fluid"     = "OND",
                                                               "Congenital malformation"                        = "OND",
                                                               "Macrosomia"                                     = "OND",
                                                               "Obstetric trauma"                               = "OND",
                                                               "Hypothermia"                                    = "OND",
                                                               "Infection >48 h after birth"                    = "OND",
                                                               "Anaemia (need of transfusion)"                  = "OND",
                                                               "Well baby"                                      = "OND",
                                                               "Other (neonatal)"                               = "OND",
                                                               "Neonatal Option 1"                              = "OND",
                                                               "Neonatal Option 2"                              = "OND",
                                                               "Neonatal Option 3"                              = "OND",
                                                               "Neonatal Option 4"                              = "OND",
                                                               "Neonatal Option 5"                              = "OND"

                     ))

  ## If want to use chronic case definition provided by HIS team then recode
  ## else leave as defined above "Chronic Diseases" so can be used to match later
  if (chronic_defs) {

    x <- dplyr::mutate(x,
                       diagnosis_at_exit_primary = dplyr::recode(diagnosis_at_exit_primary,
                                                                 "Chronic diseases"                               = "OTC"))
  }


  ## return dataset
  x


}
