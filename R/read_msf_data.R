#' Function to read, clean and merge data sets from old HIS excel tools and DHIS2
#'
#' @param country_folder Path to folder of interest (usually containing all files
#' for a specific country/project)
#'
#' @param site Which datasets to read options include "IPD", "OPD" or "IPD Beds"
#'
#' @param data_source Which data source to use options include "HIS", "DHIS", or
#' "Both"(default)
#'
#' @param reformat Logical (TRUE/FALSE) to specify if want to reformat data to
#' fit to DHIS2 exports. FALSE returns original dataset.
#' Must be TRUE if source == "Both"
#'
#' @param chronic_defs Logical (TRUE/FALSE) to define if want to stick to the HIS
#' team definition for chronic conditions (default is TRUE).
#' This will put all old HIS chronic conditions in to new DHIS2 "Other chronic conditions".
#' Otherwise (if set to FALSE) leave as "Chronic diseases" to make available for
#' individually manipulating to combine with DHIS2 data.
#'
#' @param wards Specify which wards want to keep
#' Default is "Medical" (strict definition without ITFC or maternal)
#' "Medical extension" includes all medical and ITFC + maternal
#' "All" also includes surgical. Only used if site == "IPD Beds".
#'
#' @author Alex Spina
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows filter if_else across vars matches
#' @importFrom stringr str_pad str_glue_data str_remove str_remove_all
#' @importFrom aweek as.aweek
#' @importFrom tsibble yearweek
#' @importFrom rio import
#' @importFrom matchmaker match_df match_vec
#' @importFrom epitrix clean_labels
#' @importFrom readxlsb read_xlsb
#'
#' @export


read_msf_data <- function(country_folder, site,
                          data_source = "Both", reformat = TRUE,
                          chronic_defs = TRUE,
                          wards = "Medical") {

  ## remove all text before forward slash to keep only the country
  cntry <- str_remove(country_folder, ".+(\\/)|.+(\\\\)")


  ############ Pull old his excel data together
  if (data_source != "DHIS") {

    ### sort out OPD
    if (site == "OPD") {
      ## get file paths for a specific country
      project_paths <- paste0(country_folder, "/",
                              list.files(country_folder,
                                         pattern = "opd$",
                                         recursive = TRUE)
      )

      ## run clean_opd to get counts on each file
      intermediate_data <- purrr::map(project_paths, clean_opd)

      ## combine all together to have one dataset
      output <- dplyr::bind_rows(intermediate_data)

      ## add zeros to front of week
      output$week <- str_pad(output$week, 2, "left", 0)

      ## combine info to make a calendar week
      ## tsibble doesnt work directly
      ## sometimes there is a 53 week in a year that shouldnt have
      ## aweek deals with that directly
      output$calendar_week <- aweek::as.aweek(
        str_glue_data(output, "{year}-W{week}-1"),
        floor_day = TRUE, factor = TRUE)

      ## change to tsibble week
      output$calendar_week <- tsibble::yearweek(
        str_remove(output$calendar_week, "-"))

      ## create an epiweek (as dates) variable
      output$epiweek <-  as.Date(output$calendar_week)
    }

    ### sort out IPD
    if (site == "IPD") {

      ## get file paths for a specific country
      his_project_paths <- paste0(country_folder, "/",
                                  list.files(country_folder,
                                             pattern = "med$",
                                             recursive = TRUE)
      )

      ## run clean_ipd to change from coded to names on each file
      intermediate_data <- purrr::map(his_project_paths, clean_ipd)

      ## combine all together to have one dataset
      og_his_output <- dplyr::bind_rows(intermediate_data)
    }

    ## sort out IPD bed counts
    if (site == "IPD Beds") {

      ## get file paths for a specific country
      his_project_paths <- paste0(country_folder, "/",
                                  list.files(country_folder,
                                             pattern = "med$",
                                             recursive = TRUE)
      )

      ## run clean_ipd to change from coded to names on each file
      intermediate_data <- purrr::map(his_project_paths, his_ipd_beds)

      ## combine all together to have one dataset
      og_his_output <- dplyr::bind_rows(intermediate_data)
    }



    ## if returning a reformated dataset - then recode to fit DHIS2
    if (reformat) {

      ## recode to DHIS2 format
      if (site == "IPD") {
        his_output <- recode_ipd(og_his_output, chronic_defs = chronic_defs)
      }
      if (site == "OPD") {

        ### TODO: ADD RECODE_OPD FUNCTION

      }

      if (site == "IPD Beds") {
        his_output <- og_his_output
      }


      ## read in data dictionary
      cleaning_dict <- rio::import(file =  system.file("extdata",
                                                       "cleaning_dict.xlsx",
                                                       package = "epitsa"),
                                   sheet = if_else(site == "IPD Beds", "IPD", site), trim_ws = FALSE)


      ## fix variables according to dictionary
      his_output <- matchmaker::match_df(his_output,
                                         dictionary = filter(cleaning_dict,
                                                             country_folder %in% c(cntry,
                                                                                   "Universal")),
                                         from = "old",
                                         to = "new",
                                         by = "grp")
    }


  }


  ############ pull DHIS2 data together
  if (data_source != "HIS") {
    ### sort out OPD
    if (site == "OPD") {

      ### TODO: ADD DHIS OPD READING

    }


    ### sort out IPD
    if (site == "IPD") {

      ## get file paths for a specific country
      dhis_project_paths <- paste0(country_folder, "/",
                                   list.files(country_folder,
                                              pattern = "DHIS_IPD_",
                                              recursive = TRUE)
      )

      ## read in the IPD files
      intermediate_data <- purrr::map(dhis_project_paths,
                                      function(k) rio::import(k, na.strings = c("", "[99]"))
      )

      ## change variables to character (otherwise cant be merged)
      for (i in 1:length(intermediate_data)) {

        for (j in c("Case number", "Birthweight", "ICU admission",
                    "Maternal ID", "Neonate weight at exit",
                    "Time of admission", "Transfusions",
                    "Vaccination status at exit")) {
          intermediate_data[[i]][j] <- as.character(
            intermediate_data[[i]][j])
        }

      }

      ## combine all together to have one dataset
      dhis_output <- dplyr::bind_rows(intermediate_data)

      ## clean up the column names
      names(dhis_output) <- epitrix::clean_labels(names(dhis_output))

      ## add in the country from the file name
      dhis_output$country <- cntry

      ## add in project based on org unit IDs (recoded after merging)
      dhis_output$project <- dhis_output$organisation_unit

      ## change date variables to dates
      dhis_output <- dplyr::mutate(dhis_output,
                                   dplyr::across(
                                     .cols = matches("date|Date"),
                                     .fns = ~as.Date(
                                         # gsub("", NA, .x)
                                         .x
                                       )
                                     )
                                   )


      ## add in variable for report year based on event date
      dhis_output$report_year <- as.numeric(format(dhis_output$event_date,
                                                   format = "%Y"))

      ## need to calculate age years from months and days
      dhis_output$age_years[
        !is.na(dhis_output$age_months)] <- dhis_output$age_months[
          !is.na(dhis_output$age_months)] / 12

      dhis_output$age_years[
        !is.na(dhis_output$age_days)] <- dhis_output$age_days[
          !is.na(dhis_output$age_days)] / 365.25

      ## make referred vars to a character
      dhis_output$referred_from <- as.character(dhis_output$referred_from)
      dhis_output$referred_to   <- as.character(dhis_output$referred_to)

      ## make time to death in to a character
      dhis_output$time_to_death <- as.character(dhis_output$time_to_death)

      ## make birthweight numeric
      dhis_output$birthweight <- as.character(dhis_output$birthweight)

      ## add in the source of dataset
      dhis_output$source <- "DHIS2"


    }


    ### sort out IPD beds
    if (site == "IPD Beds") {

      ## get file paths for a specific country
      dhis_project_paths <- paste0(country_folder, "/",
                                   list.files(country_folder,
                                              pattern = "DHIS_BEDS_IPD_",
                                              recursive = TRUE)
      )

      ## run clean_ipd to change from coded to names on each file
      intermediate_data <- purrr::map(dhis_project_paths,
                                      function(x) {
                                        dhis_ipd_beds(x, wards = wards)})

      ## combine all together to have one dataset
      dhis_output <- dplyr::bind_rows(intermediate_data)
    }

  }


  ############ Recode DHIS formatted data from coded to named
  if (data_source %in% c("Both", "DHIS") | (data_source == "HIS" & reformat)) {
    ### sort out OPD
    if (site == "OPD") {
      ## TODO: ADD OPD RECODING
    }

    ### sort out IPD
    if (site == "IPD") {

      ## get the appropriate dataset to be recoded

      if (data_source == "HIS") {
        output <- his_output
      }

      if (data_source == "DHIS") {
        output <- dhis_output
      }

      if (data_source == "Both") {
        ## combine by sticking rows on below
        output <- bind_rows(his_output, dhis_output)
      }


      ## read in the dictionary to get dhis shortnames with corresponding dhis2 uids
      dhis_shortnames <- rio::import(file = system.file("extdata",
                                                        "cleaning_dict.xlsx",
                                                        package = "epitsa"),
                                     sheet = "dhis_data_elements",
                                     col_types = c("text", "text", "text", "numeric"))


      ## clean dhis shortnames
      dhis_shortnames$`Attribute:shortName` <- epitrix::clean_labels(
        dhis_shortnames$`Attribute:shortName`)

      ## get data element UIDs from meta data download
      de_uids <- matchmaker::match_vec(names(output),
                                       dhis_shortnames,
                                       from = "Attribute:shortName",
                                       to = "Attribute:id")

      ## read in jane's full list of data elements optionset UIDs
      ipd_de_dict <- readxlsb::read_xlsb(
        path = system.file("extdata",
                          "Events_ImportFormat_IPDmed_Template20200403draft.xlsb",
                          package = "epitsa"),
        sheet = "Full_DE_list",
        skip = 1)


      ## get optionset UIDs
      options_uids <- matchmaker::match_vec(de_uids,
                                            ipd_de_dict,
                                            from = "DE.UID",
                                            to = "Optionset.UID")

      ## read in jane's options list
      ipd_options_dict <- readxlsb::read_xlsb(
        path = system.file("extdata",
                          "Events_ImportFormat_IPDmed_Template20200403draft.xlsb",
                          package = "epitsa"),
        sheet = "IPD_Options_list")


      ## clean up options dict by removing brackets
      ipd_options_dict$Option.name <- stringr::str_remove_all(
        ipd_options_dict$Option.name, pattern = ".*\\] ")

      ipd_options_dict$Lookup.code <- stringr::str_remove_all(
        ipd_options_dict$Lookup.code, "\\[|\\]")

      ## backup original names for output
      names_backup <- names(output)

      ## temprorarily rename output to use the match IDs
      ## (have to number duplicates otherwise deoesnt work)
      names(output) <- paste0(options_uids,
                              "-",
                              sequence(rle(options_uids)$lengths))

      ## recode each of the variables individually
      for (i in names(output)) {

        ## remove the counts so can filter the dictionary properly
        var_name <- stringr::str_remove_all(i, "-.*")

        output[,i] <- matchmaker::match_vec(output[,i],
                                            ## filter the dictionary based on ID
                                            dplyr::filter(ipd_options_dict,
                                                          id == {var_name}),
                                            from = "Lookup.code",
                                            to = "Option.name")
      }


      ## put the names back correctly
      names(output) <- names_backup

      ## read in Jane's list of organisational units
      ipd_orgus_dict <- readxlsb::read_xlsb(
        path = system.file("extdata",
                          "Events_ImportFormat_IPDmed_Template20200403draft.xlsb",
                          package = "epitsa"),
        sheet = "Full_OU_list")


      ## recode project separately
      output$project <- matchmaker::match_vec(output$project,
                                              filter(ipd_orgus_dict,
                                                     Org.Unit.UID %in% unique(output$project)),
                                              from = "Org.Unit.UID",
                                              to = "Parent2.name")


    }

    ### sort out IPD Beds (just add in country based on project)
    if (site == "IPD Beds") {

      ## get the appropriate dataset to be recoded

      if (data_source == "HIS") {
        output <- his_output
      }

      if (data_source == "DHIS") {
        output <- dhis_output
      }

      if (data_source == "Both") {
        ## combine by sticking rows on below
        output <- bind_rows(his_output, dhis_output)
      }

      ## read in data dictionary
      cleaning_dict <- rio::import(file = system.file("extdata",
                                                      "cleaning_dict.xlsx",
                                                      package = "epitsa"),
                                   sheet = if_else(site == "IPD Beds", "IPD", site), trim_ws = FALSE)

      ## get data element UIDs from meta data download
      output$country <- matchmaker::match_vec(output$project,
                                              cleaning_dict,
                                              from = "new",
                                              to = "country_folder")


    }
  }


  ############ Add time variables and flag duplicates

  if (site == "OPD") {
    ## flag duplicates

    ## label rows where same week and site with same counts
    output$duplicate_row <- duplicated(output[, c("calendar_week", "project", "site", "disease",
                                                  "pop1_u5", "pop1_o5", "pop2_u5", "pop2_o5")])

    ## label rows where same week and site with different counts
    output$duplicate_site <- duplicated(output[, c("calendar_week", "project", "site", "disease")])
  }

  if (site == "IPD" & reformat) {

    ## create calendar week (from admission date)
    output$calendar_week <- tsibble::yearweek(output$date_of_admission)

    ## create an epiweek (as dates) variable
    output$epiweek <-  as.Date(output$calendar_week)

    ## create calendar week for deaths (from exit date)
    output$calendar_week_death <- tsibble::yearweek(output$date_of_exit)

    ## create an epi week (as dates) variable for deaths
    output$epiweek_death <-  as.Date(output$calendar_week_death)

    ## flag duplicates
    output$duplicate_row <- duplicated(output[, c("calendar_week", "project",
                                                  "case_number", "age_years", "sex",
                                                  "diagnosis_at_exit_primary")])
  }

  ## no point flagging duplicates here because project names different
  # if (site == "IPD Beds" & reformat) {
  #   ## flag duplicates
  #   output$duplicate_row <- duplicated(output[, c("project", "week")])
  # }

  ############ return appropriate dataframe

  ## if want the original excel files not recoded
  if (data_source == "HIS" & !reformat) {
    og_his_output
  } else {
    output
  }



  # ## if want the original excel files but recoded
  # if (data_source == "HIS" & reformat) {
  #   his_output
  # }
  #
  # ## if want DHIS not recoded (with number codes)
  # if (data_source == "DHIS" & !reformat) {
  #   dhis_output
  # }
  #
  # ## if want to combine old his and dhis (recoded always)
  # if (data_source == "Both") {
  #   output
  # }
}
