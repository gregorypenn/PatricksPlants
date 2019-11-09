# Patrick's spreadsheet contains a sparse matrix of 1s in rows for plants and columns for
# site-surveys. The strategy for wrangling this is.
#
# 1. Find and trim non-data from right and bottom sides of spreadsheet.
# 2. Create survey_is as 1:n_surveys with NA for non-survey columns. Bind to top.
# 3. Copy survey metadata to separate table and transpose.
# 4. Remove survey metadata from trimmed sheet and add survey_id as colnames
# 4a. Add unique site_id to surveys
# 5. Gather trimmed sheet surveys with key = survey_id and value = presence.
# 6. Join survey and plant tables

#' Read Patrick's xlsx file
#'
#' \code{read_patricks_xlsx} returns a data frame after parsing Patrick's xlsx file.
#'
#' Reads and parses Patrick's xlsx file of plant obersvations and metadata into a data frame.
#'
#' @param path Path to xlsx file.
#' @param coding Code occurance data as "presence-only" or "presence-absence".
#' @return  A data frame (tibble) of plants observed at sites.
#' @examples
#' read_patricks_xlsx("path/to/file.xlsx")
#' @export
#' @importFrom magrittr %>%
read_patricks_xlsx <- function (path, coding = "presence-only") {

  if (coding != "presence-only" & coding != "presence-absence") {
    stop('Specify coding as "presence-only" or "presence-absence"')
  }

  spreadsheet <- readxl::read_xlsx(path = path,
                                   col_names = FALSE,
                                   col_types = "text")

  ##### Trim non-data from right and bottom edges of spreadsheet --------------
  # Column for first survey is fixed, but last survey changes as data is added.

  first_survey_row <- 1
  last_survey_row  <- 35
  survey_rows <- first_survey_row:last_survey_row

  first_survey_col <- 7
  last_survey_col  <- grep("blank", unname(spreadsheet[1,])) - 1
  survey_cols <- first_survey_col:last_survey_col

  first_plant_row <- last_survey_row + 1
  last_plant_row <- which(spreadsheet[,1] == "totals") - 1
  plant_rows <- first_plant_row:last_plant_row

  trimmedsheet <- spreadsheet[1:last_plant_row, 1:last_survey_col]

  ##### Create surveys data frame ---------------------------------------------

  surveys <- trimmedsheet[survey_rows, survey_cols]
  surveys <- t(surveys)
  surveys <- as_tibble(surveys)

  surveys_header <- trimmedsheet$...1[first_survey_row:last_survey_row] %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all(",", "")

  names(surveys) <- surveys_header

  surveys <- surveys %>%
    mutate(survey_id = 1:nrow(.),
           project = factor(project),
           allotment = factor(allotment),
           site = factor(site),
           date = lubridate::dmy(date),
           photographed = as.logical(photographed),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           elevation = as.numeric(elevation),
           state = factor(state),
           county = factor(county),
           ecological_site = factor(ecological_site),
           ecological_site_code = factor(ecological_site_code),
           herbicide_treatment_year = as.integer(herbicide_treatment_year),
           year = as.integer(year),
           herbicide_treated = as.logical(herbicide_treated),
           LCDO_binary = as.integer(LCDO_binary),
           exclude_from_analyses = as.logical(exclude_from_analyses),
           area = parse_area(area_approx))

  # Todo: create and add unique site_id

  ##### Create occurance data frame
  occurance <- trimmedsheet[plant_rows,]
  plants_colnames <- c("family", "species", "row", "plant_id", "common_name", "taxon_notes")
  names(occurance) <- c(plants_colnames, surveys$survey_id)

  occurance <- occurance %>%
    tidyr::pivot_longer(cols = survey_cols,
                 names_to = "survey_id",
                 values_to = "presence",
                 values_drop_na = FALSE) %>%
    mutate(family = factor(family),
           species = factor(species),
           row = factor(row),
           plant_id = factor(plant_id),
           common_name = factor(common_name),
           survey_id = as.integer(survey_id),
           presence = as.logical(as.integer(presence)))

  if (coding == "presence-absence")  {
    occurance$presence <- tidyr::replace_na(occurance$presence, FALSE)
  } else if (coding == "presence-only") {
    occurance <- filter(occurance, !is.na(presence)) %>%
      select(-presence)
  }

  full_join(occurance, surveys) %>%
    return(.)

}
