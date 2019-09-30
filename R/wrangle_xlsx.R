# Issues: MIGU is a duplicated plant_id and the plants table is not being created for the SQL database
# the flattened dataframe patricks_plants.df is still being created. 2018-08-23. Search for "Warning"

# Summary
# Creates: SQLite database (patricks_plants.sqlite) and tidy table (patricks_plants.df)
#          from Patrick Alexander's plant observation data.
# Requires: a csv file converted from Patrick's Excel spreadsheet.

# Details
# This R code imports a csv of Patrick Alexander's plant observation data.
# The data is normalized into separate tables, from which an Sqlite3 database
# is created and populated. A Shiny App is being developed to provide an
# interface to the database.
#
# The process follows this sequence:
# 1) Create SQLite database with no tables as file on disk.
# 2) Read and wrangle spreadsheet data.
# 3) Create a normalized table in R and its counterpart in the database.
# 4) Repeat 3 for each normalized table.


# The spreadsheat containing this data effectively contains two tables in a single sheet.
# There exists chaff (i.e. not-data) to be trimmed from the right and bottom margins.
# Survey table: The first 35 rows are a transposed table of information about survey location, date, etc.
# Occurance table: From row 36 to the chaff it's a wide-form table of plant occurance info.
# The first 6 columns represent plant metadata. The remaining columns are a wide-form table of
# occurance coding, which each column represents a site, each row represents a plant, and occurance
# is indicated as "1".
#
# The trick to loading this data is to split the two tables apart,
# wrangle them into shape separately, then re-join them on the "survey_id" key field.
# survey_ids are generated as column headers by R on import.

##### Description of normalized data frames created for sql database #####
# plants: plant metadata;                                   key = plant_id
# surveys: site-visit metadata;                             key = survey_id
# sites: site metadata;                                     key = site
# plants_surveyed: foreign keys to survey_id and plant_id   key = joint plant_id + survey_id

# observations: long-form combination of plant metadata with survey and present columns, trimmed to presence only
# plants_surveyed: joined plant observations and survey metadata

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)
library(DBI)
library(RSQLite)

#### Version ####
spreadsheet_version <- "PJA_PDPs_3Sep19"
basedir <- "/Users/gregory/Dropbox/Patrick_data/"
filedir <- str_c(basedir, spreadsheet_version, "/")
csv_file <- str_c(filedir, spreadsheet_version, ".csv")
xlsx_file <- str_c(filedir, spreadsheet_version, ".xlsx")

#### Create SQLite Database ####
con <- dbConnect(SQLite(), dbname = "patricks_plants.sqlite")

#### Import spreadsheet and check for changes in headers ####

setwd(filedir)
spreadsheet <- read.csv(csv_file, stringsAsFactors = FALSE, fileEncoding = "macintosh", header = FALSE)
load("/Users/gregory/Dropbox/Patrick_data/R/spreadsheet_headers.Rdata"); stopifnot(spreadsheet.headers == spreadsheet[1:36,1:6])

#### Where the data is: ####
survey_metadata.first_row <- 1
survey_metadata.last_row <- 35
spreadsheet <- read_xlsx(xlsx_file, col_names = FALSE, col_types = "text")

survey_metadata.rows <- survey_metadata.first_row:survey_metadata.last_row

survey_cols.first <- 7
find_last_header <- function () {
  header <- "blank"
  num_found <- apply(spreadsheet, 2, function(r) any(r == "blank", na.rm=TRUE)) %>% sum()
  if (num_found == 1) {
    return(apply(spreadsheet, 2, function(r) any(r == "blank", na.rm=TRUE)) %>% which() - 1)
  }
  else { stop() }
}
survey_cols.last <- find_last_header()
survey_cols <- survey_cols.first:survey_cols.last

plants_metadata.first_col <- 1
plants_metadata.last_col <- 6
plants_metadata.cols <- plants_metadata.first_col:plants_metadata.last_col

plants.first_row <- survey_metadata.last_row + 1
plants.last_row <- which(spreadsheet$...1 == "totals") - 1
plants.rows <- plants.first_row:plants.last_row

#### Trimming not-data from right and bottom margins ####
trimmedsheet <- spreadsheet %>%
  slice(1:plants.last_row) %>%
  select(1:survey_cols.last)

#### Create surveys_and_sites data frame ####
# Column names generated on import will be used as unique survey ids.
# These will be added to the data.
survey_id <- names(trimmedsheet)[survey_cols] %>%
  str_replace("...", "") %>%
  as.integer()

# Because the table is transposed, the first column is the header.
# Just need to replace bad characters.
surveys_and_sites_header <- trimmedsheet$...1[survey_metadata.rows] %>%
  str_replace_all(" ", "_") %>%
  str_replace(",", "")

# Clip out the table, insert survey_id and transpose.
surveys_and_sites <- trimmedsheet %>%
  slice(survey_metadata.rows) %>%
  select(survey_cols) %>%
  rbind(survey_id) %>%
  t() %>%
  as_tibble()

names(surveys_and_sites) <- c(surveys_and_sites_header, "survey_id")

# Correct data types.
surveys_and_sites <- mutate(surveys_and_sites,
                  project = as.factor(project),
                  allotment = as.factor(allotment),
                  site = as.factor(site),
                  date = dmy(date),
                  date_verbatim = as.character(date_verbatim),
                  photographed = as.logical(photographed),
                  area_approx = as.character(area_approx),
                  latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude),
                  elevation = as.numeric(elevation),
                  elevation_verbatim = as.character(elevation_verbatim),
                  state = as.factor(state),
                  county = as.factor(county),
                  site_descriptions_and_comments = as.character(site_descriptions_and_comments),
                  ecological_site = as.factor(ecological_site),
                  ecological_site_code = as.factor(ecological_site_code),
                  herbicide_treatment_verbatim = as.character(herbicide_treatment_verbatim),
                  herbicide_treatment_year = as.integer(herbicide_treatment_year),
                  year = as.integer(year),
                  herbicide_treated = as.logical(herbicide_treated),
                  years_since_treatment = as.numeric(years_since_treatment),
                  LCDO_binary = as.integer(LCDO_binary),
                  exclude_from_analyses = as.logical(exclude_from_analyses),
                  statedirectory = as.character(statedirectory),
                  countydirectory = as.character(countydirectory),
                  exportmarker = as.numeric(exportmarker),
                  NSD = as.character(NSD),
                  NSD_comments = as.character(NSD_comments),
                  image_1 = as.character(image_1),
                  image_2 = as.character(image_2),
                  image_3 = as.character(image_3),
                  image_4 = as.character(image_4),
                  image_5 = as.character(image_5),
                  image_6 = as.character(image_6),
                  image_7 = as.character(image_7),
                  survey_id = as.integer(survey_id)
                  )

#### Sites - normalized ####
sites <- surveys_and_sites %>%
  select(
    site, # primary key
    allotment,
    area_approx,
    latitude,
    longitude,
    elevation,
    elevation_verbatim,
    state,
    county,
    ecological_site,
    ecological_site_code,
    LCDO_binary,
    statedirectory,
    countydirectory,
    exportmarker
  ) %>%
  distinct() %>%
  arrange(site) %>%
  tibble::rownames_to_column(var = "site_id")

# Add site_id to surveys_and_sites
surveys_and_sites <- inner_join(surveys_and_sites, sites)

# Temporary hack until Patrick fixes his site names to be unique.
# Hopefully not needed now that site_id has been added.
# duplicated_site_names <- sites$site[which(duplicated(sites$site))] %>% as.character()
# duplicated_sites.df <- sites %>% filter(site %in% duplicated_site_names)
# sites_sans_dupes <- sites %>% filter(!(site %in% duplicated_site_names))

# Database
dbSendQuery(con, "
            CREATE TABLE sites (
              site_id int PRIMARY_KEY,
              site text,
              allotment text,
              area_approx text,
              latitude numeric,
              longitude numeric,
              elevation numeric,
              elevation_verbatim text,
              state text,
              county text ,
              ecological_site text,
              ecological_site_code text,
              LCDO_binary text,
              statedirectory text,
              countydirectory text,
              exportmarker numeric);
            ")

# dbWriteTable(con, "sites", sites_sans_dupes, append = TRUE)
dbWriteTable(con, "sites", sites, append = TRUE)
dbGetQuery(con, "SELECT count (*) FROM sites;")

#### Surveys - normalized ####
surveys <- surveys_and_sites %>%
  select(
    survey_id, # primary key
    site_id,   # key to sites table
    date,
    date_verbatim,
    photographed,
    site_descriptions_and_comments,
    herbicide_treatment_verbatim,
    herbicide_treatment_year,
    year,
    herbicide_treated,
    years_since_treatment,
    exclude_from_analyses,
    NSD,
    NSD_comments,
    image_1,
    image_2,
    image_3,
    image_4,
    image_5,
    image_6,
    image_7
  )

# Database
dbSendQuery(con, "
            CREATE TABLE surveys (
              survey_id integer PRIMARY KEY,
              site_id int,
              date date,
              date_verbatim text,
              photographed boolean,
              site_descriptions_and_comments text,
              herbicide_treatment_verbatim text,
              herbicide_treatment_year integer,
              year integer,
              herbicide_treated boolean,
              years_since_treatment integer,
              exclude_from_analyses boolean,
              NSD text,
              NSD_comments text,
              image_1 text,
              image_2 text,
              image_3 text,
              image_4 text,
              image_5 text,
              image_6 text,
              image_7 text,
              FOREIGN KEY (site_id) REFERENCES sites(site_id));
            ")

dbWriteTable(con, "surveys", surveys, append = TRUE)
dbGetQuery(con, "SELECT count (*) FROM surveys;")

##### Plants - normalized #####
plants_metadata_names <- c("family", "species", "usda_code", "plant_id", "common_name", "taxon_notes")
plants <- trimmedsheet %>%
  select(plants_metadata.cols) %>%
  slice(plants.rows) %>%
  distinct() # Warning! This shouldn't be necessary and needs attention!
names(plants) <- plants_metadata_names

# Warning! plant_id MIGU is duplicated and needs attention


# Stop if any plant_id is duplicated
stopifnot(!anyDuplicated(plants$plant_id))

# Select fields in same order that they'll have in the database.
plants <- plants %>% select(plant_id, family, species, usda_code, common_name, taxon_notes)

# Create plants table in database.
# Field order must be same as data frame.
dbSendQuery(con, "
  CREATE TABLE plants (
          plant_id text PRIMARY KEY,
          family text,
          species text,
          usda_code text,
          common_name text,
          taxon_notes text);
          ")

plants <- plants %>% select(plant_id, family, species, usda_code, common_name, taxon_notes)
dbWriteTable(con, "plants", plants, append = TRUE)
dbGetQuery(con, "SELECT count (*) FROM plants;")

##### Plants surveyed - normalized #####
# plants_metadata_names <- c("family", "species", "usda_code", "plant_id", "common_name", "taxon_notes")
observations <- trimmedsheet %>%
  slice(plants.rows) %>%
  gather(key = "survey_id", value = "present", survey_cols) %>%
  filter(present == "1")
names(observations)[plants_metadata.cols] <- plants_metadata_names

# survey_id was taken from column headers and needs
# to be stripped of letters and converted into an integer
# in order to be used as an autoincrementing primary key later.
observations$survey_id <- observations$survey_id %>%
  str_replace("...", "") %>%
  as.integer()

# Select fields in same order they will have in the database.
plants_surveyed <- observations %>% select(plant_id, survey_id)

# Create table in database.
# Field order must be same as data frame.
dbSendQuery(con, "
    CREATE TABLE plants_surveyed (
        plant_id text,
        survey_id integer,
        PRIMARY KEY (plant_id, survey_id),
        FOREIGN KEY (plant_id) REFERENCES plants(plant_id),
        FOREIGN KEY (survey_id) REFERENCES surveys(survey_id));
        ")

# Write to database.
dbWriteTable(con, "plants_surveyed", plants_surveyed, append = TRUE)
dbGetQuery(con, "SELECT count (*) FROM plants_surveyed;")

# Verify that data was inserted
nrow(plants_surveyed) == dbGetQuery(con, "SELECT count(*) FROM plants_surveyed;")[1,1]

# Verify that all rows are unique, stop if any duplicates
stopifnot(!anyDuplicated(plants_surveyed))

##### Create flattened data frame with join #####
patricks_plants.df <- plants_surveyed %>%
  inner_join(surveys) %>%
  inner_join(sites)
filename <- paste0("patricks_plants_", spreadsheet_version, ".Rdata")
save(patricks_plants.df, file = filename)

#### Testing Database ####
dbGetQuery(con, "SELECT count(*) FROM plants;")
dbGetQuery(con, "SELECT count(*) FROM sites;")
dbGetQuery(con, "SELECT count(*) FROM surveys;")
dbGetQuery(con, "SELECT count(*) FROM plants_surveyed;")
dbDisconnect(con)
