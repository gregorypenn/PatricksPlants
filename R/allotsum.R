
# This function assumes that there is an object named "PDPs" that is the output from
# create_PDPs.R.

allotsum <- function(allotnum){

allotsummary <- PDPs %>%
  filter(allotment %in% allotnum) %>%  # filter works on rows
  select(species, plant_id, common_name, habit_duration, site) %>%   # select works on columns
  return(.)

# wrangling site metadata

sites <- PDPs %>%
  filter(site %in% allotsummary$site) %>%
  distinct(site, .keep_all = TRUE) %>%
  select(site, date, latitude, longitude) %>%
  return(.)
options(digits=10) # If any of the to-be-transposed values are greater than the current digits setting, they will be truncated!
sites <- t(sites)
sites <- tibble::as_tibble(sites)
names(sites) <- sites %>% slice(1) %>% unlist()
sites <- sites %>% slice(-1)
sites <- tibble::add_column(sites, species = "", plant_id = "", common_name = "", habit_duration = "", .before = 1)
sites <- mutate(sites, prop = "")

# wrangling allotsummary data

allotsummary <- mutate(allotsummary, presence = 1)
allotsummary <- pivot_wider(
  allotsummary,
  names_from = site,
  values_from = presence,
  values_fill = list(presence = 0))
scols <- 5:ncol(allotsummary)
allotsummary <- mutate(
  allotsummary,
  prop = rowSums(allotsummary[scols]) / (ncol(allotsummary) - 4))

# adding site metadata to allotsummary data

allotsummary <- rbind(sites,allotsummary)

return(allotsummary)}

# second version incorporating short as well as long habit duration text; should replace the first but has not been tested yet

allotsum <- function(allotnum, length){

if(length=="long") {
  allotsummary <- PDPs %>%
    filter(allotment %in% allotnum) %>%  # filter works on rows
    select(species, plant_id, common_name, habit_duration, site) %>%   # select works on columns
  return(.)}
else {
  allotsummary <- PDPs %>%
    filter(allotment %in% allotnum) %>%  # filter works on rows
    select(species, plant_id, common_name, habit_durshort, site) %>%   # select works on columns
  return(.)}  
  
allotsummary <- PDPs %>%
  filter(allotment %in% allotnum) %>%  # filter works on rows
  select(species, plant_id, common_name, habit_duration, site) %>%   # select works on columns
  return(.)

# wrangling site metadata

sites <- PDPs %>%
  filter(site %in% allotsummary$site) %>%
  distinct(site, .keep_all = TRUE) %>%
  select(site, date, latitude, longitude) %>%
  return(.)
options(digits=10) # If any of the to-be-transposed values are greater than the current digits setting, they will be truncated!
sites <- t(sites)
sites <- tibble::as_tibble(sites)
names(sites) <- sites %>% slice(1) %>% unlist()
sites <- sites %>% slice(-1)
sites <- tibble::add_column(sites, species = "", plant_id = "", common_name = "", habit_duration = "", .before = 1)
sites <- mutate(sites, prop = "")

# wrangling allotsummary data

allotsummary <- mutate(allotsummary, presence = 1)
allotsummary <- pivot_wider(
  allotsummary,
  names_from = site,
  values_from = presence,
  values_fill = list(presence = 0))
scols <- 5:ncol(allotsummary)
allotsummary <- mutate(
  allotsummary,
  prop = rowSums(allotsummary[scols]) / (ncol(allotsummary) - 4))

# adding site metadata to allotsummary data

allotsummary <- rbind(sites,allotsummary)

return(allotsummary)}
