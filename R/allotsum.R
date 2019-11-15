
# These functions (allotsum and associates) assume that there is an object named "PDPs" that is the output from
# create_PDPs.R.

# allotsum summarizes data by allotment; the output is reformatted to be similar to that of the original
# PDPs excel file, since this seems to me the most readable way to present the information.

allotsum <- function(allotnum, length="long"){
  
  if(length=="long") {
    allotsummary <- PDPs %>%
      filter(allotment %in% allotnum) %>%  # filter works on rows
      select(species, plant_id, common_name, habit_duration, site) %>%   # select works on columns
      return(.)
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
  
  else if(length=="short") {
    allotsummary <- PDPs %>%
      filter(allotment %in% allotnum) %>%  # filter works on rows
      select(species, plant_id, common_name, habit_durshort, site) %>%   # select works on columns
      return(.)
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
    sites <- tibble::add_column(sites, species = "", plant_id = "", common_name = "", habit_durshort = "", .before = 1)
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
    return(allotsummary)
    }
  }

#################################################################################

# and this one summarizes data by plots that had a particular target species (use the code)

assoctable <- function(code){
  
  # create a vector of site names where the plant is present
  
  relevant_sites <- PDPs %>%
    filter(plant_id %in% code) %>%
    select(site) %>%
    return(.)
  
  # user the vector above to filter PDPs down to the relevant sites
  
  internal <- PDPs %>%
    filter(site %in% relevant_sites$site) %>%  # filter works on rows
    select(species, plant_id, common_name, habit_duration, site) %>%   # select works on columns
    return(.)
  
  # wrangling site metadata
  
  sites <- PDPs %>%
    filter(site %in% internal$site) %>%
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
  
  # wrangling internal data
  
  internal <- mutate(internal, presence = 1)
  internal <- pivot_wider(
    internal,
    names_from = site,
    values_from = presence,
    values_fill = list(presence = 0))
  scols <- 5:ncol(internal)
  internal <- mutate(
    internal,
    prop = rowSums(internal[scols]) / (ncol(internal) - 4))
  
  # adding site metadata to internal data
  
  internal <- rbind(sites,internal)
  
  return(internal)
}
