# input is an existing tibble (if using PDPs, it is assumed you already used create_PDPs();
#    if using AIM, assumed you are pulling from the server)
# boundary is a polygon shapefile (dbf, prj, shp, shx)
# boundary is assumed to be in current working directory
# CRS of boundary is not assumed
# input is assumed to be in NAD83 lat/long (=WGS84 lat/long)
# AIM requires a connection set up following Rachel's instructions
# haven't tested with the column selection, yet...
#
# getAIMPA gets all AIM species richness data
#
# clip... clips; either AIM species richness data or patricks_plants
#
# AIMconv converts output from AIMPA into the format of patricks_plants
#
# usda_code to row will break this someday

# test tibble
#
# test <- tibble(site =
#     c("horse canyon 4", "horse canyon RHA 2", "horse canyon 5", "horse canyon RHA 1"),
#     latitude = c(32.53699,32.54220,32.54300,32.53040),
#     longitude=c(-107.05199,-107.00930,-107.03507,-107.04020))

# AIMcon <- odbcConnect("ilmocAIMPub")

getAIMPA <- function(connection){
  internal <- sqlQuery(connection, 'SELECT ProjectName, EcologicalSiteId, PlotID, Latitude_NAD83, Longitude_NAD83, PrimaryKey, State, County FROM ilmocAIMPub.ilmocAIMPubDBO.TerrADat;') %>%
    mutate(PrimaryKey = as.character(PrimaryKey))
  tblSpecRichDetail <- sqlQuery(connection, 'SELECT SpeciesList, PrimaryKey FROM ilmocAIMPub.ilmocAIMPubDBO.tblSpecRichDetail;') %>%
    mutate(PrimaryKey = as.character(PrimaryKey),
           SpeciesList = factor(SpeciesList))
  tblSpecRichHeader <- sqlQuery(connection, 'SELECT FormDate, OBJECTID, SpecRich1Area, PrimaryKey FROM ilmocAIMPub.ilmocAIMPubDBO.tblSpecRichHeader;') %>%
    mutate(PrimaryKey = as.character(PrimaryKey),
           SpecRich1Area = as.numeric(SpecRich1Area),
           OBJECTID = as.integer(OBJECTID))
  tblPlots <- sqlQuery(connection, 'SELECT Elevation, PrimaryKey FROM ilmocAIMPub.ilmocAIMPubDBO.tblPlots;') %>%
    mutate(PrimaryKey = as.character(PrimaryKey),
           Elevation = as.numeric(Elevation))
  internal <- left_join(tblSpecRichDetail, internal, by="PrimaryKey")
  internal <- internal %>% filter(!(Latitude_NAD83 %in% NA))
  internal <- left_join(internal, tblSpecRichHeader, by="PrimaryKey")
  internal <- left_join(internal, tblPlots, by="PrimaryKey")
}

# below, use a kind other than "AIM" for post-converted AIM data as well; anything with latitude
# and longitude coordinate fields

clip <- function(kind,boundary,input){

  if(kind=="AIM"){

    itemp.sp <- input
    coordinates(itemp.sp) <- c("Longitude_NAD83", "Latitude_NAD83")
    proj4string(itemp.sp) <- CRS("+proj=longlat +datum=NAD83")
    itemp.sp}

  else {

    itemp.sp <- input
    coordinates(itemp.sp) <- c("longitude", "latitude")
    proj4string(itemp.sp) <- CRS("+proj=longlat +datum=NAD83")
    itemp.sp}

  btemp.sp <- readOGR(dsn=".", layer=boundary)

  btemp.sp <- spTransform(btemp.sp, CRS=CRS("+proj=longlat +datum=NAD83"))

  output <- itemp.sp[btemp.sp,] %>% as_tibble()

  return(output)
  }

#####################################################################################
#
# for some reason, taxpath in AIMconv is the only argument in these functions that will not work unless you
# include the "taxpath ="
#
# the smallest value in OBJECTID is 65405 so, unless that changes... conflicts with survey_id values created
# by read_patricks_xlsx() are very unlikely for the foreseeable future

AIMconv <- function(input,taxpath){
  internal <- input %>% select(-PrimaryKey) %>%
    mutate(OBJECTID = as.integer(OBJECTID),
           ProjectName = factor(ProjectName),
           PlotID = factor(PlotID),
           FormDate = as.Date(FormDate),
           Latitude_NAD83 = as.numeric(Latitude_NAD83),
           Longitude_NAD83 = as.numeric(Longitude_NAD83),
           SpeciesList = factor(SpeciesList),
           Elevation = as.numeric(Elevation),
           State = factor(State),
           County = factor(County),
           SpecRich1Area = as.numeric(SpecRich1Area),
           EcologicalSiteId = factor(EcologicalSiteId)) %>%
    rename(survey_id = OBJECTID,
           project = ProjectName,
           site = PlotID,
           date = FormDate,
           latitude = Latitude_NAD83,
           longitude = Longitude_NAD83,
           elevation = Elevation,
           plant_id = SpeciesList,
           state = State,
           county = County,
           area = SpecRich1Area,
           ecological_site_code = EcologicalSiteId)

  # create an internal copy of LCDOtax by calling create_LCDOtax...

LDCOtax <- create_LCDOtax(taxpath)

  # populate common name, scientific name, family (Allred); will add taxon code
  # checking / correction later

  internal <- internal %>%
    mutate(common_name = LCDOtax$commname[match(plant_id, LCDOtax$taxoncode)],
           family = LCDOtax$Allredfam[match(plant_id, LCDOtax$taxoncode)],
           species = LCDOtax$name[match(plant_id, LCDOtax$taxoncode)])
}

############################################################################

# creating a working version of LCDOtax... here as a standalone, replicated in
# create_PDPs, used in AIMconv... should have them refer here.

create_LCDOtax <- function(taxpath){

  LCDOtax <- readxl::read_xlsx(taxpath, sheet = "LCDOtax", col_types = "text")

  LCDOtax <- LCDOtax %>%
    mutate(rownumber = as.integer(rownumber),
           Allredfam = factor(Allredfam),
           APG4fam = factor(APG4fam),
           name = factor(name),
           synonymof = factor(synonymof),
           comments = factor(comments),
           namecode = factor(namecode),
           taxoncode = factor(taxoncode),
           parentcode = factor(parentcode),
           codeinPLANTS = as.logical(codeinPLANTS), #,1
           codeaccPLANTS = as.logical(codeaccPLANTS), #,1
           PLANTScode = factor(PLANTScode),
           commname = factor(commname),
           taxkind = factor(taxkind),
           introduced = as.logical(introduced), #,TRUE
           NMnox = factor(NMnox), #, A, B, C, Watch
           endemic = as.logical(endemic), #,TRUE
           inNM = as.logical(inNM), #,1
           inOMDPNM = as.logical(inOMDPNM), #,1
           notAllred = as.logical(notAllred), #,1
           fixedgrp = factor(fixedgrp),
           opengrp = factor(opengrp),
           SortSeq = factor(SortSeq),
           GrowthHabitCode = factor(GrowthHabitCode),
           Duration = factor(Duration),
           Stabilizing = as.logical(Stabilizing),
           Invasive = as.logical(Invasive))

  LCDOtax %>%
    replace_na(list(
      codeinPLANTS = FALSE,
      codeaccPLANTS = FALSE,
      introduced = FALSE,
      endemic = FALSE,
      inNM = FALSE,
      inOMDPNM = FALSE,
      notAllred = FALSE))
}

#############################################################################
#
# this function exists, instead of just calling bind_rows, for three reasons:
# to assign FALSE values to rows of AIMPA;
# because I get a bunch of error messages from bind_rows indicating that columns
# were coerced from their original data type, and it's easier to just reassign
# data types to everything I think I need to assign a data type to than try to
# figure out which columns are affected, which aren't, and therefor which actually
# need to be changed.

append_AIMPA <- function(PDPs,AIMPA){
  internal <- bind_rows(PDPs,AIMPA)
  internal <- internal %>%
    mutate(survey_id = as.integer(survey_id),
           project = factor(project),
           allotment = factor(allotment),
           site = factor(site),
           date = lubridate::ymd(date),
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
           area = as.numeric(area),
           family = factor(family),
           species = factor(species),
           usda_code = factor(usda_code),
           plant_id = factor(plant_id),
           common_name = factor(common_name),
           exclude_from_analyses = replace(exclude_from_analyses,is.na(exclude_from_analyses),FALSE))
}
