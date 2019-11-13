# The purpose of this script is to take patricks_plants (i.e., output of read_patricks_xlsx.R) and 
# add habit / duration information using the information in LCDOtax.

# Importing LCDOtax + creating the lookup table for growth habit and duration:

create_PDPs <- function(path){

LCDOtax <- readxl::read_xlsx(path, sheet = "LCDOtax", col_types = "text")

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
	replace_na(list(codeinPLANTS = FALSE, codeaccPLANTS = FALSE, introduced = FALSE, endemic = FALSE, inNM = FALSE, inOMDPNM = FALSE, notAllred = FALSE))  

habdurconv <- tibble(habdur = c("NA NA","5 Perennial","1 Perennial","2 Perennial","3 Perennial","5 Annual","4 Perennial","6 Annual","5 NA","7 Annual","7 Perennial","7 NA","6 Perennial","6 NA"),
	habit_duration = c("Unknown","Perennial Forb","Tree","Shrub","Subshrub","Annual Forb","Succulent","Annual Grass","Forb","Annual Sedge","Perennial Sedge","Sedge","Perennial Grass","Grass"))

LCDOtax$habdur <- paste(LCDOtax$GrowthHabitCode,LCDOtax$Duration)

LCDOtax <- left_join(LCDOtax, habdurconv, by = "habdur")

PDPs <- patricks_plants %>% mutate(habit_duration = LCDOtax$habit_duration[match(plant_id, LCDOtax$taxoncode)])

return(PDPs)}

# Below is a second version adding a short code for habit / duration; it should replace the first version, but I haven't checked that it works, yet.

# Importing LCDOtax + creating the lookup table for growth habit and duration:

create_PDPs <- function(path){

LCDOtax <- readxl::read_xlsx(path, sheet = "LCDOtax", col_types = "text")

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
	replace_na(list(codeinPLANTS = FALSE, codeaccPLANTS = FALSE, introduced = FALSE, endemic = FALSE, inNM = FALSE, inOMDPNM = FALSE, notAllred = FALSE))  

habdurconv <- tibble(habdur = c("NA NA","5 Perennial","1 Perennial","2 Perennial","3 Perennial","5 Annual","4 Perennial","6 Annual","5 NA","7 Annual","7 Perennial","7 NA","6 Perennial","6 NA"),
	habit_duration = c("Unknown","Perennial Forb","Tree","Shrub","Subshrub","Annual Forb","Succulent","Annual Grass","Forb","Annual Sedge","Perennial Sedge","Sedge","Perennial Grass","Grass"), 
	habit_durshort = c("U","PF","TR","SH","SS","AF","SU","AG","F","AS","PS","S","PG","G"))

LCDOtax$habdur <- paste(LCDOtax$GrowthHabitCode,LCDOtax$Duration)

LCDOtax <- left_join(LCDOtax, habdurconv, by = "habdur")

PDPs <- patricks_plants %>% 
  mutate(habit_duration = LCDOtax$habit_duration[match(plant_id, LCDOtax$taxoncode)],
  habit_durshort = LCDOtax$habit_durshort[match(plant_id, LCDOtax$taxoncode)])

return(PDPs)}
