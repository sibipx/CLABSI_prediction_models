# DATA PREPARATION CONFIG
# -----------------------

# libraries and functions
# -----------------------

# check OS (on HPC - Linux paths and libraries are different)
if (Sys.info()[["sysname"]] == "Linux") is_linux <- TRUE else is_linux <- FALSE

if (!is_linux) { # don't load on HPC - no connection to DB
  library(odbc)
  library(DBI)
}
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)

# How should the landmarks be created?
# Accepted values: "calendar day", "24 hours", "trigger"
# "calendar day" – not implemented
# "24 hours" - first landmark 
landmark_mode <- "24 hours"
# landmark_mode <- "trigger"

# There are bandage and insert point observations (referred together as "bandage observation") 
# with missing catheter type and location.
# If TRUE, the bandage observation catheter type and location will be imputed using LOCF
# and catheters can be "continued" in time considering that these "orphaned" observations are linked to them.
# If FALSE, orphaned bandage observations will be removed before calculating catheter 
# end time and catheter episodes.
# In both cases, bandage observations with catheter type and location filled in are used.
# In both cases, orphaned bandage observations will be extracted as feature. 
# This setting only affects the catheter episode calculation.
use_orphaned_bandage_observation_in_catheter_episode_calculation <- FALSE
# FLASE seems better than TRUE. TRUE is making some peripheral catheters as central lines.

# when to start a catheter episode?
# FALSE: at first catheter placed, regardless of time passed since admission
# TRUE: at first catheter placed or 48h after admission (if the catheter was placed in the first 48h)
baseline_is_48h_after_admission <- FALSE

# the time after registraton fow which catheters are still considered at risk
catheter_end_time_lag <- 48

# CATHETER CONFIG
# ---------------

# use hoofding Dialyse zorgen for dialysis catheters
use_CARE_dialisys_catheters <- TRUE

zorgDefinitieCodes_dialisys <- c('DZhdKath','DZhdKathO')
attr_location_dialysis <- c("DZhdKathP")
attr_bandage_obs_dialysis <- c("DZhdKathOit")
attr_flush_lock_dialysis <- c("DZhdKathCH")
attr_catheter_type_dialysis <- c("DZhdKathT")
attr_aspiration_dialysis <- c("DZamALK", "DZamELK")

permanent_dialysis_catheters <- c("Permanente niet getunnelde hemodialysekatheter",
                                  "Permanente getunnelde hemodialysekatheter")
temporary_dialysis_catheters <- c("Tijdelijke hemodialysekatheter")

# use ACTA for dialysis catheters
use_ACTA_dialysis_catheters <- FALSE

acta_descriptions_dialysis <- c("diepe veneuze katheter dialysekatheter",
                                "intermittente acute dialyse via dialysekatheter op izv",
                                "plaatsen van getunnelde dialysekatheters",
                                "verwijderen dialyse katheter",
                                "plaatsen hickman dialyse katheter",
                                "verwijderen hickman dialyse katheter")

acta_descriptions_dialysis_start <- c("diepe veneuze katheter dialysekatheter",
                                      "intermittente acute dialyse via dialysekatheter op izv",
                                      "plaatsen van getunnelde dialysekatheters",
                                      "plaatsen hickman dialyse katheter")

acta_descriptions_dialysis_end <- c("verwijderen dialyse katheter",
                                    "verwijderen hickman dialyse katheter")

# maps catheters from PDMS_insertion_points to PMDS_catheters
dict_catheters_pdms <- tibble(catheter_insert = "Arteriele catheter1", catheter = "Arteriële. cath. 1")
dict_catheters_pdms <- dict_catheters_pdms %>% 
  add_row(catheter_insert = "Arteriele catheter2", catheter = "Arteriële. cath. 2") %>% 
  add_row(catheter_insert = "Coolgard Katheter", catheter = "Coolgard katheter") %>% 
  add_row(catheter_insert = "CVD Katheter1", catheter = "Centraal veneuze cath 1") %>% 
  add_row(catheter_insert = "CVD Katheter2", catheter = "Centraal veneuze cath 2") %>% 
  add_row(catheter_insert = "dialysekatheter", catheter = "Dialysecatheter") %>% 
  add_row(catheter_insert = "Hickman katheter", catheter = "Hickman-catheter") %>% 
  add_row(catheter_insert = "Introducer katheter", catheter = "Introducer (zonder Pacing Cath)") %>% 
  add_row(catheter_insert = "LAD katheter", catheter = "Linker atrium cath") %>% 
  add_row(catheter_insert = "Midline katheter", catheter = "Perifere Midline katheter") %>% 
  add_row(catheter_insert = "opschuifkatheter", catheter = "Opschuif catheter (PICC katheter)") %>% 
  add_row(catheter_insert = "PAC", catheter = "Port-a-cath") %>% 
  add_row(catheter_insert = "Pacing Katheter", catheter = "Pacing catheter") %>% 
  add_row(catheter_insert = "perifere katheter1", catheter = "Perifere cath 1") %>% 
  add_row(catheter_insert = "perifere katheter2", catheter = "Perifere cath 2") %>% 
  add_row(catheter_insert = "perifere katheter3", catheter = "Perifere cath 3") %>% 
  add_row(catheter_insert = "perifere katheter4", catheter = "Perifere cath 4") %>% 
  add_row(catheter_insert = "PICCO katheter", catheter = "PiCCO catheter") %>% 
  add_row(catheter_insert = "Pulmonalis katheter", catheter = "Arteria Pulmonalis Catheter") %>% 
  add_row(catheter_insert = "RIS katheter", catheter = "RIS-catheter") %>% 
  add_row(catheter_insert = "RV katheter", catheter = "Rechterventrikel catheter") %>% 
  add_row(catheter_insert = "Swan Ganz katheter", catheter = "Swan Ganz Catheter")

# dictionary for catheter types - PDMS catheters are the first ones
dict_catheters <- tibble(catheter_orig = "Q_Centraal veneuze kath 1", catheter_new = "CVC", is_central = TRUE)
dict_catheters <- dict_catheters %>% 
  add_row(catheter_orig = "Q_Centraal veneuze kath 2", catheter_new = "CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Centraal veneuze cath 1", catheter_new = "CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Centraal veneuze cath 2", catheter_new = "CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Port-a-cath", catheter_new = "Port-a-cath", is_central = TRUE) %>% 
  add_row(catheter_orig = "PICCO katheter", catheter_new = "PiCCO catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Hickman katheter", catheter_new = "Tunneled CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Hickman-catheter", catheter_new = "Tunneled CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "RIS katheter", catheter_new = "RIS catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "RIS-catheter", catheter_new = "RIS catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_RIS-katheter", catheter_new = "RIS catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Opschuif catheter (PICC katheter)", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Q_Opschuif katheter (PICC katheter)", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "PiCCO catheter", catheter_new = "PiCCO catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_PiCCO katheter", catheter_new = "PiCCO catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Dialysecatheter", catheter_new = "Dialysis CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Dialysekatheter 2-lumen", catheter_new = "Dialysis CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Dialysekatheter 3-lumen", catheter_new = "Dialysis CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Q_Dialysekatheter", catheter_new = "Dialysis CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Q_Swan Ganz katheter", catheter_new = "Swan Ganz", is_central = FALSE) %>% 
  add_row(catheter_orig = "Swan Ganz Catheter", catheter_new = "Swan Ganz", is_central = FALSE) %>% 
  add_row(catheter_orig = "Swan Ganz katheter", catheter_new = "Swan Ganz", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_Coolgard katheter", catheter_new = "Coolgard", is_central = FALSE) %>% 
  add_row(catheter_orig = "Coolgard katheter", catheter_new = "Coolgard", is_central = FALSE) %>% 
  add_row(catheter_orig = "Coolgard catheter", catheter_new = "Coolgard", is_central = FALSE) %>% 
  add_row(catheter_orig = "Coolgard Katheter", catheter_new = "Coolgard", is_central = FALSE) %>% 
  add_row(catheter_orig = "Arteriële. cath. 1", catheter_new = "Arterial catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Arteriële. cath. 2", catheter_new = "Arterial catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_Arteriële. kath. 1", catheter_new = "Arterial catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_Arteriële. kath. 2", catheter_new = "Arterial catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Perifere cath 1", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Perifere cath 2", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Perifere cath 3", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Perifere cath 4", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Q_Perifere kath 1", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Q_Perifere kath 2", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Q_Perifere kath 3", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Q_Perifere kath 4", catheter_new = "Peripheral catheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Introducer (zonder Pacing Cath)", catheter_new = "NOT-A-CATHETER", is_central = FALSE) %>% 
  add_row(catheter_orig = "Linker atrium cath", catheter_new = "Linker atrium cath", is_central = FALSE) %>% 
  add_row(catheter_orig = "Pacing catheter", catheter_new = "NOT-A-CATHETER", is_central = FALSE) %>% 
  add_row(catheter_orig = "Perifere Midline katheter", catheter_new = "Perifere Midline katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_Introducer (zonder Pacing kath)", catheter_new = "NOT-A-CATHETER", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_Perifere Midline katheter", catheter_new = "Perifere Midline katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_Pacing katheter", catheter_new = "NOT-A-CATHETER", is_central = FALSE) %>% 
  add_row(catheter_orig = "Q_Arteria Pulmonalis katheter", catheter_new = "Arteria Pulmonalis katheter", is_central = FALSE) %>% # only 1
  add_row(catheter_orig = "Arteria Pulmonalis Catheter", catheter_new = "Arteria Pulmonalis katheter", is_central = FALSE) %>%
  # from here on CARE catheters
  add_row(catheter_orig = "Centraal verneuze katheter", catheter_new = "CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Centraal veneuze katheter", catheter_new = "CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Centraal verneuze katheter 2", catheter_new = "CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Centraal veneuze katheter 2", catheter_new = "CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Centraal veneuse katheter PCIA pijnpomp", catheter_new = "pain_catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "PICC", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Klep PICC", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Open PICC", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Getunnelde verneuze katheter", catheter_new = "Tunneled CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Getunnelde veneuze katheter", catheter_new = "Tunneled CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Poortkatheter veneus", catheter_new = "Port-a-cath", is_central = TRUE) %>% 
  add_row(catheter_orig = "Poortkatheter intrathecaal", catheter_new = "Port-a-cath-intrathecal", is_central = FALSE) %>% 
  add_row(catheter_orig = "Midline katheter", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Klep Midline Katheter", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Open Midline Katheter", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Dialyse katheter", catheter_new = "Dialysis CVC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Perifeer veneuze katheter 1", catheter_new = "Peripheral catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Perifeer veneuze katheter 2", catheter_new = "Peripheral catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Perifeer veneuze katheter 3", catheter_new = "Peripheral catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Intra-arteriële katheter", catheter_new = "Arterial catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Intra-arteriele katheter 2", catheter_new = "Arterial catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Epidurale katheter", catheter_new = "Epidurale katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Intra-articulaire katheter", catheter_new = "Intra-articulaire katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Intra pleurale katheter", catheter_new = "Intra pleurale katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Intraperitoneale katheter", catheter_new = "Intraperitoneale katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Intrathecale katheter", catheter_new = "Intrathecale katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Perifeer veneus slot 1", catheter_new = "Peripheral catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Perifeer veneus slot 2", catheter_new = "Peripheral catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Plexus katheter", catheter_new = "Plexus katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Subcutaan slot 1", catheter_new = "Subcutaan slot", is_central = FALSE) %>% 
  add_row(catheter_orig = "Subcutaan slot 2", catheter_new = "Subcutaan slot", is_central = FALSE) %>% 
  add_row(catheter_orig = "Subcutane katheter 1", catheter_new = "Subcutane katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Subcutane katheter 2", catheter_new = "Subcutane katheter", is_central = FALSE) %>%  
  add_row(catheter_orig = "Intra-osseuze katheter", catheter_new = "Intra-osseuze katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Klep Midline", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Open Midline", catheter_new = "PICC", is_central = TRUE) %>% 
  add_row(catheter_orig = "Subpleurale katheter", catheter_new = "Subpleurale katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Arteriele katheter 3", catheter_new = "Arterial catheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Spinale katheter", catheter_new = "Spinale katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Subcutane katheter 3", catheter_new = "Subcutane katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Perifere zenuw katheter", catheter_new = "Perifere zenuw katheter", is_central = FALSE) %>% 
  add_row(catheter_orig = "Spinale katheter", catheter_new = "Spinale katheter", is_central = FALSE)

# dictionary for catheter location - PDMS catheters are first ones
dict_location <- tibble(location_orig = "vena jugularis interna rechts", location_new = "Neck right") 
dict_location <- dict_location %>% 
  add_row(location_orig = "vena jugularis interna links", location_new = "Neck left") %>% 
  add_row(location_orig = "vena jugularis externa rechts", location_new = "Neck right") %>%
  add_row(location_orig = "vena jugularis externa links", location_new = "Neck left") %>% 
  add_row(location_orig = "vena jugularis interna", location_new = "Neck right") %>% # (seems to be right side)
  add_row(location_orig = "vene jugularis interna rechts", location_new = "Neck right") %>% 
  add_row(location_orig = "onderarm re (perifeer inf)", location_new = "Forearm right") %>% 
  add_row(location_orig = "onderarm li (perifeer inf)", location_new = "Forearm left") %>% 
  add_row(location_orig = "onderarm re", location_new = "Forearm right") %>% 
  add_row(location_orig = "onderarm li", location_new = "Forearm left") %>% 
  add_row(location_orig = "linker atrium", location_new = "Left atrium") %>% 
  add_row(location_orig = "rechter atrium", location_new = "Right atrium") %>% 
  add_row(location_orig = "arteria radialis", location_new = "Forearm") %>% 
  add_row(location_orig = "arteria radialis rechts", location_new = "Forearm right") %>% 
  add_row(location_orig = "arteria radialis links", location_new = "Forearm left") %>% 
  add_row(location_orig = "vena subclavia rechts", location_new = "Collarbone right") %>% 
  add_row(location_orig = "vena subclavia links", location_new = "Collarbone left") %>% 
  add_row(location_orig = "vene subclavia rechts", location_new = "Collarbone right") %>%
  add_row(location_orig = "vene femoralis rechts", location_new = "Groin right") %>% # dialysis specific, few
  add_row(location_orig = "vene femoralis links", location_new = "Groin left") %>% # dialysis specific, few
  add_row(location_orig = "vena femoralis rechts", location_new = "Groin right") %>% 
  add_row(location_orig = "vena femoralis links", location_new = "Groin left") %>% 
  add_row(location_orig = "vena brachialis li (perif.inf.)", location_new = "Upper arm left") %>% # in fact, this can be Arm / Elleboog / Onderarm / Hand 
  add_row(location_orig = "vena brachialis re (perif.inf.)", location_new = "Upper arm right") %>% # in fact, this can be Arm / Elleboog / Onderarm / Hand 
  add_row(location_orig = "vena brachialis rechts", location_new = "Upper arm right") %>% 
  add_row(location_orig = "vena brachialis links", location_new = "Upper arm left") %>% 
  add_row(location_orig = "vena dorsalis pedis re", location_new = "Foot right") %>% 
  add_row(location_orig = "vena dorsalis pedis li", location_new = "Foot left") %>% 
  add_row(location_orig = "vena umblicalis (navel)", location_new = "Navel") %>% 
  add_row(location_orig = "arteria axillaris links", location_new = "Axillary left") %>% 
  add_row(location_orig = "arteria axillaris rechts", location_new = "Axillary right") %>% 
  add_row(location_orig = "arteria brachialis rechts", location_new = "Elbow right") %>% 
  add_row(location_orig = "arteria brachialis links", location_new = "Elbow left") %>% 
  add_row(location_orig = "arteria femoralis rechts", location_new = "Groin right") %>% 
  add_row(location_orig = "arteria femoralis links", location_new = "Groin left") %>% 
  add_row(location_orig = "art. femoralis rechts", location_new = "Groin right") %>% 
  add_row(location_orig = "art. femoralis links", location_new = "Groin left") %>% 
  add_row(location_orig = "arteria dorsalis pedis links", location_new = "Foot left") %>% 
  add_row(location_orig = "arteria dorsalis pedis rechts", location_new = "Foot right") %>% 
  add_row(location_orig = "arteria umblicalis (navel)", location_new = "Navel") %>% 
  add_row(location_orig = "arteria tibialis posterior rechts", location_new = "Foot right") %>% 
  add_row(location_orig = "arteria tibialis posterior links", location_new = "Foot left") %>% 
  add_row(location_orig = "arteria subclavia rechts", location_new = "Collarbone right") %>% # very few
  add_row(location_orig = "arteria subclavia links", location_new = "Collarbone left") %>% # very few
  add_row(location_orig = "arteria ulnaris rechts", location_new = "Hand right") %>% 
  add_row(location_orig = "arteria ulnaris links", location_new = "Hand left") %>% 
  add_row(location_orig = "handrug re", location_new = "Hand right") %>% 
  add_row(location_orig = "handrug li", location_new = "Hand left") %>% 
  add_row(location_orig = "niet aangeprikt", location_new = "niet aangeprikt") %>% # not mapped, port-a-cath
  add_row(location_orig = "aangeprikt", location_new = "aangeprikt") %>% # can be anything in care
  add_row(location_orig = "perifere vene hoofd", location_new = "Head") %>% # can be Hoofd right / left / ....
  add_row(location_orig = "trans-thoracaal", location_new = "trans-thoracaal") %>% # not mapped, ICU specific, very few
  add_row(location_orig = "intra-osseus", location_new = "intra-osseus") %>% # not mapped, ICU specific, very few
  add_row(location_orig = "linker ventrikel", location_new = "linker ventrikel") %>% # not mapped, ICU specific, very few
  add_row(location_orig = "via bestaande shunt", location_new = "via bestaande shunt") %>% # not mapped, ICU specific, very few
  # CARE locations
  add_row(location_orig = "_Bovenste ledematen", location_new = "Arm") %>% 
  add_row(location_orig = "_Onderste ledematen", location_new = "Lower leg") %>% 
  add_row(location_orig = "Arm links", location_new = "Arm left") %>% 
  add_row(location_orig = "Arm rechts", location_new = "Arm right") %>% 
  add_row(location_orig = "_Hoofd - Hals", location_new = "Head neck") %>% 
  add_row(location_orig = "Been links", location_new = "Leg left") %>% 
  add_row(location_orig = "Been rechts", location_new = "Leg right") %>% 
  add_row(location_orig = "Bil links", location_new = "Bil left") %>% 
  add_row(location_orig = "Bil rechts", location_new = "Bil right") %>% 
  add_row(location_orig = "Bovenarm links", location_new = "Upper arm left") %>% 
  add_row(location_orig = "Bovenarm rechts", location_new = "Upper arm right") %>% 
  add_row(location_orig = "Bovenbeen links", location_new = "Groin left") %>% 
  add_row(location_orig = "Bovenbeen rechts", location_new = "Groin right") %>% 
  add_row(location_orig = "Buik", location_new = "Navel") %>% 
  add_row(location_orig = "Elleboog links", location_new = "Elbow left") %>% 
  add_row(location_orig = "Elleboog rechts", location_new = "Elbow right") %>% 
  add_row(location_orig = "Flank buik links", location_new = "Flank buik left") %>% 
  add_row(location_orig = "Flank buik rechts", location_new = "Flank buik right") %>% 
  add_row(location_orig = "Halsstreek links", location_new = "Neck left") %>% 
  add_row(location_orig = "Halsstreek rechts", location_new = "Neck right") %>% 
  add_row(location_orig = "Hand links", location_new = "Hand left") %>% 
  add_row(location_orig = "Hand rechts", location_new = "Hand right") %>% 
  add_row(location_orig = "Hoofd achteraan", location_new = "Head back") %>% 
  add_row(location_orig = "Hoofd links", location_new = "Head left") %>% 
  add_row(location_orig = "Hoofd rechts", location_new = "Head right") %>% 
  add_row(location_orig = "Hoofd vooraan", location_new = "Head vooraan") %>% 
  add_row(location_orig = "Knieholte links", location_new = "Knee left") %>% 
  add_row(location_orig = "Knieholte rechts", location_new = "Knee right") %>% 
  add_row(location_orig = "Lies li", location_new = "Groin left") %>% 
  add_row(location_orig = "Lies links", location_new = "Groin left") %>% 
  add_row(location_orig = "Lies re", location_new = "Groin right") %>% 
  add_row(location_orig = "Lies rechts", location_new = "Groin right") %>% 
  add_row(location_orig = "Navel", location_new = "Navel") %>% 
  add_row(location_orig = "Oksel links", location_new = "Axillary left") %>% 
  add_row(location_orig = "Oksel rechts", location_new = "Axillary right") %>% 
  add_row(location_orig = "Onderarm links", location_new = "Forearm left") %>% 
  add_row(location_orig = "Onderarm rechts", location_new = "Forearm right") %>% 
  add_row(location_orig = "Onderbeen links", location_new = "Lower leg left") %>% 
  add_row(location_orig = "Onderbeen rechts", location_new = "Lower leg right") %>% 
  add_row(location_orig = "Pols links", location_new = "Hand left") %>% 
  add_row(location_orig = "Pols rechts", location_new = "Hand right") %>% 
  add_row(location_orig = "Rug", location_new = "Back") %>% 
  add_row(location_orig = "Subclavia links", location_new = "Collarbone left") %>% 
  add_row(location_orig = "Subclavia rechts", location_new = "Collarbone right") %>% 
  add_row(location_orig = "Thoracale shunt", location_new = "Thoracale shunt") %>% 
  add_row(location_orig = "Thorax", location_new = "Thorax") %>% 
  add_row(location_orig = "Thorax links", location_new = "Thorax left") %>% 
  add_row(location_orig = "Thorax rechts", location_new = "Thorax right") %>% 
  add_row(location_orig = "Tibia links distaal", location_new = "Lower leg left") %>% 
  add_row(location_orig = "Tibia links proximaal", location_new = "Lower leg left") %>% 
  add_row(location_orig = "Voet links", location_new = "Foot left") %>% 
  add_row(location_orig = "Voet rechts", location_new = "Foot right") %>% 
  # dialysis catheters from CARE hoofding Dialyse zorgen
  add_row(location_orig = "Vena femoralis links", location_new = "Groin left") %>% 
  add_row(location_orig = "Vena femoralis rechts", location_new = "Groin right") %>% 
  add_row(location_orig = "Vena jugularis links", location_new = "Neck left") %>% 
  add_row(location_orig = "Vena jugularis rechts", location_new = "Neck right") %>% 
  add_row(location_orig = "Vena subclavia links", location_new = "Collarbone left") %>% 
  add_row(location_orig = "Vena subclavia rechts", location_new = "Collarbone right") %>% 
  distinct()

# dictionary to further collapse location_new (after removing left|right) to a more condensed location_new
dict_location_2 <- tibble(location_orig = "Neck", location_new_2 = "Neck") %>% 
  add_row(location_orig = "Collarbone", location_new_2 = "Collarbone") %>% 
  add_row(location_orig = "Arm", location_new_2 = "Arm") %>% 
  add_row(location_orig = "Groin", location_new_2 = "Groin") %>% 
  add_row(location_orig = "Navel", location_new_2 = "Navel") %>% 
  add_row(location_orig = "Upper arm", location_new_2 = "Arm") %>% 
  add_row(location_orig = "Foot", location_new_2 = "Other") %>% 
  add_row(location_orig = "Hand", location_new_2 = "Arm") %>% 
  add_row(location_orig = "Leg", location_new_2 = "Other") %>% 
  add_row(location_orig = "Elbow", location_new_2 = "Arm") %>% 
  add_row(location_orig = "Head", location_new_2 = "Other") %>% 
  add_row(location_orig = "via bestaande shunt", location_new_2 = "Other") %>% 
  add_row(location_orig = "Thoracale shunt", location_new_2 = "Other") %>% 
  add_row(location_orig = "Axillary", location_new_2 = "Other") %>% 
  # normaly from here on should not be CLs
  add_row(location_orig = "Forearm", location_new_2 = "Arm") %>% 
  add_row(location_orig = "Left atrium", location_new_2 = "Other") %>% 
  add_row(location_orig = "Right atrium", location_new_2 = "Other") %>% 
  add_row(location_orig = "niet aangeprikt", location_new_2 = "Other") %>% 
  add_row(location_orig = "aangeprikt", location_new_2 = "Other") %>% 
  add_row(location_orig = "trans-thoracaal", location_new_2 = "Other") %>% 
  add_row(location_orig = "intra-osseus", location_new_2 = "Other") %>% 
  add_row(location_orig = "linker ventrikel", location_new_2 = "Other") %>% 
  add_row(location_orig = "Lower leg", location_new_2 = "Other") %>% 
  add_row(location_orig = "Head neck", location_new_2 = "Other") %>% 
  add_row(location_orig = "Bil", location_new_2 = "Other") %>% 
  add_row(location_orig = "Flank buik", location_new_2 = "Other") %>% 
  add_row(location_orig = "Head back", location_new_2 = "Other") %>% 
  add_row(location_orig = "Head vooraan", location_new_2 = "Other") %>% 
  add_row(location_orig = "Knee", location_new_2 = "Other") %>% 
  add_row(location_orig = "Back", location_new_2 = "Other") %>% 
  add_row(location_orig = "Thorax", location_new_2 = "Other") %>% 
  add_row(location_orig = "dialysis_unknown", location_new_2 = "dialysis_unknown") 

# used to fill in missing catheter type for the specific zorgDefinitieCode TPobPKV
bandage_code_port_a_cath <- "TPobPKV" 
catheter_type_port_a_cath <- "Port-a-cath"

# map Dutch to English (used for values of the catheter attributes)
# we collapse Bloody and Moist together because they are both wet (risk for bacteria growth)
# we put Pus with other because it is extremely sparse
dict_bandage_obs <- tibble(Dutch = "Rood", English = "Red")
dict_bandage_obs <- dict_bandage_obs %>% 
  add_row(Dutch = "Haematoom", English = "Other_Hema_Pus_Loose_Necro") %>% 
  add_row(Dutch = "Korstvorming", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "Zwelling", English = "Red") %>% 
  add_row(Dutch = "Etter", English = "Other_Hema_Pus_Loose_Necro") %>% 
  add_row(Dutch = "Blaarvorming", English = "Other_Hema_Pus_Loose_Necro") %>% 
  add_row(Dutch = "Normaal", English = "Normal") %>% 
  add_row(Dutch = "Los", English = "Other_Hema_Pus_Loose_Necro") %>% 
  add_row(Dutch = "Bloederig Vochtig", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "Etterig", English = "Other_Hema_Pus_Loose_Necro") %>% 
  add_row(Dutch = "Sereus", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "OK", English = "Normal") %>% 
  add_row(Dutch = "bloederig", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "lekt", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "rood", English = "Red") %>% 
  add_row(Dutch = "licht rood", English = "Red") %>% 
  add_row(Dutch = "etterig", English = "Other_Hema_Pus_Loose_Necro") %>% 
  add_row(Dutch = "zwart (necrose)", English = "Other_Hema_Pus_Loose_Necro") %>%  
  add_row(Dutch = "Vochtig", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "oud bloed", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "klein bloedvlekje", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "roodheid", English = "Red") %>% 
  add_row(Dutch = "bloedvlekje", English = "Bloody_or_Moist") %>% 
  add_row(Dutch = "licht bebloed", English = "Bloody_or_Moist") %>% 
  # dialysis catheters
  add_row(Dutch = "Roodheid", English = "Red") %>% 
  add_row(Dutch = "Sereus vocht", English = "Bloody_or_Moist") %>% 
  distinct()

# map care item attributes from attribute code to human understandable name (in English)
dict_care_attributes <- tibble(Dutch = "TPtpT", English = "catheter_type") # Type toegangspoort
dict_care_attributes <- dict_care_attributes %>% 
  add_row(Dutch = "TPtpP", English = "location") %>% # Plaats toegangspoort
  add_row(Dutch = "TPtpK", English = "placement") %>% # Katheter
  add_row(Dutch = "TPtpL", English = "lumens") %>% # Aantal lumen toegangspoort
  add_row(Dutch = "TPtpPD", English = "placement_date") %>% # Plaatsingsdatum kath
  add_row(Dutch = "TPtpV", English = "bandage_change") %>% # Verband insteekpunt (Plaatsen / Vervangen)
  add_row(Dutch = "TPtpBZ", English = "tube_change") %>% # Bijkomende zorgen toegangspoort
  add_row(Dutch = "TPobGVKcvkPICCvi", English = "bandage_obs") %>% # Verband insteekplaats
  add_row(Dutch = "TPobPKVva", English = "bandage_obs") %>% # Verband aanprikplaats
  add_row(Dutch = "TPobGVKcvkPICCip", English = "insert_point_obs") %>% # Insteekplaats
  add_row(Dutch = "TPobPKVap", English = "insert_point_obs") %>% # Aanprikplaats
  add_row(Dutch = "TPobGVKcvkPICCal", English = "lumens") %>% # Aantal lumen
  add_row(Dutch = "TPobGVKcvkPICCtk", English = "catheter_type") %>% # Type Katheter
  add_row(Dutch = "TPobGVKcvkPICCpk", English = "location") %>%  # Plaats katheter
  add_row(Dutch = "TPtpG", English = "needle_length") %>%  # Poortkatheter-naald - lengte Grippernaald of Hubberpuntnaald
  add_row(Dutch = "TPtpSH", English = "flush_lock") 

dict_bandage_type <- tibble(bandage_orig = "Anti allergisch gaasverband", bandage_new = "polyurethane") # CARE
dict_bandage_type <- dict_bandage_type %>% 
  add_row(bandage_orig = "Gaasverband", bandage_new = "gauze") %>% # CARE
  add_row(bandage_orig = "Polyurethaan-CHG verband", bandage_new = "polyurethane") %>% # CARE
  add_row(bandage_orig = "Polyurethaanverband", bandage_new = "polyurethane") %>% # CARE
  add_row(bandage_orig = "Siliconenverband", bandage_new = "gauze") %>% # CARE
  # silicone is like gauze wrt stickiness (needs more changes, closer risk) and like polyutheane wrt transparency (meeting Christel 16/12/2021)
  add_row(bandage_orig = "Toezicht (gaasverband dichtgelaten)", bandage_new = "gauze") %>% # PDMS
  add_row(bandage_orig = "Verzorging:gaasverband en Chloorhexidine 0,5%", bandage_new = "gauze") %>% # PDMS
  add_row(bandage_orig = "Toezicht (PU-verband dichtgelaten)", bandage_new = "polyurethane") %>% # PDMS
  add_row(bandage_orig = "Verzorging: PU-verband en Chloorhexidine 0,5%", bandage_new = "polyurethane") %>% # PDMS
  add_row(bandage_orig = "Verzorging:PU-verband en Chloorhexidine 0,5%", bandage_new = "polyurethane") # PDMS

dict_tube_change <- tibble(orig = "Vervangen infuusleiding", new = "tube_change") # CARE
dict_tube_change <- dict_tube_change %>% 
  add_row(orig = "Vervangen TPN leiding", new = "tube_change") %>% 
  add_row(orig = "Korte verlengleiding vervangen", new = "tube_change") %>% 
  add_row(orig = "Vervangen Vetemulsie leiding", new = "tube_change") %>% 
  add_row(orig = "Vervangen Cyclo-leiding", new = "tube_change") %>% 
  add_row(orig = "Drukzak +clave art/cvd", new = "tube_change") %>% #PDMS
  add_row(orig = "Drukzal + CVD-leiding (+evt clave)", new = "tube_change") %>% 
  add_row(orig = "IV leidingen (Niet-vethoudende infusen)", new = "tube_change") %>% 
  add_row(orig = "IV leidingen (vethoudende infusen)", new = "tube_change")

# map placement (TPtpK) to the location of catheter placement
dict_catheter_placement <- tibble(orig = "Geplaatst in OKa", new = "OR") %>% 
  add_row(orig = "Geplaatst op VE", new = "bedside")

# dictionary for attribute TPtpSH - Spoelen en/of locken - Locken = Het definitief afsluiten van een katheter kan met Taurolidine, Heparine of Citraat.
dict_flush_lock <- tibble(orig = "Citraat spoeling lumen", new = "lock") %>% 
  add_row(orig = "Spoelen lumen", new = "flush") %>% 
  add_row(orig = "Spoelen lumen 1", new = "flush") %>% 
  add_row(orig = "Spoelen lumen 2", new = "flush") %>% 
  add_row(orig = "Spoelen lumen 3", new = "flush") %>% 
  add_row(orig = "Spoelen lumen 4", new = "flush") %>% # DNE, added for completeness
  add_row(orig = "Spoelen lumen 5", new = "flush") %>% 
  add_row(orig = "Spoelen TPN lumen", new = "flush") %>% 
  add_row(orig = "Spoelen lume", new = "flush") %>% # DNE, added for completeness
  add_row(orig = "Spoelen lume 1", new = "flush") %>% # DNE, added for completeness
  add_row(orig = "Spoelen lume 2", new = "flush") %>% # DNE, added for completeness
  add_row(orig = "Spoelen lume 3", new = "flush") %>% # DNE, added for completeness
  add_row(orig = "Spoelen lume 4", new = "flush") %>% 
  add_row(orig = "Spoelen lume 5", new = "flush") %>% # DNE, added for completeness
  add_row(orig = "Heparinisatie lumen", new = "lock") %>% 
  add_row(orig = "Heparinisatie lumen 1", new = "lock") %>% 
  add_row(orig = "Heparinisatie lumen 2", new = "lock") %>% 
  add_row(orig = "Heparinisatie lumen 3", new = "lock") %>% 
  add_row(orig = "Heparinisatie lumen 4", new = "lock") %>% 
  add_row(orig = "Heparinisatie lumen 5", new = "lock") %>% 
  add_row(orig = "Heparinisatie lume", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Heparinisatie lume 1", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Heparinisatie lume 2", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Heparinisatie lume 3", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Heparinisatie lume 4", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Heparinisatie lume 5", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Taurolock lumen", new = "lock") %>% 
  add_row(orig = "Taurolock lumen 1", new = "lock") %>% 
  add_row(orig = "Taurolock lumen 2", new = "lock") %>% 
  add_row(orig = "Taurolock lumen 3", new = "lock") %>% 
  add_row(orig = "Taurolock lumen 4", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Taurolock lumen 5", new = "lock") %>% 
  add_row(orig = "Taurolock lume", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Taurolock lume 1", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Taurolock lume 2", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Taurolock lume 3", new = "lock") %>% # DNE, added for completeness
  add_row(orig = "Taurolock lume 4", new = "lock") %>% 
  add_row(orig = "Taurolock lume 5", new = "lock") %>% # DNE, added for completeness
  # PDMS
  add_row(orig = "Act Heparineslot", new = "lock") %>% # these come from pdmsParamText
  add_row(orig = "Activiteit Heparineslot", new = "lock") %>% 
  add_row(orig = "Infuusleiding spoelen", new = "flush") %>% # these come from pdmsParamTaken
  add_row(orig = "Midline katheter spoelen", new = "flush") %>% 
  add_row(orig = "PICC katheter spoelen", new = "flush") %>% 
  add_row(orig = "Spoelen getunnelde katheter", new = "flush") %>% 
  # dialysis catheters
  add_row(orig = "Citraatspoeling lumen 1", new = "lock") %>% 
  add_row(orig = "Citraatspoeling lumen 2", new = "lock") %>% 
  add_row(orig = "Heparinisatie lumen 1", new = "lock") %>% 
  add_row(orig = "Heparinisatie lumen 2", new = "lock") %>% 
  add_row(orig = "Taurolock", new = "lock") %>% 
  distinct()

# https://www.uzleuven.be/nl/katheter-injectie-en-aspiratie-classificatie-cinas#wat-is-cinas
# first part isfrom ZorgDef TPobGVKcvkPICC - Observatie CVK - GVK - PICC
dict_CINAS_attributes <- tibble(orig = "TPobGVKcvkPICCil1", new = "infusion") %>% 
  add_row(orig = "TPobGVKcvkPICCil2", new = "infusion") %>% 
  add_row(orig = "TPobGVKcvkPICCil3", new = "infusion") %>% 
  add_row(orig = "TPobGVKcvkPICCil4", new = "infusion") %>% 
  add_row(orig = "TPobGVKcvkPICCil5", new = "infusion") %>% 
  add_row(orig = "TPobGVKcvkPICCbl1", new = "aspiration") %>% 
  add_row(orig = "TPobGVKcvkPICCbl2", new = "aspiration") %>% 
  add_row(orig = "TPobGVKcvkPICCbl3", new = "aspiration") %>% 
  add_row(orig = "TPobGVKcvkPICCbl4", new = "aspiration") %>% 
  add_row(orig = "TPobGVKcvkPICCbl5", new = "aspiration") %>% 
  # ZorgDef TPobPKV	Observatie PKV (Poortkatheter veneus)
  add_row(orig = "TPobPKVi", new = "infusion") %>% 
  add_row(orig = "TPobPKVb", new = "aspiration") %>% 
  # ZorgDef TPaPKV	Resultaat na Actosolv toediening PKV
  add_row(orig = "TPaPKVi", new = "infusion_acto") %>% 
  add_row(orig = "TPaPKVb", new = "aspiration_acto") %>% 
  # ZorgDef TPaGVKcvkPICC	Resultaat na Actosolv toediening CVK - GVK - PICC
  add_row(orig = "TPaGVKcvkPICCI1", new = "infusion_acto") %>% 
  add_row(orig = "TPaGVKcvkPICCI2", new = "infusion_acto") %>% 
  add_row(orig = "TPaGVKcvkPICCI3", new = "infusion_acto") %>% 
  add_row(orig = "TPaGVKcvkPICCB1", new = "aspiration_acto") %>% 
  add_row(orig = "TPaGVKcvkPICCB2", new = "aspiration_acto") %>% 
  add_row(orig = "TPaGVKcvkPICCB3", new = "aspiration_acto") 

dict_CINAS_values <- tibble(orig = "Moeilijk", new = "difficult") %>% 
  add_row(orig = "Moelijk", new = "difficult") %>% 
  add_row(orig = "Onmogelijk (< 3 ml)", new = "impossible") %>% 
  add_row(orig = "Moeilijk (> 3 ml)", new = "difficult") %>% 
  add_row(orig = "Normaal", new = "normal") %>% 
  add_row(orig = "Onmogelijk", new = "impossible") %>% 
  # dialysis CARE values
  add_row(orig = "Aspireren moeilijk", new = "difficult") %>% 
  add_row(orig = "Aspireren normaal", new = "normal") %>% 
  add_row(orig = "Aspireren onmogelijk", new = "impossible")

# OUTCOME CONFIG
# --------------

# dictionary for discharge type: Discharge or Death or Censored
dict_discharge_type <- tibble(type_orig = "N", type_new = "Discharge")
dict_discharge_type <- dict_discharge_type %>%
  add_row(type_orig = "O", type_new = "Death") %>%
  add_row(type_orig = "OMOD", type_new = "Death") %>%
  add_row(type_orig = "TA", type_new = "Discharge") %>%
  add_row(type_orig = "Z", type_new = "Discharge") %>%
  add_row(type_orig = "X", type_new = "Discharge") 

# To identify the moment when a patient starts receiving "palliative care", we always 
# use the transfer to palliative care department. Additionally we can use the first moment 
# when the palliative care department is contacted if the value of this parameter is TRUE. 
use_contact_with_palliative_care <- TRUE

# list of germs from Veerle
table_germs <- read_xlsx("../config/ZHH_CLABSI_lijst_organismen_062020_def.xlsx")
table_MBI_germs <- read_xlsx("../config/master-organism-com-commensals-lists_NHSN_2020.xlsx", 
                             sheet = "MBI Organisms")
table_MBI_germ_mapping <- read_xlsx("../config/germs_MBI_mapping.xlsx")

# identification of blood samples for BSI
samples_blood <- c("bloed aeroob", "bloed anaeroob", "bloed pediatrie")
sample_pediatrics <- c("bloed pediatrie")
# catheter samples are excluded from secondary (secondary to catheter is CR-BSI)
# modermelk and faeces / lies / perineum are screening samples acc. to veerle (should be excluded too)
samples_exclude <- c("bloedbaan-katheter tip", "subcutaan deel katheter", 
                     "moedermelk", "faeces / lies / perineum")
# alos exclude extenise moedermelk (freetext)
extensie_exclude_moedermelk <- "moedermelk"
# for wound samples, some can be at the catheter wound
# these should not be considered secondary to wound because in fact they are CR-BSIs
#wound_sample_type <- c("wondvocht / etter")
table_extensie_catheter <- read_xlsx("../config/extensie_wound_catheter.xlsx")

# time window for secondary BSI, in days (vector: past window, future window)
# Sciensano does not mention the time window, leaves it to clinical judgement
# 17 days = 14 days episode length + 3 days 
time_window_secondary <- c(17, 17)

# consider polybacterial infections as CLABSI?
is_poly_CLABSI <- FALSE

coagulase_negatieve_stafylokok <- c("Staphylococcus auricularis",
                                    "Staphylococcus capitis",
                                    "Staphylococcus caprae",
                                    "Staphylococcus carnosus",
                                    "Staphylococcus chromogenes",
                                    "Staphylococcus cohnii",
                                    "Staphylococcus condimenti",
                                    "Staphylococcus epidermidis",
                                    "Staphylococcus haemolyticus",
                                    "Staphylococcus hominis",
                                    "Staphylococcus hyicus",
                                    "Staphylococcus intermedius",
                                    "Staphylococcus kloosii",
                                    "Staphylococcus lugdunensis",
                                    "Staphylococcus pasteuri",
                                    "Staphylococcus pettenkoferi",
                                    "Staphylococcus pseudintermedius",
                                    "Staphylococcus saccharolyticus",
                                    "Staphylococcus saprophyticus",
                                    "Staphylococcus schleiferi",
                                    "Staphylococcus sciuri",
                                    "Staphylococcus simulans",
                                    "Staphylococcus species",
                                    "Staphylococcus succinus",
                                    "Staphylococcus warneri",
                                    "Staphylococcus xylosus")

use_CLABSI_definition_Veerle <- FALSE

# PM_WHERE CONFIG
# ----------------

# used to exclude the palliative care landmarks
# exclusion is done directly using the eenheid rather than the mapped wards (it seems safer like this)
palliative_care_eenhied <- c(812)

AARDOMSCH_ICU <- "HOSPITALISATIE ITE"
AARDOMSCH_OR <- "OPERATIEKWARTIER"
omschrijving_endoscopy <- "FM. Endoscopie"
omschrijving_IRCC <- "IRCC"

# dictionary for AC_AARDOMSCH
dict_AC_AARDOMSCH <- tibble(type_orig = "BEELDVORMING", type_new = "Imaging")
dict_AC_AARDOMSCH <- dict_AC_AARDOMSCH %>%
  add_row(type_orig = "ANDERE MED-TECH ACTIV", type_new = "Function Measurement") %>%
  add_row(type_orig = "BEVALLINGSKWARTIER", type_new = "Hospitalization") %>%
  add_row(type_orig = "DAGHOSPITALISATIE", type_new = "Hospitalization") %>%
  add_row(type_orig = "DIALYSE", type_new = "Dialysis") %>%
  add_row(type_orig = "HOSPITALISATIE ITE", type_new = "Hospitalization") %>%
  add_row(type_orig = "HOSPITALISATIE", type_new = "Hospitalization") %>%
  add_row(type_orig = "FUNCTIEMETING", type_new = "Function Measurement") %>%
  add_row(type_orig = "LABO", type_new = "Other") %>%
  add_row(type_orig = "NIET ZIEKENHUIS", type_new = "Other") %>%
  add_row(type_orig = "OPERATIEKWARTIER", type_new = "Operation") %>%
  add_row(type_orig = "PARAMEDISCHE STAF", type_new = "Other") %>%
  add_row(type_orig = "RAADPL-FUNCTIEMETING", type_new = "Function Measurement") %>%
  add_row(type_orig = "RAADPLEGING", type_new = "Other") %>%
  add_row(type_orig = "RADIOTHERAPIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "SECRETARIAAT", type_new = "Other") %>%
  add_row(type_orig = "SPOEDGEVALLEN", type_new = "Hospitalization")

# dictionary for AC_OMSCHR (used for PM_ features, used for exploration)
dict_AC_OMSCHR <- tibble(type_orig = "AMBULANT CENTRUM KINDEREN", type_new = "Other") 
dict_AC_OMSCHR <- dict_AC_OMSCHR %>%
  add_row(type_orig = "AMBULANTE ZORG PEL", type_new = "Other") %>% 
  add_row(type_orig = "BESTRALINGSAFDELING", type_new = "Function Measurement") %>% 
  add_row(type_orig = "BEVALLINGSKWARTIER", type_new = "Gynecology") %>% 
  add_row(type_orig = "CARDIALE INTENSIEVE ZORGEN", type_new = "ICU") %>%
  add_row(type_orig = "CENTR.VR PRENAT.&GYNAEC.ECHOGR", type_new = "Function Measurement") %>%
  add_row(type_orig = "CENTR.VR SLAAP-EN WAAKSTOORNIS", type_new = "Function Measurement") %>%
  add_row(type_orig = "CENTRUM VOOR BOTDENSITOMETRIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "CERM", type_new = "Other") %>% # why?
  add_row(type_orig = "CHIRURGISCH DAGCENTR.ST.RAFAEL", type_new = "Operation") %>%
  add_row(type_orig = "DAGHOSP GYNAECO", type_new = "Gynecology") %>%
  add_row(type_orig = "DAGHOSP OOGZIEKTEN", type_new = "Other") %>%
  add_row(type_orig = "DAGZIEKENHUIS CHIRURGIE A", type_new = "Operation") %>%
  add_row(type_orig = "EXPERTCENTRUM KINDERCARDIO", type_new = "Cardiac") %>%
  add_row(type_orig = "FM-RPL UROLOGIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "FM ENDOSCOPIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "FM IG HART- EN VAATZIEKTEN", type_new = "Function Measurement") %>%
  add_row(type_orig = "FM IG PNEUMOLOGIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "FM KLINISCHE NEUROFYSIOLOGIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "FM NUCLEAIRE GENEESKUNDE", type_new = "Function Measurement") %>%
  add_row(type_orig = "HEMODIALYSE VOLWASSENEN", type_new = "Hemodialysis") %>% # are there many here?
  add_row(type_orig = "HOSP-DAGHOSP INTER.GENEESKUNDE", type_new = "Internal Medicine") %>%
  add_row(type_orig = "HOSP  CARDIALE HEELKUNDE B", type_new = "Cardiac") %>%
  add_row(type_orig = "HOSP  IG HART-EN VAATZIEKTEN", type_new = "Cardiac") %>%
  add_row(type_orig = "HOSP GERIATRIE A", type_new = "Geriatrics") %>%
  add_row(type_orig = "HOSP GERIATRIE B", type_new = "Geriatrics") %>%
  add_row(type_orig = "HOSP GERIATRIE E", type_new = "Geriatrics") %>%
  add_row(type_orig = "HOSP HEMATO (ISOLATIE)", type_new = "Hematology") %>%
  add_row(type_orig = "HOSP HEMATOLOGIE", type_new = "Hematology") %>%
  add_row(type_orig = "HOSP HLK ABDOMEN A", type_new = "Abdomen") %>%
  add_row(type_orig = "HOSP HLK ABDOMEN B", type_new = "Abdomen") %>%
  add_row(type_orig = "HOSP HLK NEURO", type_new = "Neuro") %>%
  add_row(type_orig = "HOSP HLK ONCOLOGIE/MBC", type_new = "Oncology") %>%
  add_row(type_orig = "HOSP HLK UROLOGIE A", type_new = "Urology") %>%
  add_row(type_orig = "HOSP HLK UROLOGIE B", type_new = "Urology") %>%
  add_row(type_orig = "HOSP IG ALG INWENDIGE", type_new = "Internal Medicine") %>%
  add_row(type_orig = "HOSP IG H&V KORT VERBLIJF", type_new = "Cardiac") %>%
  add_row(type_orig = "HOSP IG HART- EN VAATZ (HTX)", type_new = "Cardiac") %>%
  add_row(type_orig = "HOSP IG NEFROLOGIE", type_new = "Nephrology") %>%
  add_row(type_orig = "HOSP KORT VERBLIJF", type_new = "Traumatology") %>% # why?
  add_row(type_orig = "HOSP MAAG-DARM-LEVER A", type_new = "Abdomen") %>%
  add_row(type_orig = "HOSP MAAG-DARM-LEVER B", type_new = "Abdomen") %>%
  add_row(type_orig = "HOSP MAAG-DARM-LEVER C", type_new = "Abdomen") %>%
  add_row(type_orig = "HOSP MAAG-DARM-LEVER D", type_new = "Abdomen") %>%
  add_row(type_orig = "HOSP MATERNITEIT B", type_new = "Gynecology") %>%
  add_row(type_orig = "HOSP NEUROLOGIE", type_new = "Neuro") %>%
  add_row(type_orig = "HOSP NIER-LEVER TRANSPLANTATIE", type_new = "Transplant") %>%
  add_row(type_orig = "HOSP NKO - STOMATO", type_new = "ORL") %>%
  add_row(type_orig = "HOSP ONCOLOGIE A", type_new = "Oncology") %>%
  add_row(type_orig = "HOSP ONCOLOGIE B", type_new = "Oncology") %>%
  add_row(type_orig = "HOSP ORTHOPEDIE A", type_new = "Traumatology") %>%
  add_row(type_orig = "HOSP ORTHOPEDIE B", type_new = "Traumatology") %>%
  add_row(type_orig = "HOSP ORTHOPEDIE C", type_new = "Traumatology") %>%
  add_row(type_orig = "HOSP PALLIATIEVE ZORGEN", type_new = "Palliative") %>%
  add_row(type_orig = "HOSP PNEUMOLOGIE A", type_new = "Pneumology") %>%
  add_row(type_orig = "HOSP PNEUMOLOGIE B", type_new = "Pneumology") %>%
  add_row(type_orig = "HOSP PNEUMOLOGIE C", type_new = "Pneumology") %>%
  add_row(type_orig = "HOSP PSY OUDEREN", type_new = "Other") %>%
  add_row(type_orig = "HOSP PSY VOLW", type_new = "Other") %>%
  add_row(type_orig = "HOSP REVALIDATIE A", type_new = "Other") %>% # Revalidation mapped to Other
  add_row(type_orig = "HOSP REVALIDATIE B", type_new = "Other") %>% # Revalidation mapped to Other
  add_row(type_orig = "HOSP STROKE-UNIT/NCH", type_new = "Neuro") %>%
  add_row(type_orig = "HOSP THORAXHEELKUNDE", type_new = "Thoracic Surgery") %>%
  add_row(type_orig = "HOSP TRAUMATO-PREHK", type_new = "Traumatology") %>%
  add_row(type_orig = "HOSP TRAUMATO/PLAST.REC HLK B", type_new = "Traumatology") %>%
  add_row(type_orig = "HOSP VAATHK/ HOSP CAH.", type_new = "Cardiac") %>%
  add_row(type_orig = "HOSP. OOGZIEKTEN", type_new = "Other") %>%
  add_row(type_orig = "HOSPITALISATIE ENDOCRINOLOGIE", type_new = "Endocrinology") %>%
  add_row(type_orig = "HOSPITALISATIE NEONATALE ZORG", type_new = "Neonatology") %>%
  add_row(type_orig = "HOSPITALISATIE VROUW A", type_new = "Gynecology") %>%
  add_row(type_orig = "HOSPITALISATIE VROUW B", type_new = "Gynecology") %>%
  add_row(type_orig = "HOSPITALISATIE VROUW C", type_new = "Gynecology") %>%
  add_row(type_orig = "INFUSIE-EENHEID", type_new = "Function Measurement") %>% 
  add_row(type_orig = "INTENSIEVE GENEESKUNDE A", type_new = "ICU") %>%
  add_row(type_orig = "INTENSIEVE GENEESKUNDE B", type_new = "ICU") %>%
  add_row(type_orig = "INTENSIEVE GENEESKUNDE C", type_new = "ICU") %>%
  add_row(type_orig = "INTENSIEVE GENEESKUNDE D", type_new = "ICU") %>%
  add_row(type_orig = "INTENSIEVE GENEESKUNDE E", type_new = "ICU") %>%
  add_row(type_orig = "INTENSIEVE NEONATALE ZORGEN", type_new = "Neonatology") %>%
  add_row(type_orig = "IRCC", type_new = "Function Measurement") %>%
  add_row(type_orig = "KINDERZH INF/KPN/KTR/KHE/KNC", type_new = "Pediatrics") %>%
  add_row(type_orig = "KINDERZH KLEUTER KIND ADOLESC", type_new = "Pediatrics") %>%
  add_row(type_orig = "KINDERZH ZUIGELINGEN & PEUTERS", type_new = "Pediatrics") %>%
  add_row(type_orig = "LABO CME", type_new = "Function Measurement") %>%
  add_row(type_orig = "LABO FERTILITEITSCENTRUM", type_new = "Function Measurement") %>%
  add_row(type_orig = "LEUVENS ALGOLOGISCH CENTRUM", type_new = "Other") %>%
  add_row(type_orig = "MED.INTENSIEVE GENEESKUNDE A", type_new = "ICU") %>%
  add_row(type_orig = "MED.INTENSIEVE GENEESKUNDE B", type_new = "ICU") %>%
  add_row(type_orig = "MEDISCH AMBULANT CENTRUM", type_new = "Other") %>%
  add_row(type_orig = "MOC COORDINATIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "MORTUARIUM", type_new = "Other") %>%
  add_row(type_orig = "NIERCENTRUM", type_new = "Nephrology") %>%
  add_row(type_orig = "OKA GASTHUISBERG", type_new = "Operation") %>%
  add_row(type_orig = "OKA OOGZIEKTEN", type_new = "Operation") %>%
  add_row(type_orig = "OKA PELLENBERG", type_new = "Operation") %>%
  add_row(type_orig = "ONCOLOGISCH DAGCENTRUM", type_new = "Oncology") %>%
  add_row(type_orig = "ONCOLOGISCH DAGCENTRUM B", type_new = "Oncology") %>%
  add_row(type_orig = "RADIOLOGIE PRODUCTIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "RADIOLOGIE SR", type_new = "Function Measurement") %>%
  add_row(type_orig = "REVALIDATIE IG HART- EN VAATZ", type_new = "Cardiac") %>%
  add_row(type_orig = "REVALIDATIECENTRUM GHB", type_new = "Other") %>% # Revalidation mapped to Other
  add_row(type_orig = "REVALIDATIECENTRUM PEL", type_new = "Other") %>% # Revalidation mapped to Other
  add_row(type_orig = "RPL-FM OOGZIEKTEN", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL & TECHN PREST MKA", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL & TECHN. PREST. TANDHEELK.", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL DERMATOLOGIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL GYNAECOLOGIE & VERLOSKUNDE", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL HEELKUNDE", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL INTERNE GENEESKUNDE", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL NEURO / NEUROCHIRURGIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL NEUS-KEEL-OORZIEKTEN", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL OFT - NKO SATELLIET", type_new = "Function Measurement") %>%
  add_row(type_orig = "RPL ONCOLOGIE", type_new = "Function Measurement") %>%
  add_row(type_orig = "SPOEDGEVALLEN", type_new = "Emergency") %>%
  add_row(type_orig = "UPC EPSI-UNIT", type_new = "Other") %>%
  add_row(type_orig = "UPC HOSP KINDEREN", type_new = "Other") %>% # why not ped?
  add_row(type_orig = "UPC RPL PSY  VOLW & OUDEREN", type_new = "Function Measurement") %>%
  add_row(type_orig = "VZW CENTR. ONTWIK.STOORN.(COS)", type_new = "Other") %>%
  add_row(type_orig = "WEEKHOSP KINDEREN", type_new = "Pediatrics") %>% 
  add_row(type_orig = "HOSP PSY OUDEREN LUBBEEK", type_new = "Other") %>% 
  add_row(type_orig = "AMBULANT CENTRUM A", type_new = "Other") %>% 
  add_row(type_orig = "AMBULANT CENTRUM C", type_new = "Other") %>% 
  add_row(type_orig = "AMBULANT CENTRUM D", type_new = "Other") %>% 
  add_row(type_orig = "AMBULANT CENTRUM E", type_new = "Other") %>% 
  add_row(type_orig = "AMBULANT CENTRUM VROUW", type_new = "Other") %>% 
  add_row(type_orig = "BEELDVORMING NUC", type_new = "Function Measurement") %>% 
  add_row(type_orig = "CONSULTATIE ACTIVITEITEN CME", type_new = "Other") %>% 
  add_row(type_orig = "FERTILITEITSTEAM", type_new = "Gynecology") %>% 
  add_row(type_orig = "FM AFERESE", type_new = "Function Measurement") %>% 
  add_row(type_orig = "GERIATRISCH DAGCENTRUM", type_new = "Geriatrics") %>% 
  add_row(type_orig = "Niet ingevuld", type_new = "Other") %>% 
  add_row(type_orig = "RAADPLEGING C PELLENBERG", type_new = "Other") %>% 
  add_row(type_orig = "THERAPIETOREN CAMPUS PLB", type_new = "Other") %>% # Revalidation mapped to Other
  add_row(type_orig = "UPC RPL KINDEREN", type_new = "Other")

# dictionary for afdelingOmschrijving
dict_AC_afdelingOmschrijving <- tibble(type_orig = "abdomin.transplantatiechir.", type_new = "Transplant")
dict_AC_afdelingOmschrijving <- dict_AC_afdelingOmschrijving %>%
  add_row(type_orig = "abdominale heelkunde", type_new = "Abdomen") %>%
  add_row(type_orig = "aferese", type_new = "Hematology") %>%
  add_row(type_orig = "algemene kindergeneeskunde", type_new = "Pediatrics") %>%
  add_row(type_orig = "algemene medische oncologie", type_new = "Oncology") %>%
  add_row(type_orig = "allergische kinderaandoeningen", type_new = "Pediatrics") %>%
  add_row(type_orig = "andrologie", type_new = "Other") %>%
  add_row(type_orig = "anesthesiologie", type_new = "Operation") %>%
  add_row(type_orig = "bekkenbodemcentrum", type_new = "Function Measurement") %>%
  add_row(type_orig = "bijz. echogr. onderzoeken", type_new = "Function Measurement") %>%
  add_row(type_orig = "brandwondencentrum", type_new = "Burns") %>%
  add_row(type_orig = "cardiale heelkunde", type_new = "Cardiac") %>%
  add_row(type_orig = "cardiovasculaire revalidatie", type_new = "Cardiac") %>%
  add_row(type_orig = "centrum logopedie audiologie (MUCLA)", type_new = "Other") %>%
  add_row(type_orig = "centrum ontwikkelingsstoornissen", type_new = "Pediatrics") %>%
  add_row(type_orig = "cerebrovasculaire aandoeningen", type_new = "Neuro") %>%
  add_row(type_orig = "cleft lip and palate", type_new = "Other") %>%
  add_row(type_orig = "conserv. tandheelkunde", type_new = "ORL") %>%
  add_row(type_orig = "contactallergie", type_new = "Other") %>%
  add_row(type_orig = "contactlenzen centrum", type_new = "Other") %>%
  add_row(type_orig = "crisis interventiecentrum", type_new = "Other") %>%
  add_row(type_orig = "depressie bij ouderen", type_new = "Other") %>%
  add_row(type_orig = "dermatologie", type_new = "Other") %>%
  add_row(type_orig = "diabetes", type_new = "Other") %>%
  add_row(type_orig = "diabetische voet", type_new = "Endocrinology") %>%
  add_row(type_orig = "diagnostische kliniek", type_new = "Other") %>%
  add_row(type_orig = "dietiste", type_new = "Endocrinology") %>%
  add_row(type_orig = "digestieve oncologie", type_new = "Abdomen") %>%
  add_row(type_orig = "echogr. gynaeco en obstetrie", type_new = "Gynecology") %>%
  add_row(type_orig = "elektromyografie", type_new = "Function Measurement") %>%
  add_row(type_orig = "epilepsiemonitoring", type_new = "Neuro") %>%
  add_row(type_orig = "fertiliteitscentrum", type_new = "Other") %>%
  add_row(type_orig = "fertiliteitslab", type_new = "Other") %>%
  add_row(type_orig = "flebologie", type_new = "Other") %>%
  add_row(type_orig = "forensische geneeskunde", type_new = "Other") %>%
  add_row(type_orig = "fotodermatologie", type_new = "Other") %>%
  add_row(type_orig = "functiemeting radiologie", type_new = "Function Measurement") %>%
  add_row(type_orig = "fysische geneeskunde", type_new = "Other") %>%
  add_row(type_orig = "gescreende hemofiliepatienten", type_new = "Other") %>%
  add_row(type_orig = "gynaecologie", type_new = "Gynecology") %>%
  add_row(type_orig = "gynaecologische oncologie", type_new = "Gynecology") %>%
  add_row(type_orig = "haarkliniek", type_new = "Other") %>%
  add_row(type_orig = "harttransplantatie", type_new = "Cardiac") %>%
  add_row(type_orig = "huidtumoren", type_new = "Oncology") %>%
  add_row(type_orig = "hypertensie", type_new = "Cardiac") %>%
  add_row(type_orig = "I.G. algemene", type_new = "Internal Medicine") %>%
  add_row(type_orig = "I.G. allergie", type_new = "Other") %>%
  add_row(type_orig = "I.G. bloedings- en vaatziekten", type_new = "Cardiac") %>%
  add_row(type_orig = "I.G. cardiologie", type_new = "Cardiac") %>%
  add_row(type_orig = "I.G. endocrinologie", type_new = "Endocrinology") %>%
  add_row(type_orig = "I.G. geriatrie", type_new = "Geriatrics") %>%
  add_row(type_orig = "I.G. hematologie", type_new = "Hematology") %>%
  add_row(type_orig = "I.G. lever, galblaas, pancreas", type_new = "Abdomen") %>%
  add_row(type_orig = "I.G. maag- darmziekten", type_new = "Abdomen") %>%
  add_row(type_orig = "I.G. nefrologie", type_new = "Nephrology") %>%
  add_row(type_orig = "I.G. pneumologie", type_new = "Pneumology") %>%
  add_row(type_orig = "I.G. reumatologie", type_new = "Other") %>%
  add_row(type_orig = "ilizarov", type_new = "Pediatrics") %>%
  add_row(type_orig = "infectieziekten", type_new = "Other") %>%
  add_row(type_orig = "intensieve geneeskunde", type_new = "ICU") %>%
  add_row(type_orig = "kinder-immuundeficienties", type_new = "Pediatrics") %>%
  add_row(type_orig = "kindercardiologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderendocrinologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kindergastro-enterologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kindergen-endocrino-diabetes", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderhematologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kindernefrologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderneurologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderoncologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderpneumologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderpsychiatrie urgentie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderreumatologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "knie- en sportletsels", type_new = "Traumatology") %>%
  add_row(type_orig = "Leuvens algologisch centrum", type_new = "Other") %>%
  add_row(type_orig = "liaisonpsychiatrie", type_new = "Other") %>%
  add_row(type_orig = "lipiden", type_new = "Endocrinology") %>%
  add_row(type_orig = "locomot. centrum sportletsels", type_new = "Other") %>%
  add_row(type_orig = "longtransplantatie", type_new = "Pneumology") %>%
  add_row(type_orig = "maternal intensive care", type_new = "Gynecology") %>%
  add_row(type_orig = "medische retina afdeling", type_new = "Other") %>%
  add_row(type_orig = "mens. erfelijkheid", type_new = "Other") %>%
  add_row(type_orig = "metabole ziekten", type_new = "Other") %>%
  add_row(type_orig = "mond-kaak-aangezichtschirurgie", type_new = "ORL") %>%
  add_row(type_orig = "motorische revalidatie", type_new = "Other") %>%
  add_row(type_orig = "mucoviscidose", type_new = "Other") %>%
  add_row(type_orig = "multidisc. raadpl. osteoporose", type_new = "Other") %>%
  add_row(type_orig = "multidisciplinair borstcentrum", type_new = "Gynecology") %>%
  add_row(type_orig = "multidisciplinaire metabole ziekten", type_new = "Other") %>%
  add_row(type_orig = "neonatologie", type_new = "Neonatology") %>%
  add_row(type_orig = "neuro-oncologieraadpleging", type_new = "Oncology") %>%
  add_row(type_orig = "neuro-orthopedie kinderen", type_new = "Traumatology") %>%
  add_row(type_orig = "neurochirurgie", type_new = "Neuro") %>%
  add_row(type_orig = "neurologie", type_new = "Neuro") %>%
  add_row(type_orig = "neuromusculaire aandoeningen", type_new = "Neuro") %>%
  add_row(type_orig = "neus-, keel- en oorziekten alg.", type_new = "ORL") %>%
  add_row(type_orig = "Niet ingevuld", type_new = "Other") %>%
  add_row(type_orig = "nucleaire geneeskunde", type_new = "Oncology") %>%
  add_row(type_orig = "observatiebedden kinderpsy.", type_new = "Other") %>%
  add_row(type_orig = "oncologische heelkunde", type_new = "Oncology") %>%
  add_row(type_orig = "oogziekten", type_new = "Other") %>%
  add_row(type_orig = "orale beeldvorming", type_new = "ORL") %>%
  add_row(type_orig = "ortho-reuma voetraadpleging", type_new = "Traumatology") %>%
  add_row(type_orig = "orthodontie", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie handen", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie heupen", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie kinderen", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie rug/nek", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie schouders", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie tumoren", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopedie voeten", type_new = "Traumatology") %>%
  add_row(type_orig = "orthopsie", type_new = "Other") %>%
  add_row(type_orig = "ouderen- en neuropsychiatrie", type_new = "Other") %>%
  add_row(type_orig = "overleden patienten", type_new = "Other") %>%
  add_row(type_orig = "palliatieve", type_new = "Palliative") %>%
  add_row(type_orig = "parodontologie", type_new = "ORL") %>%
  add_row(type_orig = "pasgeborenen", type_new = "Neonatology") %>%
  add_row(type_orig = "pastorale dienst", type_new = "Other") %>%
  add_row(type_orig = "perimetrie", type_new = "Function Measurement") %>%
  add_row(type_orig = "persist.vegetatieve status", type_new = "Other") %>%
  add_row(type_orig = "plast. reconstr.en esthet.chir.", type_new = "Operation") %>%
  add_row(type_orig = "proctologie", type_new = "Other") %>%
  add_row(type_orig = "psychiatrie", type_new = "Other") %>%
  add_row(type_orig = "radiologie alg.", type_new = "Function Measurement") %>%
  add_row(type_orig = "radiotherapie-oncologie", type_new = "Oncology") %>%
  add_row(type_orig = "respiratoire oncologie/pneumologie", type_new = "Pneumology") %>%
  add_row(type_orig = "respiratoire revalidatie", type_new = "Other") %>%
  add_row(type_orig = "restauratieve tandheelkunde", type_new = "ORL") %>%
  add_row(type_orig = "reumatologie-systeemziekten", type_new = "Other") %>%
  add_row(type_orig = "revalidatie", type_new = "Other") %>%
  add_row(type_orig = "revalidatie ex-carcinoompt.", type_new = "Other") %>%
  add_row(type_orig = "revalidatie orthopedie", type_new = "Other") %>%
  add_row(type_orig = "revalidatie slechtzienden", type_new = "Other") %>%
  add_row(type_orig = "scoliose", type_new = "Traumatology") %>%
  add_row(type_orig = "slaaplabo", type_new = "Neuro") %>%
  add_row(type_orig = "slikstoornissen", type_new = "Function Measurement") %>%
  add_row(type_orig = "staalname", type_new = "Other") %>%
  add_row(type_orig = "stemming en cognitie", type_new = "Neuro") %>%
  add_row(type_orig = "STOMATOLOGIE", type_new = "ORL") %>%
  add_row(type_orig = "systeemziekten", type_new = "Other") %>%
  add_row(type_orig = "tandheelk. urgentiekliniek", type_new = "ORL") %>%
  add_row(type_orig = "tandheelkunde, mondziekten, stomato", type_new = "ORL") %>%
  add_row(type_orig = "thoracale heelkunde", type_new = "Thoracic Surgery") %>%
  add_row(type_orig = "transplantatie bij kinderen", type_new = "Pediatrics") %>%
  add_row(type_orig = "traumatologische heelkunde", type_new = "Traumatology") %>%
  add_row(type_orig = "urgentiegeneeskunde", type_new = "ICU") %>%
  add_row(type_orig = "urine-incontinentie", type_new = "Urology") %>%
  add_row(type_orig = "urologie", type_new = "Urology") %>%
  add_row(type_orig = "urologie-gezwelziekten", type_new = "Urology") %>%
  add_row(type_orig = "vaatheelkunde", type_new = "Cardiac") %>%
  add_row(type_orig = "verloskunde/gynaecologie", type_new = "Gynecology") %>%
  add_row(type_orig = "zuigelingen algemeen", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderhepatologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "kinderpsychiatrie", type_new = "Pediatrics") %>%
  add_row(type_orig = "tandheelkunde alg. kliniek", type_new = "ORL") %>%
  add_row(type_orig = "liaison kinderen psychologen", type_new = "Pediatrics") %>%
  add_row(type_orig = "ramp", type_new = "Other") %>%
  add_row(type_orig = "apotheek", type_new = "Other") %>%
  add_row(type_orig = "C.E.R.M.", type_new = "Other") %>%
  add_row(type_orig = "electrofysiologie", type_new = "Other") %>%
  add_row(type_orig = "kinderdermatologie", type_new = "Pediatrics") %>%
  add_row(type_orig = "laserbehandeling", type_new = "Other") %>%
  add_row(type_orig = "expertisecentrum autisme", type_new = "Other") %>%
  add_row(type_orig = "mobiel crisisteam", type_new = "Other") %>%
  add_row(type_orig = "administratieve code", type_new = "Other") %>%
  add_row(type_orig = "atopisch eczeem", type_new = "Other") %>%
  add_row(type_orig = "autom.creatie lab-activiteiten", type_new = "Other") %>%
  add_row(type_orig = "interdisciplinaire aanpak multiple sclerose", type_new = "Other") %>%
  add_row(type_orig = "lymfoomraadpleging", type_new = "Other") %>%
  add_row(type_orig = "sociaal werk", type_new = "Other") 


# interdisciplinaire aanpak multiple sclerose   
# lymfoomraadpleging   
# sociaal werk 

# ADMISSION CONFIG
# ----------------

exclude_neonates <- FALSE

# dictionary for gender
dict_gender <- tibble(orig = "m", new = 1) %>% 
  add_row(orig = "M", new = 1) %>% 
  add_row(orig = "v", new = 0) %>% 
  add_row(orig = "V", new = 0) 

# dictionary for admission source: Home or Other
dict_place_before_admission <- tibble(place_orig = "Woonplaats", place_new = "Home")
dict_place_before_admission <- dict_place_before_admission %>% 
  add_row(place_orig = "Niet-universitair ziekenhuis", place_new = "Other") %>% 
  add_row(place_orig = "UZLeuven", place_new = "Other") %>% 
  add_row(place_orig = "Rustoord", place_new = "Other") %>% 
  add_row(place_orig = "Openbare plaats", place_new = "Other") %>% 
  add_row(place_orig = "Onbekend", place_new = "NA") %>% # map NA this way
  add_row(place_orig = "Psychiatrische instelling", place_new = "Other") %>% 
  add_row(place_orig = "Andere instelling", place_new = "Other") %>% 
  add_row(place_orig = "Ander universitair ziekenhuis", place_new = "Other") %>% 
  add_row(place_orig = "Buitenlands ziekenhuis", place_new = "Other") %>% 
  add_row(place_orig = "Werkmilieu", place_new = "Other") %>% 
  add_row(place_orig = "Verkeer (niet van/naar werk/school)", place_new = "Other") %>% 
  add_row(place_orig = "Sportterrein", place_new = "Other") %>% 
  add_row(place_orig = "Weg naar/van werk", place_new = "Other") %>% 
  add_row(place_orig = "Rust- en verzorgingstehuis", place_new = "Other") %>% 
  add_row(place_orig = "School", place_new = "Other") %>% 
  add_row(place_orig = "Weg naar/van school", place_new = "Other") %>% 
  add_row(place_orig = "Niet ingevuld", place_new = "NA") # map NA this way

# dictionary for admission type: Emergency or not
dict_admission_type <- tibble(adm_type_orig = "Geplande opname", adm_type_new = "Other")
dict_admission_type <- dict_admission_type %>% 
  add_row(adm_type_orig = "Spoedopname spoedgevallen", adm_type_new = "Emergency") %>% 
  add_row(adm_type_orig = "Andere spoedopname", adm_type_new = "Emergency") %>% 
  add_row(adm_type_orig = "Onbekend", adm_type_new = "NA") %>%# map NA this way
  add_row(adm_type_orig = "Niet ingevuld", adm_type_new = "NA") # map NA this way

# dictionary for admission reason: Accident or not
dict_admission_reason <- tibble(adm_reason_orig = "Ziekte", adm_reason_new = "Other")
dict_admission_reason <- dict_admission_reason %>% 
  add_row(adm_reason_orig = "Onbekend", adm_reason_new = "NA") %>% # map NA this way
  add_row(adm_reason_orig = "Pasgeboren", adm_reason_new = "Other") %>% 
  add_row(adm_reason_orig = "Ongeval thuis", adm_reason_new = "Accident") %>% 
  add_row(adm_reason_orig = "Ongeval algemeen", adm_reason_new = "Accident") %>% 
  add_row(adm_reason_orig = "Verkeersongeval", adm_reason_new = "Accident") %>% 
  add_row(adm_reason_orig = "Werkongeval", adm_reason_new = "Other") %>% 
  add_row(adm_reason_orig = "Controle", adm_reason_new = "Other") %>% 
  add_row(adm_reason_orig = "Sportongeval", adm_reason_new = "Accident") %>% 
  add_row(adm_reason_orig = "Bevalling", adm_reason_new = "Other") %>% 
  add_row(adm_reason_orig = "Schoolongeval", adm_reason_new = "Accident") %>% 
  add_row(adm_reason_orig = "Niet ingevuld", adm_reason_new = "NA") # map NA this way

# dictionary for admission referral: GP or not
dict_referral <- tibble(referral_orig = "Specialist UZLeuven", referral_new = "Other")
dict_referral <- dict_referral %>% 
  add_row(referral_orig = "Eigen initiatief", referral_new = "Other") %>% # map NA this way
  add_row(referral_orig = "Specialist niet UZLeuven", referral_new = "Other") %>% 
  add_row(referral_orig = "Huisarts + brief", referral_new = "GP") %>% 
  add_row(referral_orig = "Leek", referral_new = "Other") %>% 
  add_row(referral_orig = "Wachtarts + brief", referral_new = "GP") %>% 
  add_row(referral_orig = "Onbekend", referral_new = "NA") %>% 
  add_row(referral_orig = "Huisarts + telefoon", referral_new = "GP") %>% 
  add_row(referral_orig = "Huisarts", referral_new = "GP") %>% 
  add_row(referral_orig = "Wachtarts + telefoon", referral_new = "GP") %>% 
  add_row(referral_orig = "Politie", referral_new = "Other") %>% 
  add_row(referral_orig = "Verzekeringsorganisme", referral_new = "Other") %>% 
  add_row(referral_orig = "Wachtarts", referral_new = "GP") %>% 
  add_row(referral_orig = "Niet ingevuld", referral_new = "NA") # map NA this way

# Historic Comorbidities CONFIG
# -----------------------------

# historic comorbidities codes to include in data

dict_historic_comorbidities <- tibble(his_com_orig = "Other nutritional; endocrine; and metabolic disorders", his_com_new = "58_Other_nutritional_endocrine_and_metabolic_disorders")
dict_historic_comorbidities <- dict_historic_comorbidities %>% 
  add_row(his_com_orig = "Secondary malignancies", his_com_new = "42_Secondary_malignancies") %>% 
  add_row(his_com_orig = "Complications of surgical procedures or medical care", his_com_new = "238_Complications_of_surgical_procedures_or_medical_care") %>% 
  add_row(his_com_orig = "Fluid and electrolyte disorders", his_com_new = "55_Fluid_and_electrolyte_disorders") %>% 
  add_row(his_com_orig = "Bacterial infection; unspecified site", his_com_new = "3_Bacterial_infection_unspecified_site") %>% 
  add_row(his_com_orig = "Coronary atherosclerosis and other heart disease", his_com_new = "101_Coronary_atherosclerosis_and_other_heart_disease") %>%
  add_row(his_com_orig = "Other gastrointestinal disorders", his_com_new = "155_Other_gastrointestinal_disorders") %>%
  add_row(his_com_orig = "Cardiac dysrhythmias", his_com_new = "106_Cardiac_dysrhythmias") %>%
  add_row(his_com_orig = "Disorders of lipid metabolism", his_com_new = "53_Disorders_of_lipid_metabolism") %>%
  add_row(his_com_orig = "Allergic reactions", his_com_new = "253_Allergic_reactions") %>%
  add_row(his_com_orig = "Essential hypertension", his_com_new = "98_Essential_hypertension") %>%
  add_row(his_com_orig = "Other circulatory disease", his_com_new = "117_Other_circulatory_disease") %>%
  add_row(his_com_orig = "Heart valve disorders", his_com_new = "96_Heart_valve_disorders") %>%
  add_row(his_com_orig = "Maintenance chemotherapy; radiotherapy", his_com_new = "45_Maintenance_chemotherapy_radiotherapy") %>%
  add_row(his_com_orig = "Chronic kidney disease", his_com_new = "158_Chronic_kidney_disease") %>%
  add_row(his_com_orig = "Deficiency and other anemia", his_com_new = "59_Deficiency_and_other_anemia") %>%
  add_row(his_com_orig = "Adverse effects of medical drugs", his_com_new = "2617_Adverse_effects_of_medical_drugs") %>%
  add_row(his_com_orig = "E Codes: Adverse effects of medical drugs", his_com_new = "2617_Adverse_effects_of_medical_drugs")


# -----------------

# MICROBIOLOGY DATA CONFIG
# ------------------------

# marks results that are cancelled / invalid
MB_invalid_results <- c("overbodig", 
                        "resultaat gewist wegens foutieve patiëntidentificatie",
                        "geannuleerd wegens foutieve patiëntidentificatie",
                        "aanvraag geannuleerd wegens foutieve test-aanvraag",
                        "geen kweek", "niet uitgevoerd", "verkeerde afname")

dict_MB_sample_type <- tibble(Dutch = "wondvocht / etter", English = "skin") %>%
  add_row(Dutch = "urinestaal", English = "urogen") %>%
  add_row(Dutch = "bloed aeroob", English = "blood") %>%
  add_row(Dutch = "bloed anaeroob", English = "blood") %>%
  add_row(Dutch = "bronchusaspiraat", English = "lung") %>%
  add_row(Dutch = "sputum", English = "sputum") %>%
  add_row(Dutch = "biopsie/weefsel", English = "deep_tissue") %>%
  add_row(Dutch = "bronchuslavage/BAL", English = "lung") %>%
  add_row(Dutch = "faeces / lies / perineum", English = "GI") %>%
  add_row(Dutch = "respiratoir specimen", English = "lung") %>%
  add_row(Dutch = "bloedbaan-katheter tip", English = "catheter") %>%
  add_row(Dutch = "bloed pediatrie", English = "blood") %>%
  add_row(Dutch = "redonvocht", English = "drain") %>%
  add_row(Dutch = "diversen", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "subcutaan deel katheter", English = "catheter") %>%
  add_row(Dutch = "ascites/peritoneaalvocht", English = "GI") %>%
  add_row(Dutch = "diverse punctievochten", English = "drain") %>%
  add_row(Dutch = "pleuravocht", English = "lung") %>%
  add_row(Dutch = "rectale wisser", English = "GI") %>%
  add_row(Dutch = "keel / mondholte", English = "sputum") %>%
  add_row(Dutch = "redon/draintip", English = "drain") %>%
  add_row(Dutch = "keel", English = "sputum") %>%
  add_row(Dutch = "MRSA", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "vagina", English = "urogen") %>%
  add_row(Dutch = "gewrichtsvocht", English = "deep_tissue") %>%
  add_row(Dutch = "divers materiaal", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "neus / nasofaryngaal aspiraat", English = "sputum") %>%
  add_row(Dutch = "blaassondetip", English = "urogen") %>%
  add_row(Dutch = "moedermelk", English = "urogen") %>%
  add_row(Dutch = "oog", English = "UNKNOWN_VALUE") %>% # originally eye, too sparse
  add_row(Dutch = "faeces", English = "GI") %>%
  add_row(Dutch = "sonicatie", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "lumbaal vocht", English = "UNKNOWN_VALUE") %>% # originally neuro, too sparse
  add_row(Dutch = "ventrikel vocht", English = "UNKNOWN_VALUE") %>% # originally neuro, too sparse
  add_row(Dutch = "diabetische voet", English = "skin") %>%
  add_row(Dutch = "oor", English = "UNKNOWN_VALUE") %>% # originally ORL, too sparse
  add_row(Dutch = "urethra", English = "urogen") %>%
  add_row(Dutch = "neus", English = "UNKNOWN_VALUE") %>% # originally ORL, too sparse
  add_row(Dutch = "weefselbank", English = "UNKNOWN_VALUE") %>% # originally external, too sparse
  add_row(Dutch = "hematopoietische stamcellenbank", English = "UNKNOWN_VALUE") %>% # originally external, too sparse
  add_row(Dutch = "huid", English = "skin") %>%
  add_row(Dutch = "pericardvocht", English = "deep_tissue") %>%
  add_row(Dutch = "sinus", English = "UNKNOWN_VALUE") %>% # originally ORL, too sparse
  add_row(Dutch = "nagel", English = "skin") %>%
  add_row(Dutch = "preservatievloeistof", English = "UNKNOWN_VALUE") %>% # originally external, too sparse
  add_row(Dutch = "stuit", English = "skin") %>%
  add_row(Dutch = "galvocht", English = "GI") %>%
  add_row(Dutch = "chronische wonde", English = "skin") %>%
  add_row(Dutch = "cervix", English = "urogen") %>%
  add_row(Dutch = "drainagevocht", English = "drain") %>%
  add_row(Dutch = "cerebrospinaal vocht", English = "UNKNOWN_VALUE") %>% # originally neuro, too sparse
  add_row(Dutch = "operatieveld", English = "skin") %>%
  add_row(Dutch = "sperma", English = "urogen") %>%
  add_row(Dutch = "cultuur", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "urine", English = "urogen") %>%
  add_row(Dutch = "blaassonde", English = "urogen") %>%
  add_row(Dutch = "bloed", English = "blood") %>%
  add_row(Dutch = "bloedproduct ECL", English = "UNKNOWN_VALUE") %>% # originally external, too sparse
  add_row(Dutch = "maagvocht", English = "GI") %>%
  add_row(Dutch = "abces", English = "skin") %>%
  add_row(Dutch = "redontip", English = "drain") %>%
  add_row(Dutch = "wondvocht wisser", English = "skin") %>%
  add_row(Dutch = "EPS", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "recto-vaginale wisser", English = "urogen")
# Note: lung, drain and GI should be the most important


# LAB DATA CONFIG
# ---------------

# lab tests used in neutropenia calculation 
neutropenia_lab_tests <- c("WBC telling (bloed)", 
                           "WBC differentiatie (bloed) : Neutrofielen - automaat - aantal/L",
                           "WBC differentiatie (bloed) : Neutrofielen - microscopisch - aantal/L")

dict_lab_tests <- tibble(Dutch = "CRP (bloed)", English = "CRP") %>%
  add_row(Dutch = "NT-proBNP (bloed)", English = "NT_proBNP") %>%
  add_row(Dutch = "Hemoglobine (bloed)", English = "Hemoglobine") %>%
  add_row(Dutch = "Bloedplaatjes telling (bloed)", English = "Platelet_count") %>% 
  add_row(Dutch = "WBC telling (bloed)", English = "WBC_count") %>%
  # Monocytes
  add_row(Dutch = "WBC differentiatie (bloed) : Monocyten - microscopisch - aantal/L", 
          English = "WBC_Monocytes") %>% 
  add_row(Dutch = "WBC differentiatie (bloed) : Monocyten - automaat - aantal/L", 
          English = "WBC_Monocytes") %>% 
  # Neutrophils
  add_row(Dutch = "WBC differentiatie (bloed) : Neutrofielen - microscopisch - aantal/L", 
          English = "WBC_Neutrophils") %>% 
  add_row(Dutch = "WBC differentiatie (bloed) : Neutrofielen - automaat - aantal/L", 
          English = "WBC_Neutrophils") %>% 
  # other lab (feednack from Frank)
  add_row(Dutch = "Glucose (arterieel bloed) - bloedgasanalyser", English = "glucose_arterial") %>%
  add_row(Dutch = "Bloedgassen (arterieel bloed) : pH", English = "pH") %>%
  add_row(Dutch = "Bloedgassen (arterieel bloed) : pO2", English = "pO2") %>%
  add_row(Dutch = "Bloedgassen (arterieel bloed) : O2 saturatie", English = "O2_saturation") %>%
  add_row(Dutch = "Kalium (bloed)", English = "potassium") %>%
  add_row(Dutch = "Natrium (bloed)", English = "natrium") %>%
  add_row(Dutch = "Creatinine (bloed)", English = "creatinine") %>%
  add_row(Dutch = "Ureum (bloed)", English = "urea") %>%
  add_row(Dutch = "RBC telling (bloed)", English = "RBC_count") %>%
  add_row(Dutch = "Hematocriet (bloed)", English = "haematocrit") %>%
  add_row(Dutch = "Bilirubine totaal (bloed)", English = "bilirubin") %>%
  add_row(Dutch = "AST (bloed)", English = "AST") %>%
  add_row(Dutch = "ALT (bloed)", English = "ALT") %>%
  add_row(Dutch = "LDH (bloed)", English = "LDH") %>%
  add_row(Dutch = "APTT (bloed)", English = "APTT") %>%
  add_row(Dutch = "Glucose (bloed)", English = "glucose") %>%
  add_row(Dutch = "CK (creatinekinase) (bloed)", English = "CK") %>%
  add_row(Dutch = "Troponine T hs (bloed)", English = "troponine_T") %>%
  add_row(Dutch = "Vancomycine (bloed) - dalwaarde", English = "vancomycine") %>%
  add_row(Dutch = "Aspergillus Ag (bloed) - index", English = "aspergillus_ag") %>%
  add_row(Dutch = "Fibrinogeen (bloed)", English = "fibrinogen") %>%
  add_row(Dutch = "Troponine I (bloed)", English = "troponine_I") %>%
  add_row(Dutch = "TSH (bloed)", English = "TSH") %>%
  add_row(Dutch = "Ferritine (bloed)", English = "ferritin") %>%
  add_row(Dutch = "Creatinine Klaring", English = "creatinine_clearance") %>%
  add_row(Dutch = "Cyclosporine (bloed)", English = "ciclosporin") %>%
  add_row(Dutch = "D-dimeren (bloed)", English = "D_dimer") %>%
  add_row(Dutch = "Eiwitelektroforese (bloed) : Albumine - in g/L", English = "SPE_albumin") %>% # SPE = Serum protein electrophoresis
  add_row(Dutch = "Eiwitelektroforese (bloed) : Alfa 1-globuline - in g/L", English = "SPE_albumin_alpha_1_globulin") %>%
  add_row(Dutch = "Eiwitelektroforese (bloed) : Alfa 2-globuline - in g/L", English = "SPE_albumin_alpha_2_globulin") %>%
  add_row(Dutch = "Eiwitelektroforese (bloed) : Beta-globuline - in g/L", English = "SPE_albumin_beta_globulin") %>%
  add_row(Dutch = "Eiwitelektroforese (bloed) : Gamma-globuline - in g/L", English = "SPE_albumin_gamma_globulin") %>%
  add_row(Dutch = "Protrombinetijd (PT) (bloed) - in sec", English = "PT_sec") %>%
  add_row(Dutch = "Protrombinetijd (PT) (bloed) - in %", English = "PT_percent") %>%
  add_row(Dutch = "Protrombinetijd (PT) (bloed) - INR", English = "PT_INR")  

# MEDICATION CONFIG
# -----------------

# shamelessly stolen from https://github.com/OHDSI/Vocabulary-v4.5/blob/661804cf3c17add61b02e2e83e477f48acb011d5/21-ATC/atc_code.txt
dict_ATC <- read_tsv("../config/dict_ATC.tsv", 
                     col_types = list(col_character(), col_character()))

dict_ATC_level_1 <- dict_ATC %>% 
  mutate(code_length = nchar(Code)) %>% 
  filter(code_length == 1) %>% 
  distinct(Code, Description)

dict_ATC_level_2 <- dict_ATC %>% 
  mutate(code_length = nchar(Code)) %>% 
  filter(code_length == 3) %>% 
  distinct(Code, Description)

dict_ATC_level_3 <- dict_ATC %>% 
  mutate(code_length = nchar(Code)) %>% 
  filter(code_length == 4) %>% 
  distinct(Code, Description)

# how many days in the past should we aggregate?
# (pharmacy data has gaps so 1 day window is not appropriate for this extraction)
MED_time_window <- c(3, 7) # in days

# L02 codes to include in data
ATC_L02_to_include <- c("J01", "L01", "L04", "B01", "D03", "H02", "J02", 
                        "J04", "J05", "J06")
# J01 - Antibacterials for systemic use (antibiotics)
# L01 - Antineoplastic agents (chemotherapy)
# L04 - Immunosuppressants 
# B01 - Antithrombotic agents
# D03 - PREPARATIONS FOR TREATMENT OF WOUNDS AND ULCERS 
# H02 - CORTICOSTEROIDS FOR SYSTEMIC USE 
# J02 - ANTIMYCOTICS FOR SYSTEMIC USE  
# J04 - ANTIMYCOBACTERIALS  
# J05 - ANTIVIRALS FOR SYSTEMIC USE  
# J06 - IMMUNE SERA AND IMMUNOGLOBULINS 
# H02 - CORTICOSTEROIDS FOR SYSTEMIC USE

# immunoglobulins codes
immunoglobulins_level_2 <- c("J06")
immunoglobulins_level_3 <- c("L01F")
# Decision" do not include ATC codes (for diagnostic not for treatment)
#immunoglobulins_atc_codes <- c("L04AA03", "L04AA04", "V09HA01", "J06BB02", 
#                               "J07AM52", "V09IX03")
# J06 IMMUNE SERA AND IMMUNOGLOBULINS
# L01F MONOCLONAL ANTIBODIES AND ANTIBODY DRUG CONJUGATES
# L04AA03 	antilymphocyte immunoglobulin (horse) 
# L04AA04 	antithymocyte immunoglobulin (rabbit) 
# V09HA01 	technetium (99mTc) human immunoglobulin – not in data
# J06BB02 	tetanus immunoglobulin 
# J07AM52 	tetanus toxoid, combinations with tetanus immunoglobulin – not in data 
# V09IX03 	iodine (125I) CC49-monoclonal antibody – not in data

# individual ATC codes (L5) to include in data
ATC_codes_to_include = c("B05BA01", "B05BA02", "B05BA03", "B05BA10")

# B05BA01 - amino acids – ok, TPN 
# B05BA02 - fat emulsions – ok, TPN 
# B05BA03 - carbohydrates – ok, not TPN 
# B05BA04 - protein hydrolysates – nok (not available in extraction --> don't take it) 
# B05BA10 - combinations – ok, TPN 

# fat (lipids) are associated with higher risk
# combinations - the question is raised if they include lipids or not
# a check on the product name reveals that probably most combinations (code B05BA10) contain lipids:

#statisticalCodeDescription artikelOmschrijving                             n
#<chr>                      <chr>                                       <int>
#  1 DRUPPELINFUSIES            "OLIMEL  N7-960E 1500  MET MN"               6081 - has lipids
#2 DRUPPELINFUSIES            "OLIMEL  N7-960E 1000  MET MN"               3489 - has lipids
#3 DRUPPELINFUSIES            "OLIMEL  N7-960E 2000  MET MN"               1976 - has lipids
#4 DRUPPELINFUSIES            "OLIMEL  N9-840E 2000  MET MN"               1941 - has lipids
#5 DRUPPELINFUSIES            "SMOFKABIVEN ELEKFREE 16 MET MN"              884 - has lipids
#6 DRUPPELINFUSIES            "PERIOLIMEL N4E ZAK 2,0L"                     536 - has lipids
#7 DRUPPELINFUSIES            "CLINIMIX N14 2000ML MET MN"                  519 - has lipids
#8 DRUPPELINFUSIES            "TPN NEONAT.\"170\" GLUC. 10%  500ML"         469 - probably yes
#9 DRUPPELINFUSIES            "TPN - VOCHTBEPERKING 1/2 ZAK INFUUS 837ML"   424 - probably yes
#10 DRUPPELINFUSIES            "TPN - VOCHTBEPERKING ZAK INFUUS 1660ML"      402 - probably yes
#11 DRUPPELINFUSIES            "TPN NEONAT.\"130\" GLUC. 15%  500ML"         253 - probably yes
#12 DRUPPELINFUSIES            "CLINIMIX N17 1500ML MET MN"                  176 - has lipids
#13 DRUPPELINFUSIES            "TPN NEONAT.\"110\" GLUC. 20%  500ML"          37 - probably yes
#14 DRUPPELINFUSIES            "TPN NEONAT.\" 90\" GLUC. 30%  500ML"          35 - probably yes
#15 DRUPPELINFUSIES            "OLIMEL N7-960E FL INF 1500ML M/ELECTR."        9 - has lipids
#16 DRUPPELINFUSIES            "SMOFKABIVEN 2700  20GN 831904190  2463ML"      1 - has lipids
#
## OLIMEL
## https://www.baxter.be/sites/g/files/ebysai1181/files/2019-10/Olimel%20gebruikershandleiding.pdf
## SMOFKABIVEN
## https://www.fresenius-kabi.com/hk/products/smofkabiven-smofkabiven-peripheral
## PERIOLIMEL
## https://www.nps.org.au/medicine-finder/peri-olimel-emulsion-for-infusion
## CLINIMIX
## https://www.geneesmiddeleninformatiebank.nl/smpc/h17378_smpc.pdf

# dictionary to map administration route (only IV is used now)
dict_med_route <- tibble(Dutch = "IV-INF", English = "IV") %>%
  add_row(Dutch = "I.V.", English = "IV") %>% 
  add_row(Dutch = "INFUUS ALS DUSDANING", English = "IV") %>% 
  add_row(Dutch = "INF.", English = "IV") %>% 
  add_row(Dutch = "INTRALUMINAAL", English = "IV") %>% 
  add_row(Dutch = "P.O.", English = "ORAL")

# lock products for catheters
vanco_cefta_lock_products <- c("VANCOLOCK (MAGISTRALE BEREIDING)",
                               "CEFTAZIDIM LOCK 10MG/ML SPUIT 2ML (MAGISTR.BER.)")
citra_lock_products <- c("CITRA-LOCK 30% AMP 5ML", "CITRA-LOCK S 30% SPUIT 2X2.5ML")
tauro_lock_products <- c("TAUROSEPT 2% FL 10ML", "TAUROSEPT 2% FL 6ML")

# CAREPROGRAM CONFIG
# ------------------

dict_care_program <- tibble(Dutch = "Veneuze tromboembolie", English = "venous_thromboembolism") %>%
  # add_row(Dutch = "Chronisch veneuze ziekte", English = "chronic_venous_insufficiency") %>% # much underestimated
  add_row(Dutch = "Benigne (bij)schildklieraandoeningen", English = "thyroid_adenoma") %>% 
  add_row(Dutch = "Arterieel occlusief lijden", English = "arterial_occlusive_disease") %>% 
  add_row(Dutch = "Chronisch nierlijden", English = "chronic_kidney_disease") %>% 
  add_row(Dutch = "Kleplijden", English = "valvular_disease") %>% 
  # transplant matches perfectly the COM_PATH_transplant_before_LM (from pathologies)
  #add_row(Dutch = "Longtransplantatie", English = "transplant") %>% 
  #add_row(Dutch = "Niertransplantatie", English = "transplant") %>% 
  #add_row(Dutch = "Stamceltransplantatie", English = "transplant") %>% 
  #add_row(Dutch = "Levertransplantatie", English = "transplant") %>% 
  #add_row(Dutch = "Harttransplantatie", English = "transplant") %>% 
  #add_row(Dutch = "Pediatrische lever-, nier- en darmtransplantatie", English = "transplant") %>% 
  add_row(Dutch = "HIV", English = "HIV") %>% 
  add_row(Dutch = "Levercirrose en verwikkeld leverlijden", English = "cirrhosis") %>% 
  # better covered by COM_PATH_tumor_before_LM
  #add_row(Dutch = "Pelviene gynaecologische tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Gastrointestinale tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Respiratoire tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Hoofd en hals tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Urogenitale tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Pediatrische vaste tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Slokdarmtumoren", English = "tumour") %>% 
  #add_row(Dutch = "Pancreas tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Mesenchymale tumoren", English = "tumour") %>% 
  #add_row(Dutch = "Pediatrische hersentumoren", English = "tumour") %>% 
#add_row(Dutch = "Primaire hepato-biliaire tumoren", English = "tumour") %>% 
#add_row(Dutch = "Pediatrische hematologische maligniteiten", English = "tumour") %>% 
#add_row(Dutch = "Intracraniële tumoren", English = "tumour") %>% 
#add_row(Dutch = "Huidtumoren", English = "tumour") %>% 
#add_row(Dutch = "Erfelijke belasting voor gynaecologische en borsttumoren", English = "tumour") %>% 
#add_row(Dutch = "Oogtumoren", English = "tumour") %>% 
add_row(Dutch = "Lymfomen", English = "lymphoma") %>% 
  add_row(Dutch = "Acute nierinsufficiëntie", English = "renal_failure") %>% 
  add_row(Dutch = "Pediatrische nierinsufficiëntie", English = "renal_failure") %>% 
  add_row(Dutch = "COPD", English = "COPD") %>% 
  # add_row(Dutch = "Hypertensie", English = "hypertension") %>% # underestimated
  add_row(Dutch = "Hartfalen", English = "heart_failure") %>% 
  add_row(Dutch = "Diabetes", English = "diabetes") %>% 
  add_row(Dutch = "Pediatrische diabetes", English = "diabetes")

dict_pathology_organ <- read_xlsx("../config/organ_pathology_CareProgram_mapping.xlsx")

dict_organ <- tibble(Dutch = "Maag - Darm Stelsel", English = "intestinal_system") %>%
  add_row(Dutch = "Binnen meerdere orgaanstelsels", English = "multiple_organs") %>% 
  add_row(Dutch = "Ademhalingsstelsel", English = "respiratory_system") %>% 
  add_row(Dutch = "Voortplantingsstelsel", English = "reproductive_system") %>% 
  add_row(Dutch = "Neus, Keel, Oor en Mond", English = "otorhinolaryngology") %>% 
  add_row(Dutch = "Nier en Urinewegen", English = "kidney_and_urinary_tract") %>% 
  add_row(Dutch = "Hart en Vaatstelsel", English = "heart_and_circulatory_system") %>% 
  add_row(Dutch = "Lever, Gal en Pancreas", English = "liver_bile_and_pancreas") %>% 
  add_row(Dutch = "Lymfatisch Stelsel en Bloedvormende Organen", English = "lymphatic_system") %>% 
  add_row(Dutch = "Over orgaanstelsels heen", English = "multiple_organs") %>% 
  add_row(Dutch = "Huid, Onderhuids Weefsel en Borst", English = "skin_and_breast") %>% 
  add_row(Dutch = "Zenuwstelsel", English = "nervous_system_or_psychiatric") %>% 
  add_row(Dutch = "Bewegingsstelsel", English = "musculoskeletal_system") %>% 
  add_row(Dutch = "Oog", English = "eyes") %>% 
  add_row(Dutch = "Zenuwstelsel - Kinderen", English = "neurology") %>% 
  add_row(Dutch = "Maag - Darm Stelsel - Kinderen", English = "intestinal_system") %>% 
  add_row(Dutch = "Hormonaal Stelsel", English = "hormonal_system") %>% 
  add_row(Dutch = "Hart en Vaatstelsel - Kinderen", English = "heart_and_circulatory_system") %>% 
  add_row(Dutch = "Neus, Keel, Oor en Mond - Kinderen", English = "otorhinolaryngology") %>% 
  add_row(Dutch = "Nier en Urinewegen - Kinderen", English = "kidney_and_urinary_tract") %>% 
  add_row(Dutch = "Oog - Kinderen", English = "eyes") %>% 
  add_row(Dutch = "Psychiatrische aandoeningen", English = "nervous_system_or_psychiatric") %>% 
  add_row(Dutch = "Ademhalingsstelsel - Kinderen", English = "respiratory_system") %>% 
  add_row(Dutch = "Lever, Gal en Pancreas - Kinderen", English = "liver_bile_and_pancreas") %>% 
  add_row(Dutch = "Bewegingsstelsel - Kinderen", English = "musculoskeletal_system") %>% 
  add_row(Dutch = "Hormonaal Stelsel - Kinderen", English = "hormonal_system")

dict_pathology <- tibble(Dutch = "Tumoren", English = "tumor") %>%
  add_row(Dutch = "Binnen meerdere pathologienoemers", English = "multiple_pathologies") %>% 
  add_row(Dutch = "Functioneel", English = "functional") %>% 
  add_row(Dutch = "Inflammatie, infectie, immunologie, systeem", English = "inflammation_infection_immunology") %>% 
  add_row(Dutch = "Degeneratie", English = "degeneration") %>% 
  add_row(Dutch = "Vasculair", English = "vascular") %>% 
  add_row(Dutch = "Transplantatie", English = "transplant") %>% 
  add_row(Dutch = "Congenitaal, genetisch", English = "congenital_genetic") %>% 
  add_row(Dutch = "Trauma/Toxicologie", English = "trauma_toxicology") %>% 
  add_row(Dutch = "Revalidatie", English = "rehabilitation") %>% 
  add_row(Dutch = "Psychiatrie", English = "psychiatry") %>% 
  add_row(Dutch = "Psychosomatisch", English = "psychiatry") 

# BLOOD TRANSFER CONFIG
# ---------------------

dict_blood_transfer <- tibble(Dutch = "ECL", English = "RBC_CONCENTRATE") %>%
  add_row(Dutch = "PLAATJES", English = "PLATELETS") %>% 
  add_row(Dutch = "PLASMA", English = "PLASMA")

# CARE MODULE - VS - Vital signs
# ------------------------------

# this dictionary refers to Hoofding Vitale par.
dict_vital_signs_attributes <- tibble(Dutch = "VPademhFreq", English = "respiratory_rate", type = "cont") %>% 
  add_row(Dutch = "VPbloeddrukSys", English = "systolic_BP", type = "cont") %>% 
  add_row(Dutch = "VPbloeddrukDia", English = "diastolic_BP", type = "cont") %>% 
  add_row(Dutch = "VPbloeddrukSDmean", English = "SD_mean_BP", type = "cont") %>% 
  add_row(Dutch = "VPhartslagfreqw", English = "heart_rate", type = "cont") %>% 
  add_row(Dutch = "VPtemperatuurW", English = "temperature", type = "cont") %>% 
  add_row(Dutch = "VPzuurstofsatMZ", English = "breathing_aid", type = "categ") %>% # available in PDMS only from 2016 onwards
  add_row(Dutch = "VPzuurstofsatW", English = "oxygen_saturation", type = "cont") %>% 
  add_row(Dutch = "VPCVDmmHgCVD", English = "CVP", type = "cont") %>% # central_venous_pressure (see also note below)
  # PDMS
  add_row(Dutch = "Ademhalingsfrequentie (meting monitor)", English = "respiratory_rate", type = "cont") %>% 
  add_row(Dutch = "Hartfrequentie", English = "heart_rate", type = "cont") %>%
  add_row(Dutch = "Temperatuur blaas", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur bloed", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur kwik", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur lies (=Ta)", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur lies (=Tkern)", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur oesofagaal", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur oksel (digitaal)", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur oor", English = "temperature", type = "cont") %>%
  add_row(Dutch = "Temperatuur perifeer Li (=T1)", English = "temperature", type = "cont") %>% 
  add_row(Dutch = "Temperatuur perifeer Re (=T2)", English = "temperature", type = "cont") %>% 
  add_row(Dutch = "Temperatuur rectaal", English = "temperature", type = "cont") %>% 
  add_row(Dutch = "Temp. centraal (gevalideerd)", English = "temperature", type = "cont") %>% 
  add_row(Dutch = "Zuurstofsaturatie pulse-oximeter", English = "oxygen_saturation", type = "cont") %>% 
  add_row(Dutch = "Zuurstofsaturatie pulse-oximeter (Links)", English = "oxygen_saturation", type = "cont") %>% 
  add_row(Dutch = "Zuurstofsaturatie pulse-oximeter (Rechts)", English = "oxygen_saturation", type = "cont") %>% 
  add_row(Dutch = "Arteriële Bloeddruk diastolisch", English = "diastolic_BP", type = "cont") %>%
  add_row(Dutch = "Niet Invasieve Bloeddruk diastolisch", English = "diastolic_BP", type = "cont") %>% 
  add_row(Dutch = "Arteriële Bloeddruk systolisch", English = "systolic_BP", type = "cont") %>%
  add_row(Dutch = "Niet Invasieve Bloeddruk systolisch", English = "systolic_BP", type = "cont") %>% 
  add_row(Dutch = "Arteriële Bloeddruk mean", English = "SD_mean_BP", type = "cont") %>% 
  add_row(Dutch = "Niet Invasieve Bloeddruk mean", English = "SD_mean_BP", type = "cont") %>%
  add_row(Dutch = "Centraal Veneuze Druk", English = "CVP", type = "cont") %>% 
  # PDMS - MV params from pdmsParamText
  add_row(Dutch = "SERVO-u modus", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "SERVO-i modus", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus Hamilton", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Dräger modus", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "NU_Modus ventilatie", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus_V60 (Respironics)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus VS Ultra (Thuisventilatie)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus Trilogy (thuisventilatie)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus Thuisventilatie_v2", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus thuisventilatie", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus Niet invasieve ventilatie", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus invasieve ventilatie", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus Focus (Respironics)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus EOLE (Thuisventilatie)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus BiPAP Vision (Respironics)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Modus BiPAP A30 (thuisventilatie)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Q_Dubbel lumen ET Tube (Préopname)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Q_Endotracheale tube (Préopname)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Q_Tracheocanule (Préopname)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Dubbel lumen ET Tube", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Endotracheale tube", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "High Frequency Oscillatie (HFO)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Larynxmasker", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Niet-invasieve BiPaP", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Optiflow", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Thuisventilatie", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Thuisventilatie (duur)", English = "breathing_aid", type = "categ") %>% 
  add_row(Dutch = "Tracheocanule", English = "breathing_aid", type = "categ") %>% 
  # PDMS  MV parameters (FiO2 or PEEP)
  add_row(Dutch = "PEEP (NIV)", English = "PEEP", type = "cont") %>% 
  add_row(Dutch = "Assist druk (P asb incl. PEEP)", English = "PEEP", type = "cont") %>% 
  add_row(Dutch = "Intrinsieke PEEP", English = "PEEP", type = "cont") %>% 
  add_row(Dutch = "TriggerServo (boven PEEP)", English = "PEEP", type = "cont") %>% 
  add_row(Dutch = "TriggerServo (onder PEEP)", English = "PEEP", type = "cont") %>% 
  add_row(Dutch = "FiO2 ingesteld (NIV)", English = "FiO2", type = "cont") %>% 
  add_row(Dutch = "Zuurstoffractie inspiratoir (ECMO)", English = "FiO2", type = "cont") %>% 
  add_row(Dutch = "Zuurstoffractie inspiratoir (beademd, gemeten)", English = "FiO2", type = "cont") %>% 
  add_row(Dutch = "(V) Zuurstoffractie inspiratoir (beademd)", English = "FiO2", type = "cont") %>% 
  add_row(Dutch = "Zuurstoffractie inspiratoir (beademd, ingesteld)", English = "FiO2", type = "cont") %>% 
  add_row(Dutch = "Zuurstoffractie inspiratoir (HFOV)", English = "FiO2", type = "cont") %>% 
  add_row(Dutch = "Zuurstoffractie inspiratoir (nt.beademd, cfr. Nunn)", English = "FiO2", type = "cont") %>% 
  add_row(Dutch = "Zuurstoffractie (afh modus zuurstoftherapie)", English = "FiO2", type = "cont")

# in CARE we have zonder / met zuurstoftoediening (keep only met)
oxygen_admin_waarde <- "Met zuurstoftoediening"

# Decision Elena: I do not take attribute VPCVDmmHgM (Monitoring), but only the CVP measurements
# Most of the times, CVD is measured and monitored at the exact same timestamp
# It doesn't bring much added value and it would be difficult to map to PDMS

# ranges of possible values for vital signs (to delete errors in the system)
temperature_range <- c(30, 45)
BP_range <- c(30, 370)
respiratory_rate_range <- c(0, 900)
heart_rate_range <- c(0, 500)
oxygen_saturation_range <- c(0, 100)
CVP_range <- c(-5, 20)

# CARE MODULE - SAF - Safety and mobility
# ---------------------------------------

# this dictionary refers to Hoofding Veiligheidsmaatregelen & hoofding mobiliteit
# because some topics overlap we take them together
dict_safety_attributes <-   tibble(Dutch = "VMdelirRwijzer", English = "delirium") %>% 
  add_row(Dutch = "VMdelirRwijzer", English = "delirium") %>% 
  #add_row(Dutch = "VMvalRris", English = "risk_of_falling") %>% # only from 2016-07-07 in CARE
  add_row(Dutch = "VMvbmT", English = "freedom_resctriction_measure") %>% 
  add_row(Dutch = "VMwisselSchema", English = "patient_position") %>% 
  add_row(Dutch = "MOBhouEpos", English = "patient_position") %>% 
  add_row(Dutch = "MOBinstalBed", English = "patient_position") %>% 
  #add_row(Dutch = "VMdecubRw", English = "decubitus_risk") %>% # removed after exploration
  #add_row(Dutch = "VMrdRWD", English = "decubitus_risk") %>% 
  add_row(Dutch = "MOBverplhulp", English = "mobility_assistance") %>%
  add_row(Dutch = "MOBinstalZhulp", English = "mobility_assistance") %>%
  add_row(Dutch = "MOBtransferH", English = "mobility_assistance") %>%
  # PDMS
  add_row(Dutch = "Resultaat (Delier)", English = "delirium") %>% # only from 2014
  #add_row(Dutch = "Valrisico", English = "risk_of_falling") %>% 
  add_row(Dutch = "Tijdstip Maatregelen", English = "freedom_resctriction_measure") %>% # only since 2013-05-03 in PDMS
  add_row(Dutch = "Houding", English = "patient_position") %>% 
  # Ris_Decub_Bepaling_Score & Ris_Decub_Bepaling_Score_Risico overlap but have 
  # numeric (0,1,2) or character (Risico, Geen risico, risico ) values 
  #add_row(Dutch = "Ris_Decub_Bepaling_Score_Risico", English = "decubitus_risk") %>% 
  add_row(Dutch = "Mobiliteit : niveau hulp", English = "mobility_assistance")

#dict_decubitus_risk_values <- tibble(Dutch = "Geen risico", English = "NO") %>% 
#  add_row(Dutch = "risico", English = "YES") %>% 
#  add_row(Dutch = "Risico", English = "YES") %>% 
#  add_row(Dutch = "Sterk verhoogd risico", English = "YES") %>% 
#  add_row(Dutch = "Verhoogd risico", English = "YES") 

dict_position_values <- tibble(Dutch = "Rug", English = "supine") %>% # CARE attr VMwisselSchema
  add_row(Dutch = "Linker zijde", English = "lateral") %>% 
  add_row(Dutch = "Rechter zijde", English = "lateral") %>% 
  add_row(Dutch = "Opzitten", English = "sitting") %>% 
  add_row(Dutch = "Rug hoofd links", English = "supine") %>% 
  add_row(Dutch = "Rug hoofd rechts", English = "supine") %>% 
  add_row(Dutch = "Buik hoofd links", English = "prone") %>% 
  add_row(Dutch = "Buik hoofd rechts", English = "prone") %>% 
  add_row(Dutch = "Buik", English = "prone") %>% 
  add_row(Dutch = "Zetelbed", English = "Fowler") %>% 
  # CARE attr MOBhouEpos
  add_row(Dutch = "Voorkeurshouding Rechts", English = "lateral") %>% 
  add_row(Dutch = "Voorkeurshouding Links", English = "lateral") %>% 
  # CARE attr MOBinstalBed
  add_row(Dutch = "Fowler houding", English = "Fowler") %>% 
  add_row(Dutch = "Liggende houding tot max 30Â°", English = "Fowler") %>% 
  add_row(Dutch = "Liggende houding min 30Â°", English = "Fowler") %>% 
  add_row(Dutch = "Liggende houding tot max 30°", English = "Fowler") %>% 
  add_row(Dutch = "Liggende houding min 30°", English = "Fowler") %>% 
  add_row(Dutch = "Zittende houding", English = "sitting") %>% 
  add_row(Dutch = "Linker zijde", English = "lateral") %>% 
  add_row(Dutch = "Rechter zijde", English = "lateral") %>%
  add_row(Dutch = "Rugligging", English = "supine") %>%
  add_row(Dutch = "Buikligging", English = "prone") %>%
  # PDMS
  add_row(Dutch = "Rug 30° hoogstand(= standaard)", English = "Fowler") %>% 
  add_row(Dutch = "Links/rechts gedraaid (zorg)", English = "lateral") %>%
  add_row(Dutch = "Li zij", English = "lateral") %>%
  add_row(Dutch = "Re zij", English = "lateral") %>%
  add_row(Dutch = "Rechtop zittend (eten)", English = "sitting") %>%
  add_row(Dutch = "Geeft zichzelf WH", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "Zetel + visco-elastisch kussen", English = "Fowler") %>%
  add_row(Dutch = "Zetel zonder kussen", English = "Fowler") %>%
  add_row(Dutch = "Rondlopen", English = "walking") %>%
  add_row(Dutch = "Zetelbed", English = "Fowler") %>%
  add_row(Dutch = "Rug Hoofd Re", English = "supine") %>%
  add_row(Dutch = "Toiletstoel", English = "walking") %>%
  add_row(Dutch = "Medische contra-indicatie wisselhouding", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "Rug Hoofd Li", English = "supine") %>%
  add_row(Dutch = "Zetel + luchtkussen", English = "Fowler") %>%
  add_row(Dutch = "Zetel + alternerend kussen", English = "Fowler") %>%
  add_row(Dutch = "Op de schoot", English = "supine") %>%
  add_row(Dutch = "Rug 0°( vlak)", English = "supine") %>%
  add_row(Dutch = "Buik Hoofd Li  (+herpositioneren benen)", English = "prone") %>%
  add_row(Dutch = "Buik Hoofd Re  (+herpositioneren benen)", English = "prone") %>%
  add_row(Dutch = "Maxi cosi", English = "Fowler") %>%
  add_row(Dutch = "Laterale Wisselligging", English = "lateral") %>% 
  distinct()

dict_delirium_values <- tibble(Dutch = "Geen delirium", English = "NO") %>% # CARE 
  add_row(Dutch = "Waarschijnlijk delirium", English = "YES") %>% 
  # PDMS
  add_row(Dutch = "Afname niet mogelijk", English = "UNKNOWN_VALUE") %>% 
  add_row(Dutch = "Geen delirium", English = "NO") %>% 
  add_row(Dutch = "Wel delirium", English = "YES")

# Note: delirium score not taken because the score in CARE VMdelirRresult does not match as range with 
# the score in PDMS ICDSC score

dict_freedom_resctriction_measure_values <- tibble(Dutch = "Tijdens nacht", English = "partial") %>% # CARE 
  add_row(Dutch = "Tijdens dag", English = "partial") %>% 
  add_row(Dutch = "Dag en nacht", English = "complete") %>% 
  add_row(Dutch = "Tijdens avond", English = "partial") %>% 
  add_row(Dutch = "Tijdens ochtend", English = "partial") %>% 
  add_row(Dutch = "Tijdens namiddag", English = "partial") %>% 
  # PDMS
  add_row(Dutch = "Dag en Nacht", English = "complete") #%>% 
#add_row(Dutch = "Tijdens avond", English = "partial") %>% # these are the same in PDMS and CARE
#add_row(Dutch = "Tijdens dag", English = "partial") %>% 
#add_row(Dutch = "Tijdens nacht", English = "partial") %>% 
#add_row(Dutch = "Tijdens namiddag", English = "partial") %>% 
#add_row(Dutch = "Tijdens ochtend", English = "partial")

# dict_risk_of_falling_values <- tibble(Dutch = "Geen valrisico", English = "NO") %>% # CARE 
#  add_row(Dutch = "Sterk verhoogd valrisico", English = "YES") %>% 
#  add_row(Dutch = "Verhoogd valrisico", English = "YES") %>% 
#  # PDMS
#  add_row(Dutch = "Geen valrisico", English = "NO") %>% 
#  add_row(Dutch = "Valrisico o.w.v.  opname wegens val", English = "YES") %>% 
#  add_row(Dutch = "Valrisico o.w.v. de leeftijd (>75 jr)", English = "YES") %>% 
#  add_row(Dutch = "Valrisico o.w.v. eerder valincident", English = "YES") %>% 
#  add_row(Dutch = "Valrisico volgens inschatting van de verpleegkundige", English = "YES")

dict_mobility_assistance_values <- tibble(Dutch = "Volledige hulp", English = "full_help") %>% 
  add_row(Dutch = "Begeleiden", English = "partial_help") %>% 
  add_row(Dutch = "Zelfstandig", English = "no_help") %>% 
  add_row(Dutch = "Ondersteunen", English = "partial_help") %>% 
  add_row(Dutch = "Begeleiden/supervisie", English = "partial_help") %>% 
  add_row(Dutch = "Gedeeltelijke hulp", English = "partial_help") %>% 
  add_row(Dutch = "Looprek", English = "partial_help") %>% 
  add_row(Dutch = "Buggy", English = "partial_help") %>% 
  add_row(Dutch = "Krukken", English = "partial_help") %>% 
  add_row(Dutch = "Stok", English = "partial_help") %>% 
  add_row(Dutch = "Kruk (1)", English = "partial_help") %>% 
  add_row(Dutch = "Guiding/ Sturing", English = "partial_help") %>% 
  add_row(Dutch = "Brancard", English = "full_help") %>% 
  add_row(Dutch = "Koets", English = "full_help") %>% 
  add_row(Dutch = "Bed", English = "full_help") %>% 
  add_row(Dutch = "Rolstoel", English = "partial_help") %>% 
  add_row(Dutch = "Vierwielrolator", English = "partial_help") %>% 
  add_row(Dutch = "Rolzetel", English = "partial_help") %>% 
  add_row(Dutch = "Elektrische rolstoel", English = "full_help") %>% 
  add_row(Dutch = "Rolator", English = "partial_help") %>% 
  add_row(Dutch = "Electrische rolstoel met gordel", English = "full_help") %>% 
  add_row(Dutch = "Aangepaste rolstoel", English = "partial_help") %>% 
  add_row(Dutch = "Rolstoel met stompsteun", English = "partial_help") %>% 
  add_row(Dutch = "orthopedische rollator", English = "partial_help") %>% 
  add_row(Dutch = "Tillift", English = "full_help") %>% 
  add_row(Dutch = "Dragen", English = "full_help") %>% 
  add_row(Dutch = "Vierpikkel", English = "partial_help") %>% 
  # CARE MOBinstalZhulp
  add_row(Dutch = "Begeleiding", English = "partial_help") %>% 
  # add_row(Dutch = "Zelfstandig", English = "no_help") %>% # already exists
  add_row(Dutch = "Ondersteuning patiÃ«nt", English = "partial_help") %>% 
  add_row(Dutch = "Volledige hulp", English = "full_help") %>% 
  add_row(Dutch = "Dragen patiÃ«nt", English = "full_help") %>% 
  add_row(Dutch = "Ondersteuning patiënt", English = "partial_help") %>% 
  add_row(Dutch = "Vierwielrollator", English = "partial_help") %>% 
  add_row(Dutch = "Passieve tillift", English = "full_help") %>% 
  # add_row(Dutch = "Looprek", English = "partial_help") %>% # already exists
  add_row(Dutch = "Dragen patiënt", English = "full_help") %>% 
  add_row(Dutch = "Actieve tillift", English = "full_help") %>% 
  add_row(Dutch = "Kruk", English = "partial_help") %>% 
  # add_row(Dutch = "Stok", English = "partial_help") %>% # already exists
  add_row(Dutch = "Plank", English = "full_help") %>% 
  add_row(Dutch = "Tweewielrollator", English = "partial_help") %>% 
  add_row(Dutch = "Rolboard", English = "partial_help") %>% 
  # add_row(Dutch = "Vierpikkel", English = "partial_help") %>% # already exists
  add_row(Dutch = "Draaischijf", English = "partial_help") %>% 
  add_row(Dutch = "Slide", English = "partial_help") %>% 
  add_row(Dutch = "Driewielrollator", English = "partial_help") %>% 
  # add_row(Dutch = "Brancard", English = "partial_help") %>% # already exists
  # CARE MOBtransferH
  add_row(Dutch = "Gedeeltelijke hulp", English = "partial_help") %>% 
  # add_row(Dutch = "Zelfstandig", English = "no_help") %>% # already exists
  add_row(Dutch = "Supervisie", English = "no_help") %>% 
  # add_row(Dutch = "Volledige hulp", English = "full_help") %>% # already exists
  add_row(Dutch = "Verbale sturing", English = "no_help") %>% 
  # add_row(Dutch = "Guiding/ Sturing", English = "partial_help") %>% # already exists
  # PDMS
  add_row(Dutch = "Gedeeltelijke hulp (ondersteuning)", English = "partial_help") %>% 
  add_row(Dutch = "Volledige hulp (patiënt steunt niet zelf : optillen, dragen, ...)", English = "full_help") %>% 
  add_row(Dutch = "Geen hulp noch aanwezigheid", English = "no_help") %>% 
  add_row(Dutch = "Aanwezigheid (passief)", English = "no_help") %>% 
  distinct()

# CARE MODULE - ISO - Isolation
# -----------------------------

# this dictionary refers to Hoofding Isolatie
# Note: for CARE this is mapped on zorgDefinitieCode and not attributDefinitieCode (like other dictionaries)
dict_isolation <- tibble(Dutch = "VMBroni", English = "source_isolation") %>% 
  add_row(Dutch = "VMProtMaatr", English = "protective_isolation") %>% 
  # PDMS
  add_row(Dutch = "Bronisolatie in afzonderlijke kamer + 3 maatregelen", English = "source_isolation") %>% 
  add_row(Dutch = "Bronisolatie in gemeenschappelijke kamer", English = "source_isolation") %>% 
  add_row(Dutch = "Protectieve isolatie in afzonderlijke kamer", English = "protective_isolation") %>% 
  add_row(Dutch = "Bronisolatie in afzonderlijke kamer + 2 maatregelen", English = "source_isolation") %>% 
  add_row(Dutch = "Isolatie voor gekende kiem", English = "source_isolation") %>% 
  add_row(Dutch = "Cohortisolatie in gemeenschappelijke kamer", English = "source_isolation") %>% 
  add_row(Dutch = "Quarantaine isolatie in afzonderlijke kamer", English = "source_isolation") %>% 
  add_row(Dutch = "Quarantaine isolatie in gemeenschappelijke kamer", English = "source_isolation") %>% 
  add_row(Dutch = "Gecombineerde isolatie in afzonderlijke kamer", English = "combined_isolation") %>% 
  add_row(Dutch = "Gecombineerde isolatie in gemeenschap", English = "combined_isolation")

# CARE MODULE - SYM - Symptoms
# ----------------------------

# this dictionary refers to Hoofding Sympt. par.
dict_symptoms <- tibble(Dutch = "SPagisedRassW", English = "RASS") %>% # CARE
  add_row(Dutch = "SPjeukInt", English = "pruritus") %>%
  # PDMS
  add_row(Dutch = "Richmond Agitatie Sedatie  Score  (Rass)", English = "RASS") %>%
  add_row(Dutch = "3c.Richmond Agitatie Sedatie  Score  (Rass)", English = "RASS") %>%
  add_row(Dutch = "Jeuk (VAS)", English = "pruritus") 

dict_RASS <- tibble(Dutch = "- 1 = Slaperig", English = -1) %>% # CARE
  add_row(Dutch = "- 2 = Lichte sedatie", English = -2) %>% 
  add_row(Dutch = "- 3 = Matige sedatie", English = -3) %>% 
  add_row(Dutch = "- 4 = Diepe sedatie", English = -4) %>% 
  add_row(Dutch = "- 5 = Niet wekbaar", English = -5) %>% 
  add_row(Dutch = "+ 1 = Rusteloos, woelig", English = 1) %>% 
  add_row(Dutch = "+ 2 = Agitatie", English = 2) %>% 
  add_row(Dutch = "+ 3 = Zeer geagiteerd", English = 3) %>% 
  add_row(Dutch = "+ 4 = Combatief", English = 4) %>% 
  add_row(Dutch = "0 = Alert en kalm", English = 0) %>% 
  # PDMS
  add_row(Dutch = "Slaperig", English = -1) %>% 
  add_row(Dutch = "Diepe sedatie", English = -4) %>% 
  add_row(Dutch = "Lichte sedatie", English = -2) %>% 
  add_row(Dutch = "Matige sedatie", English = -3) %>% 
  add_row(Dutch = "Niet wekbaar", English = -5) %>% 
  add_row(Dutch = "Erg geagiteerd", English = 3) %>% 
  add_row(Dutch = "Alert en kalm", English = 0) %>% 
  add_row(Dutch = "Geagiteerd", English = 2) %>% 
  add_row(Dutch = "Strijdlustig", English = 4) %>% 
  add_row(Dutch = "Onrustig", English = 1) %>% 
  distinct()

dict_pruritus <- tibble(Dutch = "Geen", English = "NO") %>% # CARE
  add_row(Dutch = "Weinig", English = "YES") %>% 
  add_row(Dutch = "Matig", English = "YES") %>% 
  add_row(Dutch = "Veel", English = "YES") %>% 
  add_row(Dutch = "Zeer veel", English = "YES") %>% 
  # PDMS
  add_row(Dutch = "0 -geen jeuk", English = "NO") %>% 
  add_row(Dutch = "1 - milde jeuk", English = "YES") %>% 
  add_row(Dutch = "2 - milde jeuk", English = "YES") %>% 
  add_row(Dutch = "3 - matige jeuk", English = "YES") %>% 
  add_row(Dutch = "4 - matige jeuk", English = "YES") %>% 
  add_row(Dutch = "5 - ernstige niet te verdragen jeuk", English = "YES") %>% 
  add_row(Dutch = "6 - ernstige niet te verdragen jeuk", English = "YES") %>% 
  add_row(Dutch = "7 - ernstige niet te verdragen jeuk", English = "YES") %>% 
  add_row(Dutch = "8 - hevige verschrikkelijke jeuk", English = "YES") %>% 
  add_row(Dutch = "9 - hevige verschrikkelijke jeuk", English = "YES") %>% 
  add_row(Dutch = "10 - ergst denkbare jeuk", English = "YES") 

# CARE MODULE - NEU - Neurology parameters
# ----------------------------------------

# this dictionary refers to Hoofding Neuro. par.
dict_neurology_params <- tibble(Dutch = "NPgcsTscore", English = "GCS_score") %>% # Glasgow coma scale
  add_row(Dutch = "NPPgcs8", English = "GCS_score") %>%  # Glasgow coma scale (paediatrics)
  # PDMS
  add_row(Dutch = "GCS totaal (kind) gevalideerd", English = "GCS_score") %>% 
  add_row(Dutch = "GCS totaal (kind) gevalideerd bij opname", English = "GCS_score") %>% 
  add_row(Dutch = "GCS totaal gevalideerd", English = "GCS_score") %>% 
  add_row(Dutch = "GCS totaal gevalideerd bij opname", English = "GCS_score") #%>% 
# add_row(Dutch = "GCS voor opname", English = "GCS_score") # all dates are 1899-12-30 (I won't use)

# Note Elena: I merge the pediatric and non-pediatric scores, as there are very few pediatric
# Note Elena: I only take total GCS because only this is extracted from PDMS

# Note Elena: 
# In CARE, attribute "NPbWAPAs" contains AVPU_scale: Wakker / Aanspreekbaar / Pijn / A-reactief 
# but it doesn't immediately map to PDMS 
# In PDMS parameter Bewustzijnstoestand (only from 2014-08) contains 2 levels of consciousness 
# For now only use GCS - seems to be used in both CARE and PDMS

# CARE MODULE - EXC - Excretion
# -----------------------------

# this dictionary refers to Hoofding Uitscheiding
dict_excretion <-   tibble(Dutch = "UIThulpStoelIN", English = "faecal_incontinence") %>% 
  add_row(Dutch = "UIThulpUrineIN", English = "urinary_incontinence") %>% 
  # PDMS
  add_row(Dutch = "Faecale situatie", English = "faecal_incontinence") %>% 
  add_row(Dutch = "UitscheidingFaecesIncontinent", English = "faecal_incontinence") %>% 
  add_row(Dutch = "Verpleegprobleem: faecale incontinentie", English = "faecal_incontinence") %>% 
  add_row(Dutch = "UitscheidingUrineIncontinent", English = "urinary_incontinence") %>% 
  add_row(Dutch = "Verpleegprobleem : mictieproblemen", English = "urinary_incontinence") 

dict_faecal_incontinence <- tibble(Dutch = "Accidenteel incontinent", English = "faecal_incontinence") %>% 
  add_row(Dutch = "Dagelijks incontenent", English = "faecal_incontinence") %>% 
  add_row(Dutch = "Nachtelijk incontinent", English = "faecal_incontinence") %>% 
  # PDMS
  add_row(Dutch = "Faecale incontinentie", English = "faecal_incontinence") %>% 
  add_row(Dutch = "Incontinent", English = "faecal_incontinence") %>% 
  add_row(Dutch = "1", English = "faecal_incontinence")

dict_urinary_incontinence <- tibble(Dutch = "Accidenteel incontinent", English = "urinary_incontinence") %>% 
  add_row(Dutch = "Dagelijks incontinent", English = "urinary_incontinence") %>% 
  add_row(Dutch = "Nachtelijk incontinent", English = "urinary_incontinence") %>% 
  # PDMS
  add_row(Dutch = "Incontinentiemateriaal i.f.v. mictieproblemen", English = "urinary_incontinence") %>% 
  add_row(Dutch = "1", English = "urinary_incontinence")

# CARE MODULE - PHY - Physical parameters
# ---------------------------------------

# this dictionary refers to Fysieke par.
dict_physical_params <- tibble(Dutch = "FPflapBV", English = "flap") %>%
  add_row(Dutch = "FPflapPuls", English = "flap") %>% 
  add_row(Dutch = "FPflapT", English = "flap") %>% 
  add_row(Dutch = "FPflapK", English = "flap") %>% 
  add_row(Dutch = "FPflapCV", English = "flap") %>% 
  add_row(Dutch = "FPflapD", English = "flap") %>%
  add_row(Dutch = "FPflapType", English = "flap") %>%
  add_row(Dutch = "FPflapDpunt", English = "flap") %>%
  add_row(Dutch = "FPflapM", English = "flap") %>%
  add_row(Dutch = "FPflapS", English = "flap") %>%
  add_row(Dutch = "FPflapTF", English = "flap") %>%
  add_row(Dutch = "FPflapDS", English = "flap") %>%
  add_row(Dutch = "FPflapHuid", English = "flap") %>%
  add_row(Dutch = "FPflapGOKa", English = "flap") %>%
  add_row(Dutch = "FPflapRP", English = "flap") %>%
  add_row(Dutch = "FPflapVV", English = "flap") %>%
  add_row(Dutch = "FPflapPL", English = "flap") %>%
  add_row(Dutch = "FPflapMF", English = "flap") %>%
  add_row(Dutch = "FPflapMOp", English = "flap") %>%
  add_row(Dutch = "FPflapCR", English = "flap") %>%
  add_row(Dutch = "FPflapFA", English = "flap") %>%
  add_row(Dutch = "FPflapSks", English = "flap") %>%
  add_row(Dutch = "FPflapMO", English = "flap") %>%
  add_row(Dutch = "FPflapMFp", English = "flap") %>% 
  add_row(Dutch = "FPdrainageDebiet", English = "drain") %>%
  add_row(Dutch = "FPdrainageDnummer", English = "drain") %>%
  add_row(Dutch = "FPdrainageDverv", English = "drain") %>%
  add_row(Dutch = "FPdrainageDGO", English = "drain") %>%
  add_row(Dutch = "FPdrainageNaald", English = "drain") %>%
  add_row(Dutch = "FPdrainageDmo", English = "drain") %>%
  add_row(Dutch = "FPdrainageDAK", English = "drain") %>%
  add_row(Dutch = "FPdrainaigeP", English = "drain") %>%
  add_row(Dutch = "FPdrainageDtdNR", English = "drain") %>%
  add_row(Dutch = "FPdrainageDsip", English = "drain") %>%
  add_row(Dutch = "FPdrainageDAM", English = "drain") %>%
  add_row(Dutch = "FPdrainageDsep", English = "drain") %>%
  add_row(Dutch = "FPdrainageDSON", English = "drain") %>%
  add_row(Dutch = "FPdrainageDMOo", English = "drain") %>%
  add_row(Dutch = "FPdrainageDSuctie", English = "drain") %>%
  add_row(Dutch = "FPdrainageDis", English = "drain") %>%
  add_row(Dutch = "FPgewichtW", English = "weight") %>%
  add_row(Dutch = "FPlengteW", English = "length") %>% 
  add_row(Dutch = "FPglycW", English = "glycemia") %>%
  # PDMS
  add_row(Dutch = "Capillaire vulling (flap1)", English = "flap") %>% 
  add_row(Dutch = "Capillaire vulling (flap2)", English = "flap") %>% 
  add_row(Dutch = "Doppler (flap1)", English = "flap") %>% 
  add_row(Dutch = "Doppler (flap2)", English = "flap") %>% 
  add_row(Dutch = "Flapkleur", English = "flap") %>% 
  add_row(Dutch = "Kleur (flap1)", English = "flap") %>% 
  add_row(Dutch = "Kleur (flap2)", English = "flap") %>% 
  add_row(Dutch = "Pulsaties/Doppler flap", English = "flap") %>% 
  add_row(Dutch = "Temperatuur (flap1)", English = "flap") %>% 
  add_row(Dutch = "Temperatuur (flap2)", English = "flap") %>% 
  add_row(Dutch = "Aspect wondvocht Abdom. drain I", English = "drain") %>%
  add_row(Dutch = "Aspect wondvocht Abdom. drain II", English = "drain") %>%
  add_row(Dutch = "Aspect wondvocht Abdom. drain III", English = "drain") %>%
  add_row(Dutch = "Aspect wondvocht Abdom. drain IV", English = "drain") %>%
  add_row(Dutch = "Aspect wondvocht Abdom. drain V", English = "drain") %>%
  add_row(Dutch = "Aspect wondvocht Abdom. drain VI", English = "drain") %>%
  add_row(Dutch = "Abdominale Drain 1 (check)", English = "drain") %>%
  add_row(Dutch = "Abdominale Drain 2 (check)", English = "drain") %>%
  add_row(Dutch = "Abdominale Drain 3 (check)", English = "drain") %>%
  add_row(Dutch = "Abdominale Drain 4 (check)", English = "drain") %>%
  add_row(Dutch = "Abdominale Drain 5 (check)", English = "drain") %>%
  add_row(Dutch = "Abdominale Drain 6 (check)", English = "drain") %>%
  add_row(Dutch = "Abdom. drain 1", English = "drain") %>%
  add_row(Dutch = "Abdom. drain 2", English = "drain") %>%
  add_row(Dutch = "Abdom. drain 3", English = "drain") %>%
  add_row(Dutch = "Abdom. drain 4", English = "drain") %>%
  add_row(Dutch = "Abdom. drain 5", English = "drain") %>%
  add_row(Dutch = "Abdom. drain 6", English = "drain") %>%
  add_row(Dutch = "Spoeling drain (volume)", English = "drain") %>% 
  add_row(Dutch = "Gewicht (kg)", English = "weight") %>%
  add_row(Dutch = "Lengte (in monitor, cfr.hemodyn.berekeningen)", English = "length") %>% 
  add_row(Dutch = "Glucose bloed", English = "glycemia") 

weight_range <- c(0.05, 250)
length_range <- c(0.05, 250)
glycemia_range <- c(0, 2000)

# CARE MODULE - WND - Wound care
# ------------------------------

# this dictionary refers to Huid en wondzorg
dict_wound <- tibble(Dutch = "HWaWTtw", English = "wound_type") %>%
  add_row(Dutch = "HWtypeWonde", English = "wound_type") %>%
  # PDMS
  add_row(Dutch = "WWondtype W1", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W2", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W3", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W4", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W5", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W6", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W7", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W8", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W9", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W10", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W11", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W12", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W13", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W14", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W15", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W16", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W17", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W18", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W19", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W20", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W21", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W22", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W23", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W24", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W25", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W26", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W27", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W28", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W29", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W30", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W31", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W32", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W33", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W34", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W35", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W36", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W37", English = "wound_type") %>% 
  add_row(Dutch = "Wondtype W38", English = "wound_type")

dict_wound_type <- tibble(Dutch = "Sutuur en/of insteekpunt (materiaal aanwezig)", English = "suture") %>% # PDMS
  add_row(Dutch = "Open wonde of gedeeltelijk heropende wonde", English = "open_wound") %>%
  add_row(Dutch = "Brandwonde (open)", English = "open_burn_wound") %>%
  add_row(Dutch = "gesloten dermat. aandoening", English = "closed_wound") %>%
  add_row(Dutch = "Insteekpunt na verwijderen materiaal", English = "post_suture") %>%
  add_row(Dutch = "brandwonde (gesloten)", English = "closed_burn_wound") %>%
  # CARE
  add_row(Dutch = "Sutuur", English = "suture") %>%
  add_row(Dutch = "Huidlestel (open)", English = "open_wound") %>%
  add_row(Dutch = "Chirurgische wonde (open)", English = "open_wound") %>%
  add_row(Dutch = "Huidletsel", English = "open_wound") %>%
  add_row(Dutch = "Decubituswonde", English = "open_wound") %>%
  add_row(Dutch = "Skin Tear", English = "open_wound") %>%
  add_row(Dutch = "Decubituswonde (gesloten)", English = "closed_wound") %>%
  add_row(Dutch = "Navelstrengstomp", English = "closed_wound") %>%
  add_row(Dutch = "Tracheotomie", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "Traumatische wonde", English = "open_wound") %>%
  add_row(Dutch = "Vochtletsel", English = "closed_wound") %>%
  add_row(Dutch = "Donor plaats", English = "post_suture") %>%
  add_row(Dutch = "Ileostomie", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "Colostomie", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "Diabetesvoet", English = "open_wound") %>%
  add_row(Dutch = "Vasculaire wonde (open)", English = "open_wound") %>%
  add_row(Dutch = "Greffe plaats", English = "post_suture") %>%
  add_row(Dutch = "Tracheostomie", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "Urostomie", English = "UNKNOWN_VALUE") %>%
  add_row(Dutch = "Fistel (open wonde)", English = "open_wound") %>%
  add_row(Dutch = "Intertrigo (vochtletsel)", English = "closed_wound") %>%
  add_row(Dutch = "IAD", English = "closed_wound") %>%
  add_row(Dutch = "Oncologische wonde", English = "open_wound") %>%
  add_row(Dutch = "Vasculaire wonde (gesloten)", English = "closed_wound") %>%
  add_row(Dutch = "Brandwonde (gesloten)", English = "closed_burn_wound") %>%
  add_row(Dutch = "Navelstrengstomp met klem", English = "open_wound") %>%
  add_row(Dutch = "Brandwonde", English = "open_burn_wound") %>%
  add_row(Dutch = "Oncologische ulcus", English = "open_wound") %>%
  add_row(Dutch = "Vochtletsel rond wonde", English = "closed_wound") %>%
  add_row(Dutch = "Slijmvliezen", English = "closed_wound") %>%
  add_row(Dutch = "Biopsieplaats", English = "post_suture") %>%
  add_row(Dutch = "Schaafwonde", English = "open_wound") %>%
  add_row(Dutch = "Decubituswonde (open)", English = "open_wound") %>%
  add_row(Dutch = "Huidletsel (open)", English = "open_wound") %>%
  add_row(Dutch = "Traumatische wonde (gesloten)", English = "closed_wound") %>%
  add_row(Dutch = "Externe fixator", English = "suture") %>%
  add_row(Dutch = "Huidgreffe, split thickness", English = "open_wound") %>%
  add_row(Dutch = "Vochtletsel rond stoma / PEG-sonde", English = "closed_wound") %>%
  add_row(Dutch = "Radiodermatitis", English = "open_wound") %>%
  add_row(Dutch = "Huidgreffe, full thickness", English = "open_wound") %>%
  add_row(Dutch = "Intertigo (vochtletsel)", English = "closed_wound") %>%
  add_row(Dutch = "Chirurgische sutuur", English = "suture") %>%
  add_row(Dutch = "Huidletsel (gesloten)", English = "closed_wound") %>%
  add_row(Dutch = "Oncologisch ulcus", English = "open_wound") %>%
  add_row(Dutch = "Episiotomie (gesloten)", English = "closed_wound") %>%
  add_row(Dutch = "Brandwonde (open)", English = "open_burn_wound") %>%
  add_row(Dutch = "Radiodermitis", English = "open_wound") %>%
  add_row(Dutch = "Ruptuur", English = "open_wound") 

# CARE MODULE - CRD - Cardiac care
# --------------------------------

# decision to remove after exploration (temporal trend, not so important)

## this dictionary refers to Cardiale - Vasculaire zorg
#dict_cardiac <- tibble(Dutch = "CZthermO", English = "thermoregulation") %>% # CARE (only since 2016)
#  # PDMS
#  add_row(Dutch = "Bair-Hugger", English = "thermoregulation") 
#
#dict_thermoregulation_values <- tibble(Dutch = "Bair hugger", English = "bair_hugger") %>% # CARE
#  # PDMS
#  add_row(Dutch = "Bair-Hugger", English = "bair_hugger")
## Note: unfortunately for other heating measures or cooling measures there is no mapping PDMS - CARE

# CARE MODULE - DIA - Dialysis
# ----------------------------

# this dictionary refers to Dialyse zorgen & Peritoneale dialyse
# this maps ZorgDefinitieCode not attibuutdefinitiecode
dict_dialysis <- tibble(Dutch = "DZhdMon", English = "hemodialysis") %>%
  add_row(Dutch = "DZhdObs", English = "hemodialysis") %>%
  add_row(Dutch = "DZam", English = "hemodialysis") %>%
  add_row(Dutch = "DZsp", English = "hemodialysis") %>%
  add_row(Dutch = "DZep", English = "hemodialysis") %>%
  add_row(Dutch = "DZhdKath", English = "hemodialysis") %>%
  add_row(Dutch = "DZhdKathO", English = "hemodialysis") %>% 
  # PDMS
  add_row(Dutch = "HemoDialyse (intermittent)", English = "hemodialysis") %>% 
  add_row(Dutch = "Start/Stop Dialyse", English = "hemodialysis") 

# CARE MODULE - ICU - ICU specific parameters
# -------------------------------------------

# ICU specific features 
dict_ICU_params <- tibble(Dutch = "Totaal verbrande lichaamsoppervlak (eerste 48u)", English = "percent_body_burned") %>%
  add_row(Dutch = "Totaal verbrande lichaamsoppervlak (numeriek)", English = "percent_body_burned") %>%
  add_row(Dutch = "ECLS", English = "ECMO") # Extracorporeal membrane oxygenation (ECMO)

percent_range <- c(0, 100)

# POST-PROCESSING BASED ON EXPLORATION & CLINICAL INPUT
# -----------------------------------------------------

columns_to_exclude <- c("CARE_SAF_patient_position_binary_all_walking",
                        "CARE_WND_wound_type_binary_all_closed_burn_wound",
                        "CARE_WND_wound_type_binary_all_open_burn_wound")




