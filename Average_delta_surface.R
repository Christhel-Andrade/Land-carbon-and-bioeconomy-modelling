#Created on 2021-09-09 by C. Andrade, modif 2021-09-10, modif 2021-09-22
#--- Creating ranges for SOC change observed across the PCUs. Calculating the proportion (%) of areas affected by each SOC change range.
#--- C. Andrade, S. Sokol

install.packages("BAMMtools")
library(BAMMtools)
library(data.table)
library(tidyverse)
library(plyr)
library(readr)
library(filesstrings)

suyield=fread("path/to/your/folder/simulation_units_with_yield.csv") # only simulation units which provide yield
suclim=fread("path/to/your/folder/simulation_units_no_meteo.csv")  # only simulation units without meteorological data

# Moving output data into 'AMG_outputs' folder and  input data into 'Simulated_input_files' folder----------------------------------------------------------------------
setwd("path/to/your/directory") # setting working directory

if (dir.exists("Avg_Results") == FALSE) {
    dir.create("Avg_Results")
}

if (dir.exists("All_Results") == FALSE) {
    dir.create("All_Results")
}

list_files <- list.files(pattern = "_conv", all.files = FALSE, full.names = FALSE) # creating a list from all output files
move_files(list_files,"All_Results", overwrite = TRUE)

list_files_exp <- list.files(pattern = "_exp", all.files = FALSE, full.names = FALSE) # creating a list from all input (i.e. BDD) files
move_files(list_files_exp,"All_Results", overwrite = TRUE)

# Storing already simulated input data into respective folders ----------------------------------------------------------------------

setwd("path/to/your/directory/All_Results")

sensitivity_analysis <- "no"

simulated_scenarios <- c("digestate")
simulated_scenarios <- c("Baseline","Biochar","Digestate","Molasses","Gaschar","Hydrochar")
simulated_scenarios_SA <- c("Biochar1","Digestate","Biochar2","Hydrochar","Gaschar","Molasses")
sim_scen <-""

if (sensitivity_analysis == "yes"){
  
  conversion_levels <- c("L","M","H")
  recalcitrance_levels <- c("L","M","H")
  conv_level <- ""
  recalci_level <- ""
  
  if (dir.exists("Sensitivity_analysis_Results") == FALSE) {
    dir.create("Sensitivity_analysis_Results")
  }
  
  list_files_SA <- list.files(pattern = paste("_conv",sep=""), all.files = FALSE, full.names = FALSE)
  move_files(list_files_SA,paste("Sensitivity_analysis_Results",sep=""), overwrite = TRUE)
  
  setwd("path/to/your/directory/All_Results/Sensitivity_analysis_Results")
  
  for (sim_scen in simulated_scenarios_SA){
    if (dir.exists(paste(sim_scen,sep="")) == FALSE) {
      dir.create(paste(sim_scen,sep=""))
    }
    list_files_sim <- list.files(pattern = paste(sim_scen,"_",sep=""), all.files = FALSE, full.names = FALSE)
    move_files(list_files_sim,paste(sim_scen,sep=""), overwrite = TRUE)
  }
  
}else{
  
  for (sim_scen in simulated_scenarios){
    setwd("path/to/your/directory:/All_Results")
    if (dir.exists(paste(sim_scen,sep="")) == FALSE) {
      dir.create(paste(sim_scen,sep=""))
    }
    list_files_sim <- list.files(pattern = paste("Results_",sim_scen,sep=""), all.files = FALSE, full.names = FALSE)
    move_files(list_files_sim,paste(sim_scen), overwrite = TRUE)
  }
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------
###BASELINE ANALYSIS AVERAGE###

setwd("path/to/your/directory:/All_Results/Baseline")
carb0 <- fread("path/to/your/directory:/All_Results/Baseline/Results_Baseline.csv",sep=";")

#removing SU where we don't know crop yields
carb2=carb0[suyield,on=.('ID Traitement'=ID), allow.cartesian=TRUE]

#removing SU where we don't know crop yields nor meteo
carb=carb2[Temperature!=0]
carb[,c("UPC"):=NULL]
range(carb$change_base_t100vst0)

###Simulation units scale###
# get breaks for carbon ratios 
bneg=getJenksBreaks(carb$change_base_t100vst0[carb$change_base_t100vst0 < 0], 4) # 3 intervals
bpos=getJenksBreaks(carb$change_base_t100vst0[carb$change_base_t100vst0 >= 0], 4) # 3 intervals
br=c(head(bneg, -1), bpos) # to check visually!!!
carb$cranges=cut(carb$change_base_t100vst0, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
totsurf=sum(carb$surf_traitement, na.rm=TRUE)

#carb[is.na(surf_traitement), .N, by=`ID Essai`]
prc=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges]

#percentage of surface not changing SOC with bioeconomy because residues are already being exported for other endings
nul=carb[change_base_t100vst0==0]
nul_surf=sum(nul$surf_traitement, na.rm=TRUE)
nul_prc=nul[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100)]

#C converted to CO2, if negative it indicates extra CO2 emitted, if positive it indicates that CO2 has been kept in soil as SOC 
carb[, CO2_delta_t100t0_base_surf:=delta_t100t0_base_surf*44/12]
carb[, CO2_weighted_avrg_delta_t100t0_base_surf:=weighted.mean(CO2_delta_t100t0_base_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

# average delta_scenbase_t100_surf per simulation unit by ID Essai
carb[, weighted_avrg_delta_tC_SU:=weighted.mean(delta_t100t0_base_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]
carb[, weighted_avrg_change:=weighted.mean(change_base_t100vst0, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

###PCU scale###
carb[, Total_delta_tC_PCU:=sum(delta_t100t0_base_surf_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, Total_SOC_0base_PCU:=sum(Cstock_base_ini*surf_traitement_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, w_avg_change_PCU:=Total_delta_tC_PCU/Total_SOC_0base_PCU]
carb[, CO2_delta_t100t0_base_surf_UPC:=delta_t100t0_base_surf_UPC*44/12]
carb[, Total_CO2_delta_t100t0_base_surf_UPC:=sum(CO2_delta_t100t0_base_surf_UPC, na.rm=TRUE), by=`ID Essai`]

# get breaks for carbon ratios_ average
bneg=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU < 0], 4) # 3 intervals
bpos=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU >= 0], 4) # 3 intervals
br=c(head(bneg, -1), bpos) # to check visually!!!
carb$cranges_avg=cut(carb$w_avg_change_PCU, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
prc_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_avg]

# saving results and removing tables in R

if (dir.exists("Results_avg") == FALSE) {
  dir.create("Results_avg")
}

write_delim(carb,paste("Results_avg/", paste("Results_avg_baseline.csv",sep="")), delim = ";", na = "")
carb1=carb %>% distinct(`ID Essai`, .keep_all = TRUE)
write_delim(carb1,paste("Results_avg/", paste("Results_weighted_avg_baseline.csv",sep="")), delim = ";", na = "")

 #################################
### SENSITIVITY ANALYSIS Average###
 #################################

# compiling outputs for SA for Biochar, Hydrochar and Gaschar - Not decrease expected # --------------------------------------------------------------------

setwd("path/to/your/directory/All_Results/Sensitivity_analysis_Results") #set working directory

simulated_scenarios_SA <- c("Biochar1","Biochar2","Gaschar")
export_perc <- 100
conversion_levels <- c("L","M","H")
recalcitrance_levels <- c("L","M","H")
sim_scen <-"biochar2"
conv_level <- "M"
recalci_level <- "M"

setwd(paste("path/to/your/directory/All_Results/Sensitivity_analysis_Results/",sim_scen,"/",sep=""))
carb0 <- fread(paste(sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep=""))

#removing SU where we don't know crop yields
carb2=carb0[suyield,on=.('ID Traitement'=ID), allow.cartesian=TRUE]

#removing SU where we don't know crop yields nor meteo
carb=carb2[Temperature!=0]
carb[,c("UPC"):=NULL]
range(carb$change_scenbase_t100)

###Simulation uNits scale###
# get breaks for carbon ratios
bneg=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 < 0], 6) # 3 intervals
bpos=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 >= 0], 7) # 3 intervals
br=c(head(bneg, -1), bpos) # to check visually!!!
carb$cranges=cut(carb$change_scenbase_t100, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
totsurf=sum(carb$surf_traitement, na.rm=TRUE)
carb$cranges_fix=cut(carb$change_scenbase_t100,  breaks = c(0, 0.000001, 1.06, 2.12, 3.18, 4.24, 5.30), 
                     labels = c("0%-0%","0%-106%", "106%-212%", "212%-318%", "318%-424%", "424%-530%"),
                     include.lowest=TRUE)
##for hydrochar##
#carb$cranges_fix=cut(carb$change_scenbase_t100,  breaks = c(0, 0.000001, 0.08, 0.16, 0.24), 
                      #labels = c("0% - 0%", "0% - 8%", "8% - 16%", "16% - 24%"),
                      #include.lowest=TRUE)

#carb[is.na(surf_traitement), .N, by=`ID Essai`]
prc=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges]
prc_fix=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix]

#percentage of surface not changing SOC with bioeconomy because residues are already being exported for other endings
nul=carb[change_scenbase_t100==0]
nul_surf=sum(nul$surf_traitement, na.rm=TRUE)
nul_prc=nul[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100)]

#C converted to CO2, if negative it indicates extra CO2 emitted, if positive it indicates that CO2 has been kept in soil as SOC 
carb[, CO2_delta_scenbase_t100_surf:=delta_scenbase_t100_surf*44/12]
carb[, CO2_weighted_avrg_delta_scenbase_t100_surf:=weighted.mean(CO2_delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

# average delta_scenbase_t100_surf per simulation unit by ID Essai
carb[, weighted_avrg_delta_tC_SU:=weighted.mean(delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]
carb[, weighted_avrg_change:=weighted.mean(change_scenbase_t100, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

###PCU scale###
carb[, Total_delta_tC_PCU:=sum(delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, Total_SOC_fbase_PCU:=sum(Cstock_base_final*surf_traitement_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, w_avg_change_PCU:=Total_delta_tC_PCU/Total_SOC_fbase_PCU]

#carb[, weighted_avrg_delta_tC_PCU:=weighted.mean(delta_scenbase_t100_surf_UPC, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]
carb[, CO2_delta_scenbase_t100_surf_UPC:=delta_scenbase_t100_surf_UPC*44/12]
carb[, Total_CO2_delta_scenbase_t100_surf_UPC:=sum(CO2_delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]

# get breaks for carbon ratios_ average
bneg=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU < 0], 6) # 3 intervals
bpos=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU >= 0], 7) # 3 intervals
br=c(head(bneg, -1), bpos) # to check visually!!!
carb$cranges_avg=cut(carb$w_avg_change_PCU, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
prc_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_avg]

carb$cranges_fix_avg=cut(carb$w_avg_change_PCU,  breaks = c(0, 0.000001, 1.06, 2.12, 3.18, 4.24, 5.30), 
                         labels = c("0%-0%","0%-106%", "106%-212%", "212%-318%", "318%-424%", "424%-530%"),
                         include.lowest=TRUE)
prc_fix_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix_avg]

##fixed breaks for hydrochar##
#carb$cranges_fix_avg=cut(carb$w_avg_change_PCU,  breaks = c(0, 0.000001, 0.08, 0.16, 0.24), 
 #                         labels = c("0% - 0%", "0% - 8%", "8% - 16%", "16% - 24%"),
  #                        include.lowest=TRUE)
prc_fix_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix_avg]

# saving results and removing tables in R
if (dir.exists("Results_avg") == FALSE) {
  dir.create("Results_avg")
}

write_delim(carb,paste("Results_avg/", paste("Results_avg",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
carb1=carb %>% distinct(`ID Essai`, .keep_all = TRUE)
write_delim(carb1,paste("Results_avg/", paste("Results_weighted_avg",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")

# compiling outputs for SA for Hydrochar,  Digestate, and Molasses  - Only Decrease expected # ------------------------------------------------------------------------

setwd("path/to/your/directory/All_Results/Sensitivity_analysis_Results")

simulated_scenarios_SA <- c("Digestate","Hydrochar","Molasses")
export_perc <- 100
conversion_levels <- c("L","M","H")
recalcitrance_levels <- c("L","M","H")
sim_scen <-"Digestate"
conv_level <- "M"
recalci_level <- "L"

setwd(paste("path/to/your/directory/All_Results/Sensitivity_analysis_Results/",sim_scen,"/",sep=""))
carb0 <- fread(paste(sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep=""))

#removing SU where we don't know crop yields
carb2=carb0[suyield,on=.('ID Traitement'=ID), allow.cartesian=TRUE]

#removing SU where we don't know crop yields nor meteo
carb=carb2[Temperature!=0]
carb[,c("UPC"):=NULL]
range(carb$change_scenbase_t100)

###Simulation uNits scale###
# get breaks for carbon ratios
bneg=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 < 0], 6) # 3 intervals
bpos=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 >= 0], 4) # 3 intervals
br=c(bneg, head(bpos, -1)) # to check visually!!!
carb$cranges=cut(carb$change_scenbase_t100, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
totsurf=sum(carb$surf_traitement, na.rm=TRUE)
carb$cranges_fix=cut(carb$change_scenbase_t100,  breaks = c(-0.24, -0.16, -0.08, -0.00001, 0), 
                     labels = c("-24% - -16%","-16 - %-8%", "-8% - 0%", "0% - 0%"),
                     include.lowest=TRUE)

#carb[is.na(surf_traitement), .N, by=`ID Essai`]
prc=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges]
prc_fix=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix]

#percentage of surface not changing SOC with bioeconomy because residues are already being exported for other endings
nul=carb[change_scenbase_t100==0]
nul_surf=sum(nul$surf_traitement, na.rm=TRUE)
nul_prc=nul[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100)]

#C converted to CO2, if negative it indicates extra CO2 emitted, if positive it indicates that CO2 has been kept in soil as SOC 
carb[, CO2_delta_scenbase_t100_surf:=delta_scenbase_t100_surf*44/12]
carb[, CO2_weighted_avrg_delta_scenbase_t100_surf:=weighted.mean(CO2_delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

# average delta_scenbase_t100_surf per simulation unit by ID Essai
carb[, weighted_avrg_delta_tC_SU:=weighted.mean(delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]
carb[, weighted_avrg_change:=weighted.mean(change_scenbase_t100, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

###PCU scale###
carb[, Total_delta_tC_PCU:=sum(delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, Total_SOC_fbase_PCU:=sum(Cstock_base_final*surf_traitement_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, w_avg_change_PCU:=Total_delta_tC_PCU/Total_SOC_fbase_PCU]
carb[, CO2_delta_scenbase_t100_surf_UPC:=delta_scenbase_t100_surf_UPC*44/12]
carb[, Total_CO2_delta_scenbase_t100_surf_UPC:=sum(CO2_delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]

# get breaks for carbon ratios_ average
bneg=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU < 0], 6) # 3 intervals
bpos=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU >= 0], 4) # 3 intervals
br=c(bneg, head(bpos, -1)) # to check visually!!!
carb$cranges_avg=cut(carb$w_avg_change_PCU, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
prc_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_avg]

carb$cranges_fix_avg=cut(carb$w_avg_change_PCU,  breaks = c(-0.24, -0.16, -0.08, -0.00001, 0), 
                         labels = c("-24% - -16%","-16 - %-8%", "-8% - 0%", "0% - 0%"),
                         include.lowest=TRUE)
prc_fix_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix_avg]

# saving results and removing tables in R

if (dir.exists("Results_avg") == FALSE) {
  dir.create("Results_avg")
}

write_delim(carb,paste("Results_avg/", paste("Results_avg",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
carb1=carb %>% distinct(`ID Essai`, .keep_all = TRUE)
write_delim(carb1,paste("Results_avg/", paste("Results_weighted_avg",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")

# compiling outputs for SA for Hydrochar and  Digestate - Posible increase and decrease # ----------------------------------------------------------------------------

setwd("path/to/your/directory/All_Results/Sensitivity_analysis_Results")

simulated_scenarios_SA <- c("Digestate","Hydrochar")
export_perc <- 100
conversion_levels <- c("L","M","H")
recalcitrance_levels <- c("L","M","H")
sim_scen <-"Digestate"
conv_level <- "L"
recalci_level <- "H"

setwd(paste("path/to/your/directory/All_Results/Sensitivity_analysis_Results/",sim_scen,"/",sep=""))
carb0 <- fread(paste(sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep=""))

#removing SU where we don't know crop yields
carb2=carb0[suyield,on=.('ID Traitement'=ID), allow.cartesian=TRUE]

#removing SU where we don't know crop yields nor meteo
carb=carb2[Temperature!=0]
carb[,c("UPC"):=NULL]
range(carb$change_scenbase_t100)

###Simulation uNits scale###
# get breaks for carbon ratios
bneg=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 < 0], 4) # 3 intervals
bpos=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 >= 0], 4) # 3 intervals
br=c(head(bneg, -1), bpos)
carb$cranges=cut(carb$change_scenbase_t100, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
totsurf=sum(carb$surf_traitement, na.rm=TRUE)
carb$cranges_fix=cut(carb$change_scenbase_t100,  breaks = c(-0.24, -0.16, -0.08, -0.000001, 0, 0.000001, 0.08, 0.16, 0.24), 
                     labels = c("-24% - -16%","-16 - %-8%", "-8% - -0.0001%", "-0.0001% - 0%", "0% - 0%", "0% - 8%", "8% - 16%", "16% - 24%"),
                     include.lowest=TRUE)
#carb[is.na(surf_traitement), .N, by=`ID Essai`]
prc=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges]
prc_fix=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix]

#percentage of surface not changing SOC with bioeconomy because residues are already being exported for other endings
nul=carb[change_scenbase_t100==0]
nul_surf=sum(nul$surf_traitement, na.rm=TRUE)
nul_prc=nul[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100)]

#C converted to CO2, if negative it indicates extra CO2 emitted, if positive it indicates that CO2 has been kept in soil as SOC 
carb[, CO2_delta_scenbase_t100_surf:=delta_scenbase_t100_surf*44/12]
carb[, CO2_weighted_avrg_delta_scenbase_t100_surf:=weighted.mean(CO2_delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

# average delta_scenbase_t100_surf per simulation unit by ID Essai
carb[, weighted_avrg_delta_tC_SU:=weighted.mean(delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]
carb[, weighted_avrg_change:=weighted.mean(change_scenbase_t100, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

###PCU scale###
carb[, Total_delta_tC_PCU:=sum(delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, Total_SOC_fbase_PCU:=sum(Cstock_base_final*surf_traitement_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, w_avg_change_PCU:=Total_delta_tC_PCU/Total_SOC_fbase_PCU]
carb[, CO2_delta_scenbase_t100_surf_UPC:=delta_scenbase_t100_surf_UPC*44/12]
carb[, Total_CO2_delta_scenbase_t100_surf_UPC:=sum(CO2_delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]

# get breaks for carbon ratios_ average
bneg=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU < 0], 4) # 3 intervals
bpos=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU >= 0], 4) # 3 intervals
br=c(head(bneg, -1), bpos) # to check visually!!!
carb$cranges_avg=cut(carb$w_avg_change_PCU, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
prc_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_avg]

carb$cranges_fix_avg=cut(carb$w_avg_change_PCU,  breaks = c(-0.24, -0.16, -0.08, -0.00000001, 0, 0.00000001, 0.08, 0.16, 0.24), 
                         labels = c("-24% - -16%","-16 - %-8%", "-8% - -0.000001%", "-0.000001% - 0%", "0% - 0%", "0% - 8%", "8% - 16%", "16% - 24%"),
                         include.lowest=TRUE)
prc_fix_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix_avg]

# saving results and removing tables in R

if (dir.exists("Results_avg") == FALSE) {
  dir.create("Results_avg")
}

write_delim(carb,paste("Results_avg/", paste("Results_avg",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
carb1=carb %>% distinct(`ID Essai`, .keep_all = TRUE)
write_delim(carb1,paste("Results_avg/", paste("Results_weighted_avg",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")


# compiling outputs for Hydrochar and  Digestate LESS EXPORTING RATE - Posible increase and decrease # ---------------------------------------------
setwd("path/to/your/directory:/All_Results")

simulated_scenarios <- c("hydrochar")

export_perc <- 50
sim_scen <-"hydrochar"

setwd(paste("path/to/your/directory/All_Results/",sim_scen,"/",sep=""))
carb_0 <- fread(paste("Results_",sim_scen,"_exp",export_perc,".csv",sep=""))

#removing SU where we don't know crop yields
carb2=carb_0[suyield,on=.('ID Traitement'=ID), allow.cartesian=TRUE]

#removing SU where we don't know crop yields nor meteo
carb=carb2[Temperature!=0]
carb[,c("UPC"):=NULL]

range(carb$change_scenbase_t100)

###Simulation uNits scale###
# get breaks for carbon ratios
bneg=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 < 0], 4) # 3 intervals
bpos=getJenksBreaks(carb$change_scenbase_t100[carb$change_scenbase_t100 >= 0], 4) # 3 intervals
#br=c(bneg, head(bpos, -1)) # for only negative  changes case!!!
br=c(head(bneg, -1), bpos) # for positive and negative changes case!!!
carb$cranges=cut(carb$change_scenbase_t100, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
totsurf=sum(carb$surf_traitement, na.rm=TRUE)
carb$cranges_fix=cut(carb$change_scenbase_t100,  breaks = c(-0.24, -0.16, -0.08, -0.00001, 0, 0.000001, 0.08, 0.16, 0.24), 
                     labels = c("-24% - -16%","-16 - %-8%", "-8% - -1%", "-1% - 0%", "0% - 0%", "0% - 8%", "8% - 16%", "16% - 24%"),
                     include.lowest=TRUE)
#carb[is.na(surf_traitement), .N, by=`ID Essai`]
prc=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges]
prc_fix=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix]

#percentage of surface not changing SOC with bioeconomy because residues are already being exported for other endings
nul=carb[change_scenbase_t100==0]
nul_surf=sum(nul$surf_traitement, na.rm=TRUE)
nul_prc=nul[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100)]

#C converted to CO2, if negative it indicates extra CO2 emitted, if positive it indicates that CO2 has been kept in soil as SOC 
carb[, CO2_delta_scenbase_t100_surf:=delta_scenbase_t100_surf*44/12]
carb[, CO2_weighted_avrg_delta_scenbase_t100_surf:=weighted.mean(CO2_delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

# average delta_scenbase_t100_surf per simulation unit by ID Essai
carb[, weighted_avrg_delta_tC_SU:=weighted.mean(delta_scenbase_t100_surf, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]
carb[, weighted_avrg_change:=weighted.mean(change_scenbase_t100, coef_pond_upc, na.rm=TRUE), by=`ID Essai`]

###PCU scale###
carb[, Total_delta_tC_PCU:=sum(delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, Total_SOC_fbase_PCU:=sum(Cstock_base_final*surf_traitement_UPC, na.rm=TRUE), by=`ID Essai`]
carb[, w_avg_change_PCU:=Total_delta_tC_PCU/Total_SOC_fbase_PCU]
carb[, CO2_delta_scenbase_t100_surf_UPC:=delta_scenbase_t100_surf_UPC*44/12]
carb[, Total_CO2_delta_scenbase_t100_surf_UPC:=sum(CO2_delta_scenbase_t100_surf_UPC, na.rm=TRUE), by=`ID Essai`]

# get breaks for carbon ratios_ average
bneg=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU < 0], 4) # 3 intervals
bpos=getJenksBreaks(carb$w_avg_change_PCU[carb$w_avg_change_PCU >= 0], 4) # 3 intervals
#br=c(bneg, head(bpos, -1)) # for only negative  changes case!!!
br=c(head(bneg, -1), bpos) # for positive and negative changes case!!!
carb$cranges_avg=cut(carb$w_avg_change_PCU, br, paste0(round(head(br, -1)*100, 2), "% - ", round(br[-1]*100, 2), "%"), include.lowest=TRUE)
prc_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_avg]

carb$cranges_fix_avg=cut(carb$w_avg_change_PCU,  breaks = c(-0.24, -0.16, -0.08, -0.00001, 0, 0.000001, 0.08, 0.16, 0.24), 
                         labels = c("-24% - -16%","-16 - %-8%", "-8% - -1%", "-1% - 0%", "0% - 0%", "0% - 8%", "8% - 16%", "16% - 24%"),
                         include.lowest=TRUE)
prc_fix_avg=carb[, .(perc_surf=sum(surf_traitement, na.rm=TRUE)/totsurf*100), by=cranges_fix_avg]

# saving results and removing tables in R

if (dir.exists("Results_avg") == FALSE) {
  dir.create("Results_avg")
}

write_delim(carb,paste("Results_avg/", paste("Results_avg",sim_scen,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
carb1=carb %>% distinct(`ID Essai`, .keep_all = TRUE)
write_delim(carb1,paste("Results_avg/", paste("Results_weighted_avg",sim_scen,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
