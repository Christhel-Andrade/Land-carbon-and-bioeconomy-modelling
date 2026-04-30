######## Combining output files from AMG-Cambioscop ###################
# H. Clivot, C. Andrade. --2021--

# Load libraries ---------------------------------------------------------------------

library(filesstrings)
library(tidyverse)

# Moving output data into 'AMG_outputs' folder and  input data into 'Simulated_input_files' folder----------------------------------------------------------------------

setwd("path/to/your/directory") # setting working directory

if (dir.exists("AMG_outputs") == FALSE) {
  dir.create("AMG_outputs")
}

if (dir.exists("Simulated_input_files") == FALSE) {
  dir.create("Simulated_input_files")
}

list_files <- list.files(pattern = "sortie-BDD", all.files = FALSE, full.names = FALSE) # creating a list from all output files
move_files(list_files,"AMG_outputs", overwrite = TRUE)

list_files_BDD <- list.files(pattern = "BDD_", all.files = FALSE, full.names = FALSE) # creating a list from all input (i.e. BDD) files
move_files(list_files_BDD,"Simulated_input_files", overwrite = TRUE)

# Storing already simulated input data into respective folders ----------------------------------------------------------------------

setwd("path/to/your/directory/Simulated_input_files")

sensitivity_analysis <- "no" #state if yes or no

simulated_scenarios <- c("baseline","biochar","digestate","molasses","gaschar","hydrochar")
simulated_scenarios_SA <- c("biochar1","digestate","biochar2","hydrochar","gaschar","molasses")
sim_scen <-""

if (sensitivity_analysis == "yes"){

  conversion_levels <- c("L","M","H")
  recalcitrance_levels <- c("L","M","H")
  conv_level <- ""
  recalci_level <- ""
  
  if (dir.exists("Sensitivity_analysis_inputs") == FALSE) {
    dir.create("Sensitivity_analysis_inputs")
  }
  
  list_files_SA <- list.files(pattern = paste("SA_conv",sep=""), all.files = FALSE, full.names = FALSE)
  move_files(list_files_SA,paste("Sensitivity_analysis_inputs",sep=""), overwrite = TRUE)
  setwd("path/to/your/directory/Simulated_input_files/Sensitivity_analysis_inputs")
      
  for (sim_scen in simulated_scenarios_SA){
    if (dir.exists(paste(sim_scen,sep="")) == FALSE) {
      dir.create(paste(sim_scen,sep=""))
    }
    list_files_sim <- list.files(pattern = paste("_",sim_scen,sep=""), all.files = FALSE, full.names = FALSE)
    move_files(list_files_sim,paste(sim_scen,sep=""), overwrite = TRUE)
  }
  
}else{
  
  for (sim_scen in simulated_scenarios){
    setwd("Epath/to/your/directory/Simulated_input_files")
    if (dir.exists(paste(sim_scen,sep="")) == FALSE) {
      dir.create(paste(sim_scen,sep=""))
    }
    list_files_sim <- list.files(pattern = paste("rac_AIAL_",sim_scen,sep=""), all.files = FALSE, full.names = FALSE)
    move_files(list_files_sim,paste(sim_scen), overwrite = TRUE)
  }
}


######### Moving output data into respective folders and compiling results----------------------------------------------------------------------

### compiling BASELINE outputs---------------------------------
##################################################################

setwd("path/to/your/directory/AMG_outputs") # setting working directory

scenario <- "baseline"
export_perc <- 0

if (dir.exists("Baseline") == FALSE) {
  dir.create("Baseline")
}

list_files_baseline <- list.files(pattern = "AIAL_baseline", all.files = FALSE, full.names = FALSE)
move_files(list_files_baseline,"Baseline", overwrite = TRUE)
# 
setwd("path/to/your/directory/AMG_outputs/Baseline")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, data.table, stringr)

# get the file name
dir() %>% str_subset("\\BDD_") -> fn

# use parallel setting
(cl1 <- detectCores() %>%
    makeCluster()) %>%
  registerDoParallel()

# read and bind all files together
system.time({
  data_base <- foreach(
    i = fn,
    .packages = "data.table"
  ) %dopar%
    {
      fread(i, colClasses = "character")
    } %>%
    rbindlist(fill = TRUE)
})

# end of parallel work
stopCluster(cl1)
gc()

if (dir.exists("Outputs_comp") == FALSE) {
  dir.create("Outputs_comp")
}

write_delim(data_base,paste("Outputs_comp/", paste("BIG_TABLE_Results_",scenario,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
rm(data_base)
gc()

library(data.table)
data_base <- as.data.frame(fread(paste("Outputs_comp/", paste("BIG_TABLE_Results_",scenario,"_exp",export_perc,".csv",sep=""))))

data_base_t100 <- data_base[,1:83] %>% filter(Temps == 100) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) 
names(data_base_t100)[3] <- "Cstock_base_final"
rm(data_base)

if (dir.exists("Outputs_comp") == FALSE) {
  dir.create("Outputs_comp")
}

write_delim(data_base_t100,paste("Outputs_comp/", "Results_baseline_sim1.csv", sep=""), delim = ";", na = "")
rm(data_base_t100)

###BAU 2120 vs 2020###

data_base_t100 <- data_base[,1:83] %>% filter(Temps == 100) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0)
names(data_base_t100)[3] <- "Cstock_base_final"
data_base_t0<- data_base[,1:83] %>% filter(Temps == 0) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
names(data_base_t0)[3] <- "Cstock_base_ini"
data_base_mean <- data_base[,1:83] %>% select(`ID Essai`,`ID Traitement`,`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`) %>%
  mutate_at(vars(`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`), as.numeric) %>% replace(is.na(.), 0) %>%  group_by(`ID Essai`,`ID Traitement`) %>%  summarize_all(funs(mean))
data_base_t0t100 <- left_join(data_base_t0,data_base_t100) %>% replace(is.na(.), 0)
data_base_comp <- left_join(data_base_mean,data_base_t0t100)  


data_surface <- as.data.frame(fread("path/to/your/directory/AMG_outputs/surface_pond.csv")) #surface csv
names(data_surface)[1] <- "ID Essai"
names(data_surface)[2] <- "ID Traitement"
data_surface2 <- data_surface %>% select("ID Essai","ID Traitement","surf_UPC","surf_upc_uts_seq","coef_pond_upc") %>%
  mutate(surf_traitement = surf_upc_uts_seq*coef_pond_upc)
rm(data_surface)


data_base_comp <- left_join(data_bioeco_comp,data_base_t100)
data_base_comp[,3:14] <- sapply(data_base_comp[,3:14],as.numeric)
data_base_comp <- data_base_comp %>% replace(is.na(.), 0) %>% 
  mutate(delta_t100t0_base = Cstock_base_final- Cstock_base_ini) %>%
  mutate(change_base_t100vst0 = (Cstock_base_final- Cstock_base_ini)/ Cstock_base_ini)
  
data_base_comp <- left_join(data_base_comp,data_surface2)
data_base_comp <- data_base_comp %>% mutate(delta_t100t0_base_surf = delta_t100t0_base*surf_traitement) %>%
  mutate(surf_traitement_UPC = surf_UPC*coef_pond_upc) %>%
  mutate(delta_t100t0_base_surf_UPC = delta_t100t0_base*surf_traitement_UPC)


if (dir.exists("Outputs_comp") == FALSE) {
  dir.create("Outputs_comp")
}

write_delim(data_base_comp,paste("Outputs_comp/", "Results_Baseline.csv", sep=""), delim = ";", na = "")

rm(data_base,data_base_t0,data_base_t100,data_base_t0t100,data_base_mean)
gc()



###### bioeconomy scenarios : one example is detailed below
####################################################################################

setwd("path/to/your/directory/AMG_outputs")

scenario <- "molasses" #("biochar1","digestate","biochar2","hydrochar","gaschar","molasses")
export_perc <- 50 #100, 75, 50

if (dir.exists(paste(scenario,"_exp",export_perc,sep="")) == FALSE) {
  dir.create(paste(scenario,"_exp",export_perc,sep=""))
}

list_files_bioeco <- list.files(pattern = paste("_",scenario,"_exp",export_perc,sep=""), all.files = FALSE, full.names = FALSE)
move_files(list_files_bioeco,paste(scenario,"_exp",export_perc,sep=""), overwrite = TRUE)
rm (list_files_bioeco)

setwd(paste("path/to/your/directory/AMG_outputs/",scenario,"_exp",export_perc,sep=""))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, data.table, stringr)

# get the file name
dir() %>% str_subset("\\BDD_") -> fn

# use parallel setting
(cl1 <- detectCores() %>%
    makeCluster()) %>%
  registerDoParallel()

# read and bind all files together
system.time({
  data_bioeco <- foreach(
    i = fn,
    .packages = "data.table"
  ) %dopar%
    {
      fread(i, colClasses = "character")
    } %>%
    rbindlist(fill = TRUE)
})

# end of parallel work

stopCluster(cl1)
gc()

if (dir.exists("Outputs_comp") == FALSE) {
  dir.create("Outputs_comp")
}

write_delim(data_bioeco,paste("Outputs_comp/", paste("BIG_TABLE_Results_",scenario,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
rm(data_bioeco)
gc()

library(data.table)
data_bioeco <- as.data.frame(fread(paste("Outputs_comp/", paste("BIG_TABLE_Results_",scenario,"_exp",export_perc,".csv",sep=""))))

data_bioeco2_mean <- data_bioeco[,1:83] %>% select(`ID Essai`,`ID Traitement`,`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`) %>%
  mutate_at(vars(`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`), as.numeric) %>% replace(is.na(.), 0) %>%  group_by(`ID Essai`,`ID Traitement`) %>%  summarize_all(funs(mean))

data_bioeco_t100 <- data_bioeco[,1:83] %>% filter(Temps == 100) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
names(data_bioeco_t100)[3] <- "Cstock_final"
data_bioeco_t0<- data_bioeco[,1:83] %>% filter(Temps == 0) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
names(data_bioeco_t0)[3] <- "Cstock_ini"

data_bioeco_t0t100 <- left_join(data_bioeco_t0,data_bioeco_t100)
data_bioeco_comp <- left_join(data_bioeco2_mean,data_bioeco_t0t100)
rm(data_bioeco_t0,data_bioeco_t100,data_bioeco_t0t100,data_bioeco2_mean,data_bioeco)
gc()

# retrieving baseline and surface data

data_base_t100 <- as.data.frame(fread("your_unit/AMG_outputs/Baseline/Outputs_comp/Results_baseline_sim1.csv"))

data_surface <- as.data.frame(fread("your_unit/surface_pond.csv"))

names(data_surface)[1] <- "ID Essai"
names(data_surface)[2] <- "ID Traitement"
data_surface2 <- data_surface %>% select("ID Essai","ID Traitement","surf_UPC","surf_upc_uts_seq","coef_pond_upc") %>%
  mutate(surf_traitement = surf_upc_uts_seq*coef_pond_upc)
rm(data_surface)


# combining scenario data with baseline and surface data

data_bioeco_comp <- left_join(data_bioeco_comp,data_base_t100)
data_bioeco_comp[,3:15] <- sapply(data_bioeco_comp[,3:15],as.numeric)
data_bioeco_comp <- data_bioeco_comp %>% replace(is.na(.), 0) %>% 
                    mutate(delta_t100t0_base = Cstock_base_final- Cstock_ini) %>%
                    mutate(delta_t100t0_scen = Cstock_final- Cstock_ini) %>%
                    mutate(delta_scenbase_t100 = Cstock_final- Cstock_base_final) %>%
                    mutate(change_base_t100vst0 = (Cstock_base_final- Cstock_ini)/ Cstock_ini)%>%
                    mutate(change_scen_t100vst0 = (Cstock_final- Cstock_ini)/ Cstock_ini) %>%
                    mutate(change_scenbase_t100 = (Cstock_final- Cstock_base_final)/Cstock_base_final)

data_bioeco_comp <- left_join(data_bioeco_comp,data_surface2)
data_bioeco_comp <- data_bioeco_comp %>% mutate(delta_scenbase_t100_surf = delta_scenbase_t100*surf_traitement) %>%
                    mutate(surf_traitement_UPC = surf_UPC*coef_pond_upc) %>%
                    mutate(delta_scenbase_t100_surf_UPC = delta_scenbase_t100*surf_traitement_UPC)


# saving results and removing tables in R

if (dir.exists("Outputs_comp") == FALSE) {
  dir.create("Outputs_comp")
}

write_delim(data_bioeco_comp,paste("Outputs_comp/", paste("Results_",scenario,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
rm(data_base_t100,data_bioeco_comp,data_surface2)


###### bioeconomy scenarios : C limit application
####################################################################################

setwd("path/to/your/directory/AMG_outputs")

scenario <- "gaschar"
export_perc <- 100

if (dir.exists(paste(scenario,"_exp",export_perc,"_limitC",sep="")) == FALSE) {
  dir.create(paste(scenario,"_exp",export_perc,"_limitC",sep=""))
}

list_files_bioeco_limitC <- list.files(pattern = paste("_",scenario,"_exp",export_perc,"_limitC",sep=""), all.files = FALSE, full.names = FALSE)
move_files(list_files_bioeco_limitC,paste(scenario,"_exp",export_perc,"_limitC",sep=""), overwrite = TRUE)
rm (list_files_bioeco_limitC)

setwd(paste("path/to/your/directory/AMG_outputs/",scenario,"_exp",export_perc,"_limitC",sep=""))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, data.table, stringr)

# get the file name
dir() %>% str_subset("\\BDD_") -> fn

# use parallel setting
(cl1 <- detectCores() %>%
    makeCluster()) %>%
  registerDoParallel()

# read and bind all files together
system.time({
  data_bioeco <- foreach(
    i = fn,
    .packages = "data.table"
  ) %dopar%
    {
      fread(i, colClasses = "character")
    } %>%
    rbindlist(fill = TRUE)
})

# end of parallel work

stopCluster(cl1)
gc()

if (dir.exists("Outputs_comp") == FALSE) {
  dir.create("Outputs_comp")
}

write_delim(data_bioeco,paste("Outputs_comp/", paste("BIG_TABLE_Results_",scenario,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
rm(data_bioeco)
gc()

library(data.table)
data_bioeco <- as.data.frame(fread(paste("Outputs_comp/", paste("BIG_TABLE_Results_",scenario,"_exp",export_perc,".csv",sep=""))))

data_bioeco2_mean <- data_bioeco[,1:83] %>% select(`ID Essai`,`ID Traitement`,`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`) %>%
  mutate_at(vars(`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`), as.numeric) %>% replace(is.na(.), 0) %>%  group_by(`ID Essai`,`ID Traitement`) %>%  summarize_all(funs(mean))

data_bioeco_t100 <- data_bioeco[,1:83] %>% filter(Temps == 100) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
names(data_bioeco_t100)[3] <- "Cstock_final"
data_bioeco_t0<- data_bioeco[,1:83] %>% filter(Temps == 0) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
names(data_bioeco_t0)[3] <- "Cstock_ini"

data_bioeco_t0t100 <- left_join(data_bioeco_t0,data_bioeco_t100)
data_bioeco_comp <- left_join(data_bioeco2_mean,data_bioeco_t0t100)
rm(data_bioeco_t0,data_bioeco_t100,data_bioeco_t0t100,data_bioeco2_mean,data_bioeco)
gc()

# retrieving baseline and surface data

data_base_t100 <- as.data.frame(fread("path/to/your/directory/AMG_outputs/Baseline/Outputs_comp/Results_baseline_sim1.csv"))

data_surface <- as.data.frame(fread("path/to/your/directory/AMG_outputs/surface_pond.csv"))

names(data_surface)[1] <- "ID Essai"
names(data_surface)[2] <- "ID Traitement"
data_surface2 <- data_surface %>% select("ID Essai","ID Traitement","surf_UPC","surf_upc_uts_seq","coef_pond_upc") %>%
  mutate(surf_traitement = surf_upc_uts_seq*coef_pond_upc)
rm(data_surface)


# combining scenario data with baseline and surface data

data_bioeco_comp <- left_join(data_bioeco_comp,data_base_t100)
data_bioeco_comp[,3:15] <- sapply(data_bioeco_comp[,3:15],as.numeric)
data_bioeco_comp <- data_bioeco_comp %>% replace(is.na(.), 0) %>% 
  mutate(delta_t100t0_base = Cstock_base_final- Cstock_ini) %>%
  mutate(delta_t100t0_scen = Cstock_final- Cstock_ini) %>%
  mutate(delta_scenbase_t100 = Cstock_final- Cstock_base_final) %>%
  mutate(change_base_t100vst0 = (Cstock_base_final- Cstock_ini)/ Cstock_ini)%>%
  mutate(change_scen_t100vst0 = (Cstock_final- Cstock_ini)/ Cstock_ini) %>%
  mutate(change_scenbase_t100 = (Cstock_final- Cstock_base_final)/Cstock_base_final)

data_bioeco_comp <- left_join(data_bioeco_comp,data_surface2)
data_bioeco_comp <- data_bioeco_comp %>% mutate(delta_scenbase_t100_surf = delta_scenbase_t100*surf_traitement) %>%
  mutate(surf_traitement_UPC = surf_UPC*coef_pond_upc) %>%
  mutate(delta_scenbase_t100_surf_UPC = delta_scenbase_t100*surf_traitement_UPC)


# saving results and removing tables in R

if (dir.exists("Outputs_comp") == FALSE) {
  dir.create("Outputs_comp")
}

write_delim(data_bioeco_comp,paste("Outputs_comp/", paste("Results_",scenario,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
rm(data_base_t100,data_bioeco_comp,data_surface2)


### SENSITIVITY ANALYSIS outputs
##################################################################

setwd("path/to/your/directory/AMG_outputs")

sensitivity_analysis <- "yes"

simulated_scenarios <- c("baseline","biochar","digestate")
#simulated_scenarios_SA <- c("biochar1","digestate","biochar2","hydrochar","gaschar","molasses")
simulated_scenarios_SA <- c("molasses")
sim_scen <-""

if (sensitivity_analysis == "yes"){

  conversion_levels <- c("L","M","H")
  recalcitrance_levels <- c("L","M","H")
  conv_level <- ""
  recalci_level <- ""
  
  if (dir.exists("Sensitivity_analysis_outputs") == FALSE) {
    dir.create("Sensitivity_analysis_outputs")
  }
  
  list_files_SA <- list.files(pattern = paste("SA_conv",sep=""), all.files = FALSE, full.names = FALSE)
  move_files(list_files_SA,paste("Sensitivity_analysis_outputs",sep=""), overwrite = TRUE)
  
  for (sim_scen in simulated_scenarios_SA){
    setwd("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs")
    if (dir.exists(paste(sim_scen,sep="")) == FALSE) {
      dir.create(paste(sim_scen,sep=""))
    }
    list_files_sim <- list.files(pattern = paste("_",sim_scen,sep=""), all.files = FALSE, full.names = FALSE)
    move_files(list_files_sim,paste(sim_scen,sep=""), overwrite = TRUE)
    
    for (conv_level in conversion_levels){
      setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs/",sim_scen,sep=""))
      if (dir.exists(paste("conversion_",conv_level,sep="")) == FALSE) {
        dir.create(paste("conversion_",conv_level,sep=""))
      }
      list_files_sim <- list.files(pattern = paste("SA_conv",conv_level,sep=""), all.files = FALSE, full.names = FALSE)
      move_files(list_files_sim,paste("conversion_",conv_level,sep=""), overwrite = TRUE)
      
      for (recalci_level in recalcitrance_levels){
        setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs/",sim_scen,"/","conversion_",conv_level,sep=""))
        if (dir.exists(paste("recalcitrance_",recalci_level,sep="")) == FALSE) {
          dir.create(paste("recalcitrance_",recalci_level,sep=""))
        }
        list_files_sim <- list.files(pattern = paste("SA_conv",conv_level,"_",sim_scen,recalci_level,sep=""), all.files = FALSE, full.names = FALSE)
        move_files(list_files_sim,paste("recalcitrance_",recalci_level,sep=""), overwrite = TRUE)
        
      }
    }
  }
}


# retrieving baseline and surface data

data_base_t100 <- read_delim("path/to/your/directory/AMG_outputs/Baseline/Outputs_comp/Results_baseline_sim1.csv",delim = ";")

data_surface <- read_delim("path/to/your/directory/AMG_outputs/surface_pond.csv",delim = ";")
names(data_surface)[1] <- "ID Essai"
names(data_surface)[2] <- "ID Traitement"
data_surface2 <- data_surface %>% select("ID Essai","ID Traitement","surf_UPC","surf_upc_uts_seq","coef_pond_upc") %>%
  mutate(surf_traitement = surf_upc_uts_seq*coef_pond_upc)
rm(data_surface)

# compiling outputs for SA

setwd("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs")

library(foreach)
library(doParallel)
numCores <- detectCores()
numCores
cl1<-makeCluster(numCores-2, outfile = "debug.txt")
registerDoParallel(cl1)


#simulated_scenarios_SA <- c("biochar1","digestate","biochar2","hydrochar","gaschar","molasses")
simulated_scenarios_SA <- c("molasses")

export_perc <- 100
conversion_levels <- c("L","M","H")
recalcitrance_levels <- c("L","M","H")
sim_scen <-""
conv_level <- ""
recalci_level <- ""


foreach (sim_scen = simulated_scenarios_SA,.packages = c("tidyverse", "readxl","scales","readr")) %dopar% {
  
  for (conv_level in conversion_levels){
    for (recalci_level in recalcitrance_levels){

      setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs/",sim_scen,"/","conversion_",conv_level,"/recalcitrance_",recalci_level,sep=""))
            
      if (!require("pacman")) install.packages("pacman")
      pacman::p_load(doParallel, data.table, stringr)
      
      # get the file name
      dir() %>% str_subset("\\BDD_") -> fn
      
      # use parallel setting
      (cl1 <- detectCores() %>%
          makeCluster()) %>%
        registerDoParallel()
      
      # read and bind all files together
      system.time({
        data_bioeco <- foreach(
          i = fn,
          .packages = "data.table"
        ) %dopar%
          {
            fread(i, colClasses = "character")
          } %>%
          rbindlist(fill = TRUE)
      })
      
      # end of parallel work
      
      stopCluster(cl1)
      gc()
      
      if (dir.exists("Outputs_comp") == FALSE) {
        dir.create("Outputs_comp")
      }
      
      #for (sim_scen in simulated_scenarios_SA){
        #for (conv_level in conversion_levels){
         # for (recalci_level in recalcitrance_levels){
          
      write_delim(data_bioeco,paste("Outputs_comp/", paste("BIG_TABLE_Results_",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
      rm(data_bioeco)
      gc()
          }
        }
      }
      
library(data.table)

#simulated_scenarios_SA <- c("biochar1","digestate","biochar2","hydrochar","gaschar","molasses")
simulated_scenarios_SA <- c("molasses")

export_perc <- 100
conversion_levels <- c("L","M","H")
recalcitrance_levels <- c("L","M","H")
sim_scen <-""
conv_level <- ""
recalci_level <- ""

foreach (sim_scen = simulated_scenarios_SA,.packages = c("tidyverse", "readxl","scales","readr")) %dopar% {
  
  for (conv_level in conversion_levels){
    for (recalci_level in recalcitrance_levels){
      
      setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs/",sim_scen,"/","conversion_",conv_level,"/recalcitrance_",recalci_level,sep=""))
      
      data_bioeco <- as.data.frame(fread(paste("Outputs_comp/", paste("BIG_TABLE_Results_",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep=""))))

      
      data_bioeco2_mean <- data_bioeco[,1:83] %>% select(`ID Essai`,`ID Traitement`,`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`) %>%
        mutate_at(vars(`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`), as.numeric) %>% replace(is.na(.), 0) %>%  group_by(`ID Essai`,`ID Traitement`) %>%  summarize_all(funs(mean))
      
      data_bioeco_t100 <- data_bioeco[,1:83] %>% filter(Temps == 100) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
      names(data_bioeco_t100)[3] <- "Cstock_final"
      data_bioeco_t0<- data_bioeco[,1:83] %>% filter(Temps == 0) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
      names(data_bioeco_t0)[3] <- "Cstock_ini"
      
      data_bioeco_t0t100 <- left_join(data_bioeco_t0,data_bioeco_t100)
      data_bioeco_comp <- left_join(data_bioeco2_mean,data_bioeco_t0t100)
      rm(data_bioeco_t0,data_bioeco_t100,data_bioeco_t0t100,data_bioeco2_mean,data_bioeco)
      gc()
      
      # retrieving baseline and surface data
      
      data_base_t100 <- as.data.frame(fread("path/to/your/directory/AMG_outputs/Baseline/Outputs_comp/Results_baseline_sim1.csv"))
      
      data_surface <- as.data.frame(fread("path/to/your/directory/AMG_outputs/surface_pond.csv"))
      
      names(data_surface)[1] <- "ID Essai"
      names(data_surface)[2] <- "ID Traitement"
      data_surface2 <- data_surface %>% select("ID Essai","ID Traitement","surf_UPC","surf_upc_uts_seq","coef_pond_upc") %>%
        mutate(surf_traitement = surf_upc_uts_seq*coef_pond_upc)
      rm(data_surface)
      
      
      # combining scenario data with baseline and surface data
      
      data_bioeco_comp <- left_join(data_bioeco_comp,data_base_t100)
      data_bioeco_comp[,3:15] <- sapply(data_bioeco_comp[,3:15],as.numeric)
      data_bioeco_comp <- data_bioeco_comp %>% replace(is.na(.), 0) %>% 
        mutate(delta_t100t0_base = Cstock_base_final- Cstock_ini) %>%
        mutate(delta_t100t0_scen = Cstock_final- Cstock_ini) %>%
        mutate(delta_scenbase_t100 = Cstock_final- Cstock_base_final) %>%
        mutate(change_base_t100vst0 = (Cstock_base_final- Cstock_ini)/ Cstock_ini)%>%
        mutate(change_scen_t100vst0 = (Cstock_final- Cstock_ini)/ Cstock_ini)%>%
        mutate(change_scenbase_t100 = (Cstock_final- Cstock_base_final)/Cstock_base_final)
      
      data_bioeco_comp <- left_join(data_bioeco_comp,data_surface2)
      data_bioeco_comp <- data_bioeco_comp %>% mutate(delta_scenbase_t100_surf = delta_scenbase_t100*surf_traitement) %>%
        mutate(surf_traitement_UPC = surf_UPC*coef_pond_upc) %>%
        mutate(delta_scenbase_t100_surf_UPC = delta_scenbase_t100*surf_traitement_UPC)
      
      
      # saving results and removing tables in R
      
      if (dir.exists("Outputs_comp") == FALSE) {
        dir.create("Outputs_comp")
      }
      
      write_delim(data_bioeco_comp,paste("Outputs_comp/", paste("Results_",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
      rm(data_base_t100,data_bioeco_comp,data_surface2)
      
      
    }
  }
}

rm(data_base_t100,data_surface2)
stopCluster(cl1)


####Limit C application SA###

setwd("path/to/your/directory/AMG_outputs")

sensitivity_analysis <- "yes"

simulated_scenarios <- c("baseline","biochar","gaschar")
#simulated_scenarios_SA <- c("biochar1","biochar2", "gaschar")
simulated_scenarios_SA <- c("gaschar")
sim_scen <-""

if (sensitivity_analysis == "yes"){
  
  conversion_levels <- c("L","M","H")
  recalcitrance_levels <- c("L","M","H")
  conv_level <- ""
  recalci_level <- ""
  
  if (dir.exists("Sensitivity_analysis_outputs_limitC") == FALSE) {
    dir.create("Sensitivity_analysis_outputs_limitC")
  }
  
  list_files_SA <- list.files(pattern = paste("SA_conv",sep=""), all.files = FALSE, full.names = FALSE)
  move_files(list_files_SA,paste("Sensitivity_analysis_outputs_limitC",sep=""), overwrite = TRUE)
  
  for (sim_scen in simulated_scenarios_SA){
    setwd("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs_limitC")
    if (dir.exists(paste(sim_scen,sep="")) == FALSE) {
      dir.create(paste(sim_scen,sep=""))
    }
    list_files_sim <- list.files(pattern = paste("_",sim_scen,sep=""), all.files = FALSE, full.names = FALSE)
    move_files(list_files_sim,paste(sim_scen,sep=""), overwrite = TRUE)
   
    for (conv_level in conversion_levels){
      setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs_limitC/",sim_scen,sep=""))
      if (dir.exists(paste("conversion_",conv_level,sep="")) == FALSE) {
        dir.create(paste("conversion_",conv_level,sep=""))
      }
      list_files_sim <- list.files(pattern = paste("SA_conv",conv_level,sep=""), all.files = FALSE, full.names = FALSE)
      move_files(list_files_sim,paste("conversion_",conv_level,sep=""), overwrite = TRUE)
      
      for (recalci_level in recalcitrance_levels){
        setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs_limitC/",sim_scen,"/","conversion_",conv_level,sep=""))
        if (dir.exists(paste("recalcitrance_",recalci_level,sep="")) == FALSE) {
          dir.create(paste("recalcitrance_",recalci_level,sep=""))
        }
        list_files_sim <- list.files(pattern = paste("SA_conv",conv_level,"_",sim_scen,recalci_level,sep=""), all.files = FALSE, full.names = FALSE)
        move_files(list_files_sim,paste("recalcitrance_",recalci_level,sep=""), overwrite = TRUE)
      }
    }
  }
}


# retrieving baseline and surface data

data_base_t100 <- read_delim("path/to/your/directory/AMG_outputs/Baseline/Outputs_comp/Results_baseline_sim1.csv",delim = ";")

data_surface <- read_delim("path/to/your/directory/AMG_outputs/surface_pond.csv",delim = ";")
names(data_surface)[1] <- "ID Essai"
names(data_surface)[2] <- "ID Traitement"
data_surface2 <- data_surface %>% select("ID Essai","ID Traitement","surf_UPC","surf_upc_uts_seq","coef_pond_upc") %>%
  mutate(surf_traitement = surf_upc_uts_seq*coef_pond_upc)
rm(data_surface)

# compiling outputs for SA

setwd("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs_limitC")

library(foreach)
library(doParallel)
numCores <- detectCores()
numCores
cl1<-makeCluster(numCores-2, outfile = "debug.txt")
registerDoParallel(cl1)

#simulated_scenarios_SA <- c("biochar1","digestate","biochar2","hydrochar","gaschar","molasses")
simulated_scenarios_SA <- c("gaschar")
export_perc <- 100
conversion_levels <- c("L","M","H")
recalcitrance_levels <- c("L","M","H")
sim_scen <-""
conv_level <- ""
recalci_level <- ""


foreach (sim_scen = simulated_scenarios_SA,.packages = c("tidyverse", "readxl","scales","readr")) %dopar% {
  
  for (conv_level in conversion_levels){
    for (recalci_level in recalcitrance_levels){
      
      setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs_limitC/",sim_scen,"/","conversion_",conv_level,"/recalcitrance_",recalci_level,sep=""))
            
      if (!require("pacman")) install.packages("pacman")
      pacman::p_load(doParallel, data.table, stringr)
      
      # get the file name
      dir() %>% str_subset("\\BDD_") -> fn
      
      # use parallel setting
      (cl1 <- detectCores() %>%
          makeCluster()) %>%
        registerDoParallel()
      
      # read and bind all files together
      system.time({
        data_bioeco <- foreach(
          i = fn,
          .packages = "data.table"
        ) %dopar%
          {
            fread(i, colClasses = "character")
          } %>%
          rbindlist(fill = TRUE)
      })
      
      # end of parallel work
      
      stopCluster(cl1)
      gc()
      
      if (dir.exists("Outputs_comp") == FALSE) {
        dir.create("Outputs_comp")
      }
      
      #for (sim_scen in simulated_scenarios_SA){
      #for (conv_level in conversion_levels){
      # for (recalci_level in recalcitrance_levels){
      
      write_delim(data_bioeco,paste("Outputs_comp/", paste("BIG_TABLE_Results_",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
      rm(data_bioeco)
      gc()
    }
  }
}

library(data.table)

#simulated_scenarios_SA <- c("biochar1","digestate","biochar2","hydrochar","gaschar","molasses")
simulated_scenarios_SA <- c("gaschar")
export_perc <- 100
conversion_levels <- c("L","M","H")
recalcitrance_levels <- c("L","M","H")
sim_scen <-""
conv_level <- ""
recalci_level <- ""

foreach (sim_scen = simulated_scenarios_SA,.packages = c("tidyverse", "readxl","scales","readr")) %dopar% {
  
  for (conv_level in conversion_levels){
    for (recalci_level in recalcitrance_levels){
      
      setwd(paste("path/to/your/directory/AMG_outputs/Sensitivity_analysis_outputs_limitC/",sim_scen,"/","conversion_",conv_level,"/recalcitrance_",recalci_level,sep=""))
      
      data_bioeco <- as.data.frame(fread(paste("Outputs_comp/", paste("BIG_TABLE_Results_",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep=""))))
      
      
      data_bioeco2_mean <- data_bioeco[,1:83] %>% select(`ID Essai`,`ID Traitement`,`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`) %>%
        mutate_at(vars(`Temperature`,`P-ETP`,`Irrigation`,`Argile`,`CaCO3`,`pH`,`Masse de carbone du PRO`,`Cexport_baseline`,`Cexport_bioeco`,`Creturn`), as.numeric) %>% replace(is.na(.), 0) %>%  group_by(`ID Essai`,`ID Traitement`) %>%  summarize_all(funs(mean))
      
      data_bioeco_t100 <- data_bioeco[,1:83] %>% filter(Temps == 100) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
      names(data_bioeco_t100)[3] <- "Cstock_final"
      data_bioeco_t0<- data_bioeco[,1:83] %>% filter(Temps == 0) %>% select(`ID Essai`,`ID Traitement`,`Stock de carbone simule`) %>% replace(is.na(.), 0) 
      names(data_bioeco_t0)[3] <- "Cstock_ini"
      
      data_bioeco_t0t100 <- left_join(data_bioeco_t0,data_bioeco_t100)
      data_bioeco_comp <- left_join(data_bioeco2_mean,data_bioeco_t0t100)
      rm(data_bioeco_t0,data_bioeco_t100,data_bioeco_t0t100,data_bioeco2_mean,data_bioeco)
      gc()
      
      # retrieving baseline and surface data
      
      data_base_t100 <- as.data.frame(fread("path/to/your/directory/AMG_outputs/Baseline/Outputs_comp/Results_baseline_sim1.csv"))
      
      data_surface <- as.data.frame(fread("path/to/your/directory/AMG_outputs/surface_pond.csv"))
      
      names(data_surface)[1] <- "ID Essai"
      names(data_surface)[2] <- "ID Traitement"
      data_surface2 <- data_surface %>% select("ID Essai","ID Traitement","surf_UPC","surf_upc_uts_seq","coef_pond_upc") %>%
        mutate(surf_traitement = surf_upc_uts_seq*coef_pond_upc)
      rm(data_surface)
      
      
      # combining scenario data with baseline and surface data
      
      data_bioeco_comp <- left_join(data_bioeco_comp,data_base_t100)
      data_bioeco_comp[,3:15] <- sapply(data_bioeco_comp[,3:15],as.numeric)
      data_bioeco_comp <- data_bioeco_comp %>% replace(is.na(.), 0) %>% 
        mutate(delta_t100t0_base = Cstock_base_final- Cstock_ini) %>%
        mutate(delta_t100t0_scen = Cstock_final- Cstock_ini) %>%
        mutate(delta_scenbase_t100 = Cstock_final- Cstock_base_final) %>%
        mutate(change_base_t100vst0 = (Cstock_base_final- Cstock_ini)/ Cstock_ini)%>%
        mutate(change_scen_t100vst0 = (Cstock_final- Cstock_ini)/ Cstock_ini)%>%
        mutate(change_scenbase_t100 = (Cstock_final- Cstock_base_final)/Cstock_base_final)
      
      data_bioeco_comp <- left_join(data_bioeco_comp,data_surface2)
      data_bioeco_comp <- data_bioeco_comp %>% mutate(delta_scenbase_t100_surf = delta_scenbase_t100*surf_traitement) %>%
        mutate(surf_traitement_UPC = surf_UPC*coef_pond_upc) %>%
        mutate(delta_scenbase_t100_surf_UPC = delta_scenbase_t100*surf_traitement_UPC)
      
      
      # saving results and removing tables in R
      
      if (dir.exists("Outputs_comp") == FALSE) {
        dir.create("Outputs_comp")
      }
      
      write_delim(data_bioeco_comp,paste("Outputs_comp/", paste("Results_",sim_scen,"_conv",conv_level,"_recalc",recalci_level,"_exp",export_perc,".csv",sep="")), delim = ";", na = "")
      rm(data_base_t100,data_bioeco_comp,data_surface2)
      
      
    }
  }
}

rm(data_base_t100,data_surface2)
stopCluster(cl1)
