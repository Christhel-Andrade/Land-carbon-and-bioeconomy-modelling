#--Created: 2021, Author: Christhel Andrade--#
#### Loop applied on each file

start_time <- Sys.time()

cl1<-makeCluster(numCores-2, outfile = "debug.txt")
registerDoParallel(cl1)

foreach (PCU_code = list_files,.packages = c("data.table","dplyr","magrittr","readxl","scales","readr")) %dopar% {

  if (scenario=="Baseline"){
    export_perc <- 0
  } else {
    export_perc <- export_percentage
  }

  # yield
  options(dplyr.summarise.inform = FALSE)

  yield <- read.table(paste("TS_data_subset_",PCU_code,".csv", sep = ""),sep=";", dec=".",header=TRUE, na = "")

  names(yield)[1] <- "PCU"
  #yield <- yield %>% distinct(PCU, MONTH, TYPE, .keep_all = TRUE)
  #print(yield)

  # RPR and R/S coefficients
  RPR <- read_excel(path = paste0("Data",".xlsx"), sheet = "Data",col_names = TRUE, na = "") ## RPR value per crop type
  #print(RPR)

  # Beta coefficient for root distribution
  Beta <- read_excel(path = "coef_allomEC.xlsx", sheet = "Beta",col_names = TRUE, na = "")
  #print(Beta)

  #Harvest factor
  export <- read_excel(path = "coef_allomEC.xlsx", sheet = "export",col_names = TRUE, na = "")
  #print(export)

  # C inputs from RPR, R-S, and Biomass composition-----------------------------------------------------------------------
  C_inputs <- yield  %>%
    left_join(RPR, by = c("TYPE","CROP")) %>% # joining RPR values
    mutate(Yield_DM = YIELD * (1-HUM_Prod/100)) %>% # dry yield of product t/ha
    left_join(Beta, by = "CROP") %>% # joining harvest coefficients and Cc
    left_join(export, by = "CROP") %>% # joining harvest coefficients and Cc
    mutate(Facteur_export = ifelse((Residues=="Restituted" | is.na(Residues) == TRUE), 1, Facteur_export)) %>% # modif Facteur_export, 1 if residues restituted
    mutate(Yield_Res = Yield_DM*RPR) %>% # Dry biomass calculation t/ha
    mutate(Yield_Root = RS*(Yield_DM+Yield_Res)) %>% # Dry root mass calculation t/ha
    mutate(Energy_Pot = Yield_Res*LHV_res) %>% # Dry biomass energy potential -theoretical- GJ/ha
    mutate(Total_Energy_Pot = Energy_Pot*Area_ha) %>% # Dry biomass energy potential -theoretical- GJ
    mutate(Shoot_Root = 1/RS) %>% # shoot/root ratio
    mutate(R_Prod = Yield_DM/(Yield_DM+Yield_Res+Yield_Root)) %>% # Plant distribution ratio: product fraction
    mutate(R_Res = Yield_Res/(Yield_DM+Yield_Res+Yield_Root)) %>% # Plant distribution ratio: residue fraction
    mutate(R_Root = Yield_Root/(Yield_DM+Yield_Res+Yield_Root)) # Plant distribution ratio: root fraction

  ##### To actually calculate the root C that is returned to soil. Don't know beta values for Ecuadorian crops
  C_inputs <- C_inputs %>%
    mutate(CP = ifelse(TYPE=="CI", 0, Yield_DM*0.44)) %>% # C in main product
    mutate(CS = ifelse(TYPE=="CI", Yield_DM*C_res/100, Yield_Res*C_res/100)) %>% # C in aboveground residues
    mutate(CR = ifelse(TYPE=="CI", Yield_DM/Shoot_Root*C_root, Yield_DM/(Shoot_Root*(Yield_DM/(Yield_DM+Yield_Res)))*C_root))%>%  # C in roots
    mutate(CE = CR*0.65) %>% # extra-root carbon
    mutate(Root_C_30cm = (1-Beta^30)*CR) %>% # C in roots over 30 cm
    mutate(Extra_Root_C_30cm = Root_C_30cm*0.65) %>% # extra-root carbon over 30 cm
    mutate(Aboveground_C_input = Facteur_export*CS) %>% # C in residue returned to soil
    mutate(Belowground_C_input = Root_C_30cm+Extra_Root_C_30cm) # input of belowground carbon
  ########

  # Fichiers C_inputs par type de culture ---------------------------------------------------------------------

  C_inputs_CP <- C_inputs %>%
    filter(TYPE == "CP") %>%
    select(PCU,PROVINCE,MONTH,CROP,BareToVeg,Yield_DM,Aboveground_C_input,Belowground_C_input) %>%
    mutate(dpm_rpm_ab = 1.44) %>%  #as default for agricultural crops in RothC
    mutate(SoilDepth = 40) %>% # If tillage depth is known it can be added later when merging soil parameters. If unknown use 40
    mutate(C_input_CP = Aboveground_C_input + Belowground_C_input) %>% #Total C input
    mutate(OrgProd = 0) %>% #0 if no OrgProd is input, OrgProd is the organic product used as fertilizer in BAU
    rename(CROP_CP = CROP, Yield_DM_CP = Yield_DM, Biomass_C_input = Aboveground_C_input, Root_C_input = Belowground_C_input)
  setcolorder(C_inputs_CP, c("PCU", "PROVINCE", "CROP_CP", "Yield_DM_CP","MONTH", "dpm_rpm_ab", "BareToVeg", "SoilDepth", "Biomass_C_input", "Root_C_input", "OrgProd"))

  #if (dir.exists("TS_Carbon_BAU") == FALSE) {
   #dir.create("TS_Carbon_BAU")
  #}
  #write_excel_csv(C_inputs_CP,paste("TS_Carbon_BAU/","TS_BAU_",PCU_code,"_",".csv", sep=""), delim = ";", na = "")

  # Construction of C input File

  RothC <- tibble(
    Num = 1:length(C_inputs$PCU),
    #`PCU` = as.integer(NA),
    `month` = as.character(NA),
    `dpm_rpm_ab` = as.character(NA),
    `BareToVeg` = as.character(NA),
    `SoilDepth` = as.double(NA),
    `Main Crop` = as.character(NA),
    `C_input_ab` = as.double(NA),  #total C input from crop residue left when removed for bioeconomy
    `C_input_be` = as.double(NA),
    #`Type of OrgProd` = as.character(NA),  ##Organic product used as fertilizer in BAU
    #`Carbon input from OrgProd` = as.double(NA),
    `Irrigation` = as.double(NA),
    `Residues` = as.character(NA),
    `Cexport_baseline` = as.double(NA),
    `Cexport_bioeco` = as.double(NA),
    `Type_coproduct` = as.character(NA),
    `Amendment` = as.double(NA), #C input from coproduct
    `Ta` = as.double(NA), #Average monthly temperature
    `Rain` = as.double(NA), #Accumulated monthly precipitation
    `ET_0` = as.double(NA), #Accumulated monthly evapotranspiration
    `BulkDensity` = as.double(NA) #Accumulated monthly evapotranspiration
  )
  RothC <- select(RothC,-"Num")

  # Filling RotC file with C input data---------------------------------------------------------------------

  if (generating_data_for_sensitivity_analysis == "no"){

      RothC_comp1 <- RothC %>%
      #mutate(`PCU` = C_inputs$PCU) %>%
      mutate(`month` = C_inputs$MONTH) %>%
      mutate(`month` = month-1) %>%
      mutate(`dpm_rpm_ab` = C_inputs_CP$dpm_rpm_ab) %>%
      mutate(`BareToVeg` = C_inputs_CP$BareToVeg) %>%
      mutate(`SoilDepth` = C_inputs_CP$SoilDepth ) %>%
      mutate(`Irrigation` = 0 ) %>%
      mutate(`Main Crop` = C_inputs_CP$CROP_CP) %>%
      #left_join(C_inputs_CP,by=c("Month" = "MONTH")) %>%
      #left_join(filter(C_inputs,TYPE=="CP"),by=c("Month" = "MONTH")) %>%
      left_join(export,by=c("Main Crop" = "CROP")) %>%
      mutate(`C_input_be` = C_inputs_CP$Root_C_input,NA) %>%
      mutate(`Residues` = C_inputs$Residues) %>%
      mutate(`R_Res` = C_inputs$R_Res) %>%
      mutate(`Biomass_C_input` = C_inputs_CP$Biomass_C_input) %>%
      mutate(`Facteur_export` = C_inputs$Facteur_export) %>%
      mutate(`RPR` = C_inputs$RPR) %>%
      mutate(`Export_bioeco` = C_inputs$Export_bioeco) %>%
      mutate(`Facteur_export_bioeco` = C_inputs$Facteur_export_bioeco) %>%
      mutate(`Cexport_baseline` = case_when(Residues == "Exported" & !R_Res < 0.1 ~ Biomass_C_input*(1-Facteur_export)/Facteur_export,
                                            R_Res < 0.1 ~ Biomass_C_input*(1/RPR),
                                            !Residues == "Exported" & !R_Res < 0.1  | !R_Res < 0.1 ~ 0)) %>%
      mutate(`Cexport_bioeco` = case_when(Residues == "Restituted" & Export_bioeco == "yes"~ Biomass_C_input*(1-Facteur_export_bioeco)*export_perc/100,
                                          TRUE ~ 0)) %>%
      mutate(`Type_coproduct` = ifelse(Cexport_bioeco > 0,paste(scenario),"")) %>%
      mutate(`Amendment` = ifelse(Cexport_bioeco > 0,0, as.numeric(""))) %>%
      mutate(`Creturn` = ifelse(Cexport_bioeco > 0,Cexport_bioeco*eval(parse(text=paste("conversion_",scenario,sep=""))),
                                as.numeric(Biomass_C_input))) %>%
      mutate(`Amendment` = ifelse(Cexport_bioeco > 0,Creturn, as.numeric("0"))) %>%
      mutate(`C_input_ab` = Biomass_C_input-Cexport_bioeco,NA)%>%
      mutate(`Ta` = C_inputs$Ta) %>%
      mutate(`Rain` = C_inputs$Rain) %>%
      mutate(`ET_0` = C_inputs$ET_0) %>%
      mutate(`BulkDensity` = C_inputs$BulkDensity) %>%
      select(-"Main Crop", -"Residues", -"NA",-"Facteur_export",-"RPR", -"Export_bioeco")
      remove_cols <- c('conversion_Pyrochar','conversion_Pyrochar1','conversion_Gaschar','conversion_Hydrochar','conversion_Digestate','conversionL_Pyrochar','conversionM_Pyrochar','conversionH_Pyrochar','conversionL_Pyrochar1','conversionM_Pyrochar1','conversionH_Pyrochar1','conversionL_Gaschar','conversionM_Gaschar','conversionH_Gaschar','conversionL_Digestate', 'conversionM_Digestate','conversionH_Digestate','conversionL_Hydrochar','conversionM_Hydrochar','conversionH_Hydrochar')
      RothC_comp1 = subset(RothC_comp1, select = !(names(RothC_comp1) %in% remove_cols))


    # Saving files---------------------------------------------------------------------

    if (dir.exists("Bioeconomy_RothC4") == FALSE) {
      dir.create("Bioeconomy_RothC4")

    }
    write_excel_csv(RothC_comp1,paste("Bioeconomy_RothC4/", "TS_", PCU_code,"_",scenario,"_exp",export_perc,".csv", sep=""), delim = ";", na = "")
  }


if (generating_data_for_sensitivity_analysis == "yes"){

  conversion_levels <- c("L","M","H")
  recalcitrance_levels <- c("L","M","H")

  RothC_comp1 <- RothC %>%
    #mutate(`PCU` = C_inputs$PCU) %>%
    mutate(`month` = C_inputs$MONTH) %>%
    mutate(`month` = month-1) %>%
    mutate(`dpm_rpm_ab` = C_inputs_CP$dpm_rpm_ab) %>%
    mutate(`BareToVeg` = C_inputs_CP$BareToVeg) %>%
    mutate(`SoilDepth` = C_inputs_CP$SoilDepth ) %>%
    mutate(`Irrigation` = 0 ) %>%
    mutate(`Main Crop` = C_inputs_CP$CROP_CP) %>%
    #left_join(C_inputs_CP,by=c("ID PCU" = "PCU", "Month" = "MONTH")) %>%
    #left_join(filter(C_inputs,TYPE=="CP"),by=c("PCU" = "PCU", "month" = "MONTH")) %>%
    left_join(export,by=c("Main Crop" = "CROP")) %>%
    mutate(`C_input_be` = C_inputs_CP$Root_C_input,NA) %>%
    mutate(`Residues` = C_inputs$Residues) %>%
    mutate(`R_Res` = C_inputs$R_Res) %>%
    mutate(`Biomass_C_input` = C_inputs_CP$Biomass_C_input) %>%
    mutate(`Facteur_export` = C_inputs$Facteur_export) %>%
    mutate(`RPR` = C_inputs$RPR) %>%
    mutate(`Export_bioeco` = C_inputs$Export_bioeco) %>%
    mutate(`Facteur_export_bioeco` = C_inputs$Facteur_export_bioeco) %>%
    mutate(`Cexport_baseline` = case_when(Residues == "Exported" & !R_Res < 0.1 ~ Biomass_C_input*(1-Facteur_export)/Facteur_export,
                                          R_Res < 0.1 ~ Biomass_C_input*(1/RPR),
                                          !Residues == "Exported" & !R_Res < 0.1  | !R_Res < 0.1 ~ 0)) %>%
    mutate(`Cexport_bioeco` = case_when(Residues == "Restituted" & Export_bioeco == "yes"~ Biomass_C_input*(1-Facteur_export_bioeco)*export_perc/100,
                                        TRUE ~ 0)) %>%
    mutate(`Type_coproduct` = ifelse(Cexport_bioeco > 0,paste(scenario),"")) %>%
    mutate(`Amendment` = ifelse(Cexport_bioeco > 0,0, as.numeric(""))) %>%
    mutate(`Creturn` = ifelse(Cexport_bioeco > 0,Cexport_bioeco*eval(parse(text=paste("conversion_",scenario,sep=""))),
                              as.numeric(Biomass_C_input))) %>%
    mutate(`C_input_ab` = Biomass_C_input-Cexport_bioeco,NA)%>%
    mutate(`Ta` = C_inputs$Ta) %>%
    mutate(`Rain` = C_inputs$Rain) %>%
    mutate(`ET_0` = C_inputs$ET_0) %>%
    mutate(`BulkDensity` = C_inputs$BulkDensity) %>%
    select(-"Main Crop", -"Residues", -"NA",-"Facteur_export",-"RPR", -"Export_bioeco")

  conversion_levels <- c("L","M","H")
  recalcitrance_levels <- c("L","M","H")
  conv_level <- ""
  recalci_level <- ""

  for (conv_level in conversion_levels){
    for(recalci_level in recalcitrance_levels){

      RothC_comp1b <- RothC_comp1 %>%
        mutate(`Amendment` = ifelse(Cexport_bioeco > 0, Cexport_bioeco* eval(parse(text=paste("conversion",conv_level,"_",scenario,sep=""))),
                                    as.numeric(""))) %>%
        mutate(`Type_coproduct` = ifelse(Cexport_bioeco > 0,paste(scenario,recalci_level,sep=""),""))
        remove_cols <- c('conversion_Pyrochar','conversion_Pyrochar1','conversion_Gaschar','conversion_Hydrochar','conversion_Digestate','conversionL_Pyrochar','conversionM_Pyrochar','conversionH_Pyrochar','conversionL_Pyrochar1','conversionM_Pyrochar1','conversionH_Pyrochar1','conversionL_Gaschar','conversionM_Gaschar','conversionH_Gaschar','conversionL_Digestate', 'conversionM_Digestate','conversionH_Digestate','conversionL_Hydrochar','conversionM_Hydrochar','conversionH_Hydrochar')
        RothC_1 = subset(RothC_comp1b, select = !(names(RothC_comp1b) %in% remove_cols))

      # Saving files ---------------------------------------------------------------------

      if (dir.exists("Bioeconomy_RothC4") == FALSE) {
        dir.create("Bioeconomy_RothC4")

      }

      write_excel_csv(RothC_1,paste("Bioeconomy_RothC4/","TS_",PCU_code,"_SA_","conv",conv_level,"_",scenario,recalci_level,"_exp",export_perc,".csv", sep=""), delim = ";", na = "")

      }
    }
  }
}

stopCluster(cl1)
rm(RothC_comp1)
end_time <- Sys.time()
generation_time <- end_time - start_time
print(generation_time)



