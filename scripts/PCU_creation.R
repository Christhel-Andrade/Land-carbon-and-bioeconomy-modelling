#--Create simulation units using PCU data attribute extracted from shapefiles and yield information
#--2021, Christhel Andrade, Updated:2022

#libraries
library(tidyverse)
library(readxl)
library(readr)
library(data.table)
library(dplyr)

#-----------------------------------------------------------------------------------------------------#
###Monthly Yield assignation to PCU - This file will be used to create the TS files in another script##
Yield=fread(".path/yield_file.csv")
Code=fread(".path/PCU_code.csv", select=c('FID', 'PROVINCE','CANTON','CROP', 'Area_ha', 'IRRIGATION', 'CLAY', 'BD')) #extracted from shqpefile
Code$PROVINCE <- as.character(Code$PROVINCE)
Yield$PROVINCE <- as.character(Yield$PROVINCE)

setkey(Code, "PROVINCE")
setkey(Yield, "PROVINCE")

codeyield=Yield[Code, on=.(PROVINCE, CROP),allow.cartesian=TRUE]

codeyield[,c("YIELD"):=NULL]
setnames(codeyield, c("YIELD_MONTH", "FID"), c("YIELD", "PCU"))
setcolorder(codeyield, c("PCU", "PROVINCE", "CANTON", "MONTH", "CROP", "TYPE", "YIELD", "BareToVeg"))
fwrite(codeyield, file="PCU_crop.csv", sep=";", row.names=FALSE)

biomass=codeyield[MONTH == 1]
biomass <- biomass  %>%
  mutate(Biomass = YIELD * Area_ha) # dry yield of product t/ha

fwrite(biomass, file="Biomass.csv", sep=";", row.names=FALSE)

#-----------------------------------------------------------------------------------------------------#
###Initial data file creation. It includes DPM, RPM, BIO, HUM, IOM, SOC_time_0
Init=fread(".path/PCU_code_correct.csv", select=c('FID', 'SOC_2020'))
setwd("path/to/your/directory") # setting working directory 

Init_PCU <- Init %>%
  mutate(DPM = 0) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(RPM = 0) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(BIO = 0) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(HUM = 0) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(IOM = 0)   #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
setnames(Init_PCU, c("FID", "SOC_2020"), c("PCU", "SOC_time_0"))
setcolorder(Init_PCU, c("PCU", "DPM", "RPM", "BIO", "HUM", "IOM", "SOC_time_0"))
fwrite(Init_PCU, file="Init_PCU.csv", sep=";", row.names=FALSE)

if (dir.exists("Init_Zero_Size") == FALSE) {
  dir.create("Init_Zero_Size")
}

list_files_Init <- list.files(pattern = paste("Init_data",sep=""), all.files = FALSE, full.names = FALSE)
library(filesstrings)
move_files(list_files_Init,paste("Init_Zero_Size",sep=""), overwrite = TRUE)

###previous--not used anymore------27/02/2023 Needed to be used again after first running failed results
Init=fread(".path/PCU_code_correct.csv")

#Init[,4:8] <- lapply(Init[,4:8], as.numeric)
Init_PCU <- Init %>%
  mutate(DPM_f = DPM_B/SOC_BAU_2040) %>%  #Fraction of DPM in SOC pools
  mutate(RPM_f = RPM_B/SOC_BAU_2040) %>%  #Fraction of RPM in SOC pools
  mutate(BIO_f = BIO_B/SOC_BAU_2040) %>%  #Fraction of BIO in SOC pools
  mutate(HUM_f = HUM_B/SOC_BAU_2040) %>%  #Fraction of HUM in SOC pools
  mutate(IOM_f = IOM_B/SOC_BAU_2040) %>%  #Fraction of IOM in SOC pools
  mutate(DPM = SOC_t0*DPM_f) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(RPM = SOC_t0*RPM_f) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(BIO = SOC_t0*BIO_f) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(HUM = SOC_t0*HUM_f) %>%  #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-
  mutate(IOM = SOC_t0*IOM_f)   #Amount of DPM SOC in 2020 based on fractions seen in 2040 modeling from Ecuadorian Agriculture Ministry - to keep the same rationale-

Init_PCU[,c("PROVINCE","CANTON","CLAY","CROP","IRRIGATION","BD","tap","Area_ha","SOC_2020","SOC_BAU_2040", "DPM_B", "RPM_B", "BIO_B", "HUM_B", "IOM_B", "DPM_f", "RPM_f", "BIO_f", "HUM_f", "IOM_f"):=NULL] 

setnames(Init_PCU, c("FID", "SOC_t0"), c("PCU", "SOC_time_0"))
setcolorder(Init_PCU, c("PCU", "DPM", "RPM", "BIO", "HUM", "IOM", "SOC_time_0"))
Init_PCU_no <- Init_PCU[Init_PCU$SOC_time_0 ==0,]
Init_PCU_select <- Init_PCU[Init_PCU$SOC_time_0 !=0,]
fwrite(Init_PCU_select, file="Init_PCU_sel.csv", sep=";", row.names=FALSE)

dir_path <- "path/to/your/directory"

list_files <- Init_PCU_no$PCU
PCU <-""

if (dir.exists("Crops removed") == FALSE) {
  dir.create("Crops removed")
}

  for (PCU in list_files){
    
    list_files_remove <- list.files(pattern = paste("TS_data_subset_",PCU,sep=""), all.files = FALSE, full.names = FALSE)
    move_files(list_files_remove,paste("Crops removed",sep=""), overwrite = TRUE)
  }
  
list_files_remove <- list.files(pattern = paste("TS_data_subset_",sep=""), all.files = FALSE, full.names = FALSE)
move_files(list_files_SA,paste("Sensitivity_analysis_inputs",sep=""), overwrite = TRUE)
setwd("path/to/your/directory/Sensitivity_analysis_inputs")

all_files <- list.files(dir_path)
list_files <- Init_PCU_no$PCU
list_files1 <- lapply(list_files, function(x) paste("TS_data_subset", x, sep = "_"))
pattern <- paste0("(", paste(list_files1, collapse = "|"), ")")

matching_files <- all_files[grep(list_files1, all_files)]

matching_files <- lapply(list_files1, function(x) {
  grep(x, all_files, value = TRUE)
})

pattern_escaped <- regex(pattern, ignore_case = TRUE)

matching_files <- all_files[grep(pattern, all_files)]
matching_files <- grep(paste(list_files, collapse = "|"), all_files, value = TRUE)

if (dir.exists("Crops removed") == FALSE) {
  dir.create("Crops removed")
}

list_files_TS <- list.files(pattern = paste("Init_data",sep=""), all.files = FALSE, full.names = FALSE)
library(filesstrings)
move_files(list_files_Init,paste("Init_Zero_Size",sep=""), overwrite = TRUE)

#-----------------------------------------------------------------------------------------------------#
###Recycle all the TS data by 100 years (1200) months### Maybe do this before subsetting?
##25519 PCU * 12 months * 100 years = 30622800 rows expected

year2020=codeyield
year2021=year2020
year2021$MONTH=as.numeric(year2020$MONTH)+12 # 13-24
year2022=year2021
year2022$MONTH=as.numeric(year2021$MONTH)+12 # 25-36
year2023=year2022
year2023$MONTH=as.numeric(year2022$MONTH)+12 # 37-48
year2024=year2023
year2024$MONTH=as.numeric(year2023$MONTH)+12 # 49-60
year2025=year2024
year2025$MONTH=as.numeric(year2024$MONTH)+12 # 61-72
year2026=year2025
year2026$MONTH=as.numeric(year2025$MONTH)+12 # 73-84
year2027=year2026
year2027$MONTH=as.numeric(year2026$MONTH)+12 # 85-96
year2028=year2027
year2028$MONTH=as.numeric(year2027$MONTH)+12 # 97-108
year2029=year2028
year2029$MONTH=as.numeric(year2028$MONTH)+12 # 109-120
year2030=year2029
year2030$MONTH=as.numeric(year2029$MONTH)+12 # 121-132
year2031=year2030
year2031$MONTH=as.numeric(year2030$MONTH)+12 # 133-144
year2032=year2031
year2032$MONTH=as.numeric(year2031$MONTH)+12 # 145-156
year2033=year2032
year2033$MONTH=as.numeric(year2032$MONTH)+12 # 157-168
year2034=year2033
year2034$MONTH=as.numeric(year2033$MONTH)+12 # 169-180
year2035=year2034
year2035$MONTH=as.numeric(year2034$MONTH)+12 # 181-192
year2036=year2035
year2036$MONTH=as.numeric(year2035$MONTH)+12 # 193-104
year2037=year2036
year2037$MONTH=as.numeric(year2036$MONTH)+12 # 105-116
year2038=year2037
year2038$MONTH=as.numeric(year2037$MONTH)+12 # 25-36
year2039=year2038
year2039$MONTH=as.numeric(year2038$MONTH)+12 # 25-36
year2040=year2039
year2040$MONTH=as.numeric(year2039$MONTH)+12 # 25-36
year2041=year2040
year2041$MONTH=as.numeric(year2040$MONTH)+12 # 25-36
year2042=year2041
year2042$MONTH=as.numeric(year2041$MONTH)+12 # 25-36
year2043=year2042
year2043$MONTH=as.numeric(year2042$MONTH)+12 # 25-36
year2044=year2043
year2044$MONTH=as.numeric(year2043$MONTH)+12 # 25-36
year2045=year2044
year2045$MONTH=as.numeric(year2044$MONTH)+12 # 25-36
year2046=year2045
year2046$MONTH=as.numeric(year2045$MONTH)+12 # 13-24
year2047=year2046
year2047$MONTH=as.numeric(year2046$MONTH)+12 # 25-36
year2048=year2047
year2048$MONTH=as.numeric(year2047$MONTH)+12 # 37-48
year2049=year2048
year2049$MONTH=as.numeric(year2048$MONTH)+12 # 49-60
year2050=year2049
year2050$MONTH=as.numeric(year2049$MONTH)+12 # 61-72
year2051=year2050
year2051$MONTH=as.numeric(year2050$MONTH)+12 # 73-84
year2052=year2051
year2052$MONTH=as.numeric(year2051$MONTH)+12 # 85-96
year2053=year2052
year2053$MONTH=as.numeric(year2052$MONTH)+12 # 97-108
year2054=year2053
year2054$MONTH=as.numeric(year2053$MONTH)+12 # 109-120
year2055=year2054
year2055$MONTH=as.numeric(year2054$MONTH)+12 # 25-36
year2056=year2055
year2056$MONTH=as.numeric(year2055$MONTH)+12 # 25-36
year2057=year2056
year2057$MONTH=as.numeric(year2056$MONTH)+12 # 25-36
year2058=year2057
year2058$MONTH=as.numeric(year2057$MONTH)+12 # 25-36
year2059=year2058
year2059$MONTH=as.numeric(year2058$MONTH)+12 # 25-36
year2060=year2059
year2060$MONTH=as.numeric(year2059$MONTH)+12 # 25-36
year2061=year2060
year2061$MONTH=as.numeric(year2060$MONTH)+12 # 25-36
year2062=year2061
year2062$MONTH=as.numeric(year2061$MONTH)+12 # 25-36
year2063=year2062
year2063$MONTH=as.numeric(year2062$MONTH)+12 # 25-36
year2064=year2063
year2064$MONTH=as.numeric(year2063$MONTH)+12 # 25-36
year2065=year2064
year2065$MONTH=as.numeric(year2064$MONTH)+12 # 25-36
year2066=year2065
year2066$MONTH=as.numeric(year2065$MONTH)+12 # 25-36
year2067=year2066
year2067$MONTH=as.numeric(year2066$MONTH)+12 # 25-36
year2068=year2067
year2068$MONTH=as.numeric(year2067$MONTH)+12 # 25-36
year2069=year2068
year2069$MONTH=as.numeric(year2068$MONTH)+12 # 25-36
year2070=year2069
year2070$MONTH=as.numeric(year2069$MONTH)+12 # 25-36
year2071=year2070
year2071$MONTH=as.numeric(year2070$MONTH)+12 # 85-96
year2072=year2071
year2072$MONTH=as.numeric(year2071$MONTH)+12 # 97-108
year2073=year2072
year2073$MONTH=as.numeric(year2072$MONTH)+12 # 109-120
year2074=year2073
year2074$MONTH=as.numeric(year2073$MONTH)+12 # 25-36
year2075=year2074
year2075$MONTH=as.numeric(year2074$MONTH)+12 # 25-36
year2076=year2075
year2076$MONTH=as.numeric(year2075$MONTH)+12 # 25-36
year2077=year2076
year2077$MONTH=as.numeric(year2076$MONTH)+12 # 25-36
year2078=year2077
year2078$MONTH=as.numeric(year2077$MONTH)+12 # 25-36
year2079=year2078
year2079$MONTH=as.numeric(year2078$MONTH)+12 # 25-36
year2080=year2079
year2080$MONTH=as.numeric(year2079$MONTH)+12 # 25-36
year2081=year2080
year2081$MONTH=as.numeric(year2080$MONTH)+12 # 25-36
year2082=year2081
year2082$MONTH=as.numeric(year2081$MONTH)+12 # 25-36
year2083=year2082
year2083$MONTH=as.numeric(year2082$MONTH)+12 # 25-36
year2084=year2083
year2084$MONTH=as.numeric(year2083$MONTH)+12 # 25-36
year2085=year2084
year2085$MONTH=as.numeric(year2084$MONTH)+12 # 25-36
year2086=year2085
year2086$MONTH=as.numeric(year2085$MONTH)+12 # 25-36
year2087=year2086
year2087$MONTH=as.numeric(year2086$MONTH)+12 # 25-36
year2088=year2087
year2088$MONTH=as.numeric(year2087$MONTH)+12 # 25-36
year2089=year2088
year2089$MONTH=as.numeric(year2088$MONTH)+12 # 25-36
year2090=year2089
year2090$MONTH=as.numeric(year2089$MONTH)+12 # 25-36
year2091=year2090
year2091$MONTH=as.numeric(year2090$MONTH)+12 # 25-36
year2092=year2091
year2092$MONTH=as.numeric(year2091$MONTH)+12 # 25-36
year2093=year2092
year2093$MONTH=as.numeric(year2092$MONTH)+12 # 25-36
year2094=year2093
year2094$MONTH=as.numeric(year2093$MONTH)+12 # 25-36
year2095=year2094
year2095$MONTH=as.numeric(year2094$MONTH)+12 # 25-36
year2096=year2095
year2096$MONTH=as.numeric(year2095$MONTH)+12 # 25-36
year2097=year2096
year2097$MONTH=as.numeric(year2096$MONTH)+12 # 25-36
year2098=year2097
year2098$MONTH=as.numeric(year2097$MONTH)+12 # 25-36
year2099=year2098
year2099$MONTH=as.numeric(year2098$MONTH)+12 # 25-36
year2100=year2099
year2100$MONTH=as.numeric(year2099$MONTH)+12 # 25-36
year2101=year2100
year2101$MONTH=as.numeric(year2100$MONTH)+12 # 25-36
year2102=year2101
year2102$MONTH=as.numeric(year2101$MONTH)+12 # 25-36
year2103=year2102
year2103$MONTH=as.numeric(year2102$MONTH)+12 # 25-36
year2104=year2103
year2104$MONTH=as.numeric(year2103$MONTH)+12 # 25-36
year2105=year2104
year2105$MONTH=as.numeric(year2104$MONTH)+12 # 25-36
year2106=year2105
year2106$MONTH=as.numeric(year2105$MONTH)+12 # 25-36
year2107=year2106
year2107$MONTH=as.numeric(year2106$MONTH)+12 # 25-36
year2108=year2107
year2108$MONTH=as.numeric(year2107$MONTH)+12 # 25-36
year2109=year2108
year2109$MONTH=as.numeric(year2108$MONTH)+12 # 25-36
year2110=year2109
year2110$MONTH=as.numeric(year2109$MONTH)+12 # 25-36
year2111=year2110
year2111$MONTH=as.numeric(year2110$MONTH)+12 # 25-36
year2112=year2111
year2112$MONTH=as.numeric(year2111$MONTH)+12 # 25-36
year2113=year2112
year2113$MONTH=as.numeric(year2112$MONTH)+12 # 25-36
year2114=year2113
year2114$MONTH=as.numeric(year2113$MONTH)+12 # 25-36
year2115=year2114
year2115$MONTH=as.numeric(year2114$MONTH)+12 # 25-36
year2116=year2115
year2116$MONTH=as.numeric(year2115$MONTH)+12 # 25-36
year2117=year2116
year2117$MONTH=as.numeric(year2116$MONTH)+12 # 25-36
year2118=year2117
year2118$MONTH=as.numeric(year2117$MONTH)+12 # 25-36
year2119=year2118
year2119$MONTH=as.numeric(year2118$MONTH)+12 # 25-36
year2120=year2119
year2120$MONTH=as.numeric(year2119$MONTH)+12 # 25-36

for (year in c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033, 2034, 2035, 2036, 2037, 2038, 2039, 2040, 2041, 2042, 2043, 2044, 2045, 2046, 2047, 2048, 2049, 2050, 2051, 2052, 2053, 2054, 2055, 2056, 2057, 2058, 2059, 2060, 2061, 2062, 2063, 2064, 2065, 2066, 2067, 2068, 2069, 2070, 2071, 2072, 2073, 2074, 2075, 2076, 2077, 2078, 2079, 2080, 2081, 2082, 2083, 2084, 2085, 2086, 2087, 2088, 2089, 2090, 2091, 2092, 2093, 2094, 2095, 2096, 2097, 2098, 2099, 2100, 2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115, 2116, 2117, 2118, 2119, 2120)) {
  write.table(get(paste0("year", year)) , file=paste0("TS_", year, ".csv"), sep=";", row.names=FALSE)
}

file_name1 = "TS"
datalist1 = list()

for (i in c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033, 2034, 2035, 2036, 2037, 2038, 2039, 2040, 2041, 2042, 2043, 2044, 2045, 2046, 2047, 2048, 2049, 2050, 2051, 2052, 2053, 2054, 2055, 2056, 2057, 2058, 2059, 2060, 2061, 2062, 2063, 2064, 2065, 2066, 2067, 2068, 2069, 2070, 2071, 2072, 2073, 2074, 2075, 2076, 2077, 2078, 2079, 2080, 2081, 2082, 2083, 2084, 2085, 2086, 2087, 2088, 2089, 2090, 2091, 2092, 2093, 2094, 2095, 2096, 2097, 2098, 2099, 2100, 2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115, 2116, 2117, 2118, 2119, 2120)) {
  data1 <- read.csv2(paste(file_name1,"_",i,".csv",sep=""),header=T,na.strings=c("","NA"))
  datalist1[[i]] <- data1
}

all_data1 = do.call(rbind, datalist1)

write_excel_csv(all_data1,paste(file_name1,".csv", sep=""), delim = ";", na = "")
 
ts=fread("path/TS.csv")

ts_50=copy(ts)
ts_50 <- ts_50[!(ts_50$MONTH >= 613 & ts_50$MONTH <= 1212), ]
write_excel_csv(ts_50,paste("TS_50.csv", sep=""), delim = ";", na = "")

###---Join climate Data----###
PET=fread("path/PET_month_full.csv", sep=";")
Prec=fread("path/Prec_month_full.csv", sep=";")
Temp=fread("path/Temp_month_full.csv", sep=";")
setnames(PET, c("Month"), c("MONTH"))
setnames(Prec, c("Month"), c("MONTH"))
setnames(Temp, c("Month"), c("MONTH"))

setkey(ts_50, "PCU")
setkey(Temp, "PCU")
setkey(Prec, "PCU")
setkey(PET, "PCU")

data_merge1=ts_50[Temp, on=.(PCU, MONTH),allow.cartesian=TRUE]
data_merge2=data_merge1[Prec, on=.(PCU, MONTH),allow.cartesian=TRUE]
data_merge3=data_merge2[PET, on=.(PCU, MONTH),allow.cartesian=TRUE]
setnames(data_merge3, old = c('BD','tm','pr','PET','IRRIGATION'), 
         new = c('BulkDensity','Ta','Rain','ET_0','Irrigation'))
setcolorder(data_merge3, c("PCU", "PROVINCE", "CANTON", "MONTH", "CROP", "TYPE", "YIELD", "BareToVeg", "Area_ha", "Irrigation", "Ta", "Rain", "ET_0", "BulkDensity"))
data_merge3$CLAY=NULL
fwrite(data_merge3, file="TS_clim.csv", sep=";", row.names=FALSE)

#-----------------------------------------------------------------------------------------------------#
#codeyield[,c("YIELD", "niv3", "rie", "sce", "usce", "SHAPE_Leng", "Area_km2", "Area_ha", "Shape_Area"):=NULL]
codeyield=Code[Yield, on=.(PROVINCE, CROP),allow.cartesian=TRUE]
codeyield <- merge(Yield, Code, by=C("PROVINCE", "CROP"), all = TRUE)
merge(x, y, by=c("k1","k2")) 
setnames(Code, "FID", "PROVINCE")

#-----------------------------------------------------------------------------------------------------#
###Constant data file creation. It includes Clay, f_labile, f_recalcitrant, K_labile, K_recalcitrant, PrimingEffect
setwd("path/to/your/directory")

PCU=fread("PCU_code_correct.csv", sep=";", dec=".",header=TRUE, na = "", select=c('FID', 'CLAY', 'CROP'))
setnames(PCU, c("FID"), c("PCU"))
Rec <- read_excel(path = paste0("Data",".xlsx"), sheet = "Const_Data",col_names = TRUE, na = "") #### see if to keep or remove
Rec <- as.data.table(Rec)

setkey(PCU, "PCU")
setkey(Rec, "CROP")

Constant <- PCU  %>%
  left_join(Rec, by = c("CROP")) # joining RPR values
Constant$CROP=NULL
setnames(Constant, c("CLAY"), c("ClayPercent"))
fwrite(Constant, file="Constant.csv", sep=";", row.names=FALSE)
  
##Then Run Script Subset_Const_data
