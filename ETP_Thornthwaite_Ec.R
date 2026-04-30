#-Calcualting ETP using Thornwaite equation for all PCUs in Ecuador
#--Christhel Andrade, 2022

library(tidyverse)
library(readxl)
library(scales)
library(readr)
library(stringr)
library(filesstrings)
library(data.table)

setwd("path/to/your/directory")
data <- read.csv("temp_mean.csv", sep=";")
#d <- read.csv("precipitation_filtered.csv", sep=",")
data1=copy(data)
data1[c('Year', 'Month')] <- str_split_fixed(data1$time, '-', 2)
data2=copy(data1)

# Define the attribute to subset on
attribute1 <- "lon"
attribute2 <- "lat"
#attribute3 <- "Year"

# Define the value to subset on. -- Shown example for a location
value1 <- "-82.5848"
value2 <- "-5.704018"
lat <- as.numeric(value2)
#value3 <- "2020"

# Subset the data frame based on the attribute and value
data2=copy(data1)
subset_data1 <- data2[data2[, attribute1] == value1, ]
subset_data2 <- subset_data1[subset_data1[, attribute2] == value2, ]

PET <- thornthwaite(subset_data2$tmean, lat=lat)

PET_df <- as.data.frame(PET)
PET_df$lon <- as.numeric(value1)
PET_df$lat <- as.numeric(value2)
PET_df$time <- time(PET)

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
PET_df$months <- as.factor(rep(months, times = 51)) # repeat months 8 times
PET_df$Month <- ifelse(PET_df$month == "Jan", "01",
                 ifelse(PET_df$month == "Feb", "02",
                 ifelse(PET_df$month == "Mar", "03",
                 ifelse(PET_df$month == "Apr", "04",
                 ifelse(PET_df$month == "May", "05",
                 ifelse(PET_df$month == "Jun", "06",
                 ifelse(PET_df$month == "Jul", "07",
                 ifelse(PET_df$month == "Aug", "08",
                 ifelse(PET_df$month == "Sep", "09",
                 ifelse(PET_df$month == "Oct", "10",
                 ifelse(PET_df$month == "Nov", "11",
                 ifelse(PET_df$month == "Dec", "12",NA))))))))))))
years <- c(2020:2070)
PET_df$year_num <- as.numeric(as.character(gl(51, 12, 612)))  # number the months
PET_df$Year <- ifelse(PET_df$year_num == 1, 2020,
               ifelse(PET_df$year_num == 2, 2021,
               ifelse(PET_df$year_num == 3, 2022,
               ifelse(PET_df$year_num == 4, 2023,
               ifelse(PET_df$year_num == 5, 2024,
               ifelse(PET_df$year_num == 6, 2025,
               ifelse(PET_df$year_num == 7, 2026,
               ifelse(PET_df$year_num == 8, 2027,
               ifelse(PET_df$year_num == 9, 2028,
               ifelse(PET_df$year_num == 10, 2029,
               ifelse(PET_df$year_num == 11, 2030,
               ifelse(PET_df$year_num == 12, 2031,
               ifelse(PET_df$year_num == 13, 2032,
               ifelse(PET_df$year_num == 14, 2033,
               ifelse(PET_df$year_num == 15, 2034,
               ifelse(PET_df$year_num == 16, 2035,
               ifelse(PET_df$year_num == 17, 2036,
               ifelse(PET_df$year_num == 18, 2037,
               ifelse(PET_df$year_num == 19, 2038,
               ifelse(PET_df$year_num == 20, 2039,
               ifelse(PET_df$year_num == 21, 2040,
               ifelse(PET_df$year_num == 22, 2041,
               ifelse(PET_df$year_num == 23, 2042,
               ifelse(PET_df$year_num == 24, 2043,
               ifelse(PET_df$year_num == 25, 2044,
               ifelse(PET_df$year_num == 26, 2045,
               ifelse(PET_df$year_num == 27, 2046,
               ifelse(PET_df$year_num == 28, 2047,
               ifelse(PET_df$year_num == 29, 2048,
               ifelse(PET_df$year_num == 30, 2049,
               ifelse(PET_df$year_num == 31, 2050,
               ifelse(PET_df$year_num == 32, 2051,
               ifelse(PET_df$year_num == 33, 2052,
               ifelse(PET_df$year_num == 34, 2053,
               ifelse(PET_df$year_num == 35, 2054,
               ifelse(PET_df$year_num == 36, 2055,
               ifelse(PET_df$year_num == 37, 2056,
               ifelse(PET_df$year_num == 38, 2057,
               ifelse(PET_df$year_num == 39, 2058,
               ifelse(PET_df$year_num == 40, 2059,
               ifelse(PET_df$year_num == 41, 2060,
               ifelse(PET_df$year_num == 42, 2061,
               ifelse(PET_df$year_num == 43, 2062,
               ifelse(PET_df$year_num == 44, 2063,
               ifelse(PET_df$year_num == 45, 2064,
               ifelse(PET_df$year_num == 46, 2065,
               ifelse(PET_df$year_num == 47, 2066,
               ifelse(PET_df$year_num == 48, 2067,
               ifelse(PET_df$year_num == 49, 2068,
               ifelse(PET_df$year_num == 50, 2069,NA))))))))))))))))))))))))))))))))))))))))))))))))))
PET_df$Year <- ifelse(PET_df$year_num == 51, 2070,PET_df$Year)

PET_df$time=NULL
PET_df$months=NULL
PET_df$year_num=NULL
PET_df$time <- paste(PET_df$Year,"–",PET_df$Month)
setcolorder(PET_df, c("time", "Year", "Month", "lon", "lat", "PET_tho"))

write_excel_csv(PET_df,paste("PET","_long_",value1,"_lat_",value2,".csv", sep=""), delim = ";", na = "")

####----JOIN SUBSETS: Paste all subsets as columns for use as GIS---
#set the library---- here will be the input and output data, move all temp files there

setwd("path/to/your/directory")

if (dir.exists("PET_files") == FALSE) {
  dir.create("PET_files")
}

list_files1 <- list.files(pattern = paste("PET_long_",sep=""), all.files = FALSE, full.names = FALSE)
move_files(list_files1,paste("PET_files",sep=""), overwrite = TRUE)
setwd("path/to/your/directory/PET_files")

# Set the folder path
folder_path <- "path/to/your/directory/PET_files"
# Get a list of file names in the folder
file_names1 <- list.files(path = folder_path, pattern = "*.csv")

# Initialize an empty data frame to store the combined data
df_combined <- data.frame()

# Loop through the list of filenames
for (i in 1:length(file_names1)) {

  # Read the current file into a data frame
  df_current <- read.csv(file_names1[i],sep=";", dec=".")

  # Bind the current file to the combined data frame
  df_combined <- rbind(df_combined, df_current)
}

df_combined$ï..time <- sub(' â€“ ','-',df_combined$ï..time)
setnames(df_combined, c("ï..time"), c("time"))

if (dir.exists("Joined_PET") == FALSE) {
  dir.create("Joined_PET")
}
# Write the combined data to a new CSV file
write_delim(df_combined,paste("Joined_PET/", paste("PET_all.csv",sep="")), delim = ";", na = "")


