# setwd("\\\\ad.uillinois.edu/aces/ACE/personal/nogueir2/iSEE/Bousfield Data Reports")
# setwd("/Users/Eli/Energy")
setwd("/home/CampusEnergy/Energy")

# A control variable
# If set to TRUE, the script will stop if there are missing rooms
stop_for_missing_rooms = F

# set timezone to default
Sys.setenv(TZ="GMT")

Sys.time()
cat("Scripts Started ...\n")

##################################################################
# required packages
pkg <- c("readr", "chron", "data.table", "reshape2", "R.utils", "dplyr", "stringr", "tidyr", "ggplot2")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
pkgCheck <- suppressPackageStartupMessages(lapply(pkg, FUN=function(x) {
    if (!require(x, character.only=T)) {
        install.packages(x, dep=T)
        library(x, character.only=T)
    }
}))

Sys.time()
cat("Packages Loaded ...\n")

##################################################################
Sys.time()
cat("Data Cleaning Started ...\n")

source("./Data.Cleaning/clean.R")

# check existence of final.csv
final_file_path = "./Data.Cleaning/final.csv"
final_file_exist = file.exists(final_file_path)

final = data.frame()
if (!final_file_exist) {
    # process all data
    cat("final.csv does not exist ...\nProcessing all files ...\n")
    room_data_folders = paste("./Data/Room", dir("./Data/Room/"), sep="/")
    room_data_folders = room_data_folders[grep(".csv", room_data_folders, invert=T)]
    weather_data_paths = paste("./Data/Weather", dir("./Data/Weather/"), sep="/")
    final = clean(room_data_folders, weather_data_paths, stop_for_missing_rooms)
} else {
    # process only the newest data
    cat("final.csv does exist ...\nProcessing newest files ...\n")
    room_folder_dates = strptime(substr(dir("./Data/Room/"),1,6), format="%m%d%y")
    room_data_folder = paste("./Data/Room/", dir("./Data/Room/"), sep="")[which.max(as.POSIXct(room_folder_dates))]
    weather_data_dates = strptime(substr(dir("./Data/Weather/"),1,6), format="%m%d%y")
    weather_data_path = paste("./Data/Weather/", dir("./Data/Weather/"), sep="")[which.max(as.POSIXct(weather_data_dates))]
    final = clean(room_data_folder, weather_data_path, stop_for_missing_rooms)
}

##################################################################
if (!final_file_exist) {
    write.csv(final, final_file_path, row.names = F)
} else {
    write.table(final, final_file_path, sep=",", append = T, col.names = F, row.names = F)
}

##################################################################
Sys.time()
cat("Usage Computing Started ...\n")
source("./Data.Processing/prediction.R")
room_randomization = read_csv("./Data.Processing/room_randomization.csv") %>% rename("Room"=room)
hourly_usage = compute_usage(final, room_randomization)

hourly_usage$Week = format(as.Date(hourly_usage$DateTime, format="%Y-%m-%d %H"), format="%Y-%U")
max_week_sat = as.Date(paste(max(hourly_usage$Week), "6", sep="-"), format="%Y-%U-%u")
if (max_week_sat > max(as.Date(format(hourly_usage$DateTime), "%Y-%m-%d %H"))) {
    last_sat_of_whole_week = max_week_sat - 7
    last_whole_week = format(last_sat_of_whole_week, "%Y-%U")
} else {
    last_whole_week = format(max_week_sat, "%Y-%U")
}

usage = hourly_usage %>% filter(Week <= last_whole_week) %>% group_by(Room, Week) %>% 
            select(Room, Week, Usage) %>% summarise_all(funs(mean(., na.rm=TRUE))) %>% mutate(Usage = Usage*168)
usage$Usage[usage$Usage == 0] <- NA
usage = as.data.frame(usage)

##################################################################
Sys.time()
cat("Neighbor Usage Computing Started ...\n")
source("./Data.Processing/neighbor_usage.R")
cluster <- read_csv("./Data.Processing/room_randomization.csv")
key_col <- "TreatmentType"
value_col <- "TF"
gather_cols <- c("cluster_treatment", "individual_treatment", "pure_control")
cluster <- cluster %>% gather_(key_col, value_col, gather_cols) %>% filter(TF == 1) %>% select(-c(room_type, TF))
cluster$TreatmentType[cluster$TreatmentType == "cluster_treatment"] = "Cluster"
cluster$TreatmentType[cluster$TreatmentType == "individual_treatment"] = "Individual"
cluster$TreatmentType[cluster$TreatmentType == "pure_control"] = "Control"
cluster <- cluster[order(cluster$room), ]
cluster <- cluster %>% rename("Room"=room, "RoomCluster"=room_cluster, "ClusterType"=cluster_type, "RoomType"=simple_room_type)

cluster_usage = merge(cluster, usage, by="Room", all.x=T)
cat("Missing Usage in Room(s)", unique(cluster_usage$Room[which(is.na(cluster_usage$Usage))]), "\n")

Weeks = unique(cluster_usage$Week)
neighbor_usage = data.frame()
for (w in Weeks) {
    if (is.na(w))
        next
    single_ind <- cluster_usage %>% filter(Week == w, RoomType == "Single Bedroom") %>% select(Room, TreatmentType, Usage)
    double_ind <- cluster_usage %>% filter(Week == w, RoomType == "Double Bedroom") %>% select(Room, TreatmentType, Usage)
    single_ind_res <- compute_neighbor_usage(single_ind, "Individual") %>% filter(TreatmentType != "Cluster") %>% 
                        select(c(Room, All.Neighbors, Efficient.Neighbors))
    double_ind_res <- compute_neighbor_usage(double_ind, "Individual") %>% filter(TreatmentType != "Cluster") %>% 
                        select(c(Room, All.Neighbors, Efficient.Neighbors))
    non_cluster_res <- merge(cluster_usage %>% filter(Week == w, TreatmentType != "Cluster"), 
                                rbind(single_ind_res, double_ind_res), by="Room", all.x=T)
            
    single_clu <- cluster_usage %>% filter(Week == w, RoomType == "Single Bedroom", !is.na(ClusterType)) %>% 
                    group_by(RoomCluster) %>% summarise(TreatmentType = TreatmentType[1], Usage = sum(Usage)) %>% rename("Room"=RoomCluster)
    double_clu <- cluster_usage %>% filter(Week == w, RoomType == "Double Bedroom", !is.na(ClusterType)) %>% 
                    group_by(RoomCluster) %>% summarise(TreatmentType = TreatmentType[1], Usage = sum(Usage)) %>% rename("Room"=RoomCluster)
    single_clu_res <- compute_neighbor_usage(single_clu, "Cluster") %>% filter(TreatmentType == "Cluster") %>% 
                        select(c(Room, Usage, All.Neighbors, Efficient.Neighbors)) %>% rename("RoomCluster"=Room)
    double_clu_res <- compute_neighbor_usage(double_clu, "Cluster") %>% filter(TreatmentType == "Cluster") %>% 
                        select(c(Room, Usage, All.Neighbors, Efficient.Neighbors)) %>% rename("RoomCluster"=Room)
    cluster_res <- merge(cluster_usage %>% filter(Week == w, TreatmentType == "Cluster") %>% select(-Usage), 
                                rbind(single_clu_res, double_clu_res), by="RoomCluster", all.x=T)
    neighbor_usage <- rbind(neighbor_usage, rbind(non_cluster_res, cluster_res))
}
# sort and move Week to the first column
neighbor_usage <- neighbor_usage[order(neighbor_usage$Week, neighbor_usage$Room), ]
col_idx = grep("Week", names(neighbor_usage))
neighbor_usage <- neighbor_usage[, c(col_idx, (1:ncol(neighbor_usage))[-col_idx])]

# check existence of result.csv
result_file_path = "./Data.Processing/result.csv"
result_file_exist = file.exists(result_file_path)
if (!result_file_exist) {
    cat("result.csv does not exist ...\nCreating result.csv ...\n")
    write.csv(neighbor_usage, result_file_path, row.names = F)
} else {
    cat("result.csv does exist ...\nAppending to result.csv ...\n")
    write.table(neighbor_usage, result_file_path, sep=",", append = T, col.names = F, row.names = F)
}

##################################################################
Sys.time()
cat("Read in result.csv ...\n")
result = read_csv(result_file_path, col_types = cols(Week = col_character()))
result <- result %>% filter(TreatmentType != "Control", !is.na(Usage)) %>% select(c(Room, Week, TreatmentType, Usage, All.Neighbors, Efficient.Neighbors))

current_week_dat <- result %>% group_by(Room) %>% slice(n())
current_week = format(as.Date(paste(current_week_dat$Week[1],"6",sep="-"), "%Y-%U-%u"), "%m%d%y")
current_week_dat <- current_week_dat %>% select(-Week)
compare_dat <- compare_to_neighbor(current_week_dat)

email_list <- read_csv("./Email/email_list.csv") %>% rename("Room"=room, "Email"=email)
email_detail <- left_join(compare_dat, email_list, by="Room")
write_csv(email_detail, "./Email/email_detail.csv")
cat("Writing email_detail.csv ...\n")

cat("Generating Graphs ...\n")
source("./Graphic.Report/generate_graphics.R")

graph_output_folder = paste("./Graphic.Report", current_week, sep="/")
dir.create(graph_output_folder)
generate_graphics(result %>% select(-TreatmentType), graph_output_folder)

##################################################################
cat("Scripts Finished ...\n")

