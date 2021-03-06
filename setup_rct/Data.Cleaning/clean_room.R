clean_room <- function(room_data_folder_path) {
	file_names = dir(room_data_folder_path)
	room_output = data.frame()
	
	for(i in seq(file_names)) {
		cat("Processing", file_names[i],  "(", i, "/", length(file_names), ") ...\n")
        
        # import data 
        file_path = paste(room_data_folder_path, file_names[i], sep="/") 
        headers = read.csv(file_path, header=F, nrows=1, as.is=T) 
        headers[1] = "Date" 
        headers[2] = "Time" 
        file = read_csv(file_path, skip=3, col_names=F, col_types=cols(.default="c")) 
        # file = as.data.frame(file) 
        colnames(file) = gsub(" ", ".", headers) 

        if (length(grep("CTL.STPT", colnames(file))) == 0) {
            cat(" - No CTL.STPT in file\n")
            remove(file)
            next
        }
        # keep only setpoint variables as well as DateTime
        keep = grep("Date|Time|CTL.STPT", colnames(file))
        file = file[ , keep]
            
        # check and apply the right date format 
        file$DateTime = paste(file$Date, file$Time)
        file$DateTime = strptime(file$DateTime, format="%m/%d/%Y %I:%M:%S %p")

        drops = c("Date", "Time")
        file = file[ , !(names(file) %in% drops)]
        
        # clean numerically
        regexp = "[[:digit:]]+(.[[:digit:]]+)?"
        col_ind = grep("CTL.STPT", names(file))
        for (each_col in col_ind) {
            extract_num = str_extract(as.character(unlist(file[, each_col])), regexp)
            file[, each_col] = as.numeric(extract_num)
        }
            
        # Round DateTime to closest quarter and aggregate the data.frame quarterly
        file$RoundDateTime = round.POSIXt(file$DateTime, units="mins")
        file$date = as.Date(file$RoundDateTime)
        file$hour_of_day = hour(file$RoundDateTime)
        file$quarter_of_hour = minute(file$RoundDateTime) %/% 15 + 1
            
        file = file %>%
            select(-c(DateTime, RoundDateTime)) %>%
            group_by(date, hour_of_day, quarter_of_hour) %>%
            summarise_all(funs(mean(., na.rm=TRUE))
            )
        file = as.data.frame(file)
            
        # regain RoundDateTime based on date, hour_of_day and quarter_of_hour
        file$RoundDateTime = as.POSIXlt(ISOdatetime(year=substr(file$date,1,4),
                                                    month=substr(file$date,6,7),
                                                    day=substr(file$date,9,10),
                                                    hour=file$hour_of_day,
                                                    min=(file$quarter_of_hour - 1)*15,
                                                    sec=0))
        drops = c("date", "hour_of_day", "quarter_of_hour")
        file = file[ , !(names(file) %in% drops)]
        file$RoundDateTime = as.character(file$RoundDateTime)
            
        # sort and move RoundDateTime to the first column
        file = file[order(file$RoundDateTime),]
        col_idx = grep("RoundDateTime", names(file))
        file = file[, c(col_idx, (1:ncol(file))[-col_idx])]
        
        # create a column for each variable
        # stack observations for all rooms
        melt.ctl.stpt = reshape2::melt(file, id="RoundDateTime", measure=grep("CTL.STPT", colnames(file)))
        colnames(melt.ctl.stpt) = c("RoundDateTime", "Room", "CTL.STPT")
        melt.ctl.stpt$Room = gsub("B1252.FCU|:CTL.STPT", "", melt.ctl.stpt$Room)

        # Append all the data obtained from different files
        room_output = rbind(room_output, melt.ctl.stpt)
        remove(file, melt.ctl.stpt)
    }
    room_output = room_output[order(room_output$RoundDateTime, room_output$Room),]
    dup_ind = which(duplicated(room_output[ , c("Room", "RoundDateTime")]))
    if (length(dup_ind) != 0)
        room_output = room_output[-dup_ind, ]

    # first_day = as.Date(room_output$RoundDateTime[1], "%Y-%m-%d")
    # last_day = first_day + 6
    # pending = subset(room_output, RoundDateTime > last_day)
    # room_output = subset(room_output, RoundDateTime <= last_day)

    # write.csv(na.omit(pending), "./Data/Room/pending_room.csv", row.names = F)

    

    return(room_output)
}
