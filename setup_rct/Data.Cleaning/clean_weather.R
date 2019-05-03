clean_weather <- function(weather_data_path) {
	file = read_csv(weather_data_path, col_types = cols(.default = "c"))
        
    ### Keeping only hourly HOURLYDRYBULBTEMPF
    # file = file[which(file$REPORTTPYE=="FM-15"),]
    file = file[, grep("DATE|HOURLYDRYBULBTEMPF", colnames(file))]

    regexp = "-?[[:digit:]]+(.[[:digit:]]+)?" # integer or float
    extract_num = str_extract(as.character(unlist(file$HOURLYDRYBULBTEMPF)), regexp)
	file$HOURLYDRYBULBTEMPF = as.numeric(extract_num)
    
    file$DATE = strptime(file$DATE, format = "%Y-%m-%d %H:%M")
    # file$DATE = strptime(file$DATE, format = "%m/%d/%Y %H:%M")
    colnames(file)[colnames(file) == "DATE"] = "RoundDateTime"

    # round to hours (17:53 -> 18:00)
    round_df = file
    round_df$RoundDateTime = round.POSIXt(round_df$RoundDateTime, units = "hours")
    round_df$date = as.Date(round_df$RoundDateTime)
    round_df$hour = hour(round_df$RoundDateTime)

    # cutting hours (17:53 -> 17:00)
    cut_df = file
    cut_df$date = as.Date(cut_df$RoundDateTime)
    cut_df$hour = hour(cut_df$RoundDateTime)

    file <- rbind(round_df, cut_df) 

    ## Clean duplicates in DateTime
    file = file %>%
        select(-RoundDateTime) %>% 
        group_by(date, hour) %>% 
        summarise_all(funs(mean(., na.rm = TRUE))
        )
        
    file = as.data.frame(file)
    file$RoundDateTime = as.POSIXlt(ISOdatetime(year = substr(file$date,1,4),
                                                month = substr(file$date,6,7),
                                                day = substr(file$date,9,10),
                                                hour = file$hour,
                                                min = 0,
                                                sec = 0))
    
    file = file[order(file$RoundDateTime),]
    drops = c("date", "hour")
    file = file[ , !(names(file) %in% drops)]
    
    # Move RoundDateTime to the first place
    col_idx = grep("RoundDateTime", names(file))
    file = file[, c(col_idx, (1:ncol(file))[-col_idx])]
    
    # repeat rows to match 15-min interval
    file.expanded = as.data.frame(lapply(file, function(x) rep(x,each=4))) 
    ind_00 = which(!duplicated(file.expanded$RoundDateTime))
    ind_15 = ind_00 + 1
    ind_30 = ind_15 + 1
    ind_45 = ind_30 + 1
    
    file.expanded$RoundDateTime[ind_15] = file.expanded$RoundDateTime[ind_15] + 15 * 60
    file.expanded$RoundDateTime[ind_30] = file.expanded$RoundDateTime[ind_30] + 30 * 60
    file.expanded$RoundDateTime[ind_45] = file.expanded$RoundDateTime[ind_45] + 45 * 60

    file.expanded$RoundDateTime = as.character(file.expanded$RoundDateTime)
    
    return(file.expanded)
}

