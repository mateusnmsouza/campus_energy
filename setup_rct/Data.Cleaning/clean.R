source("./Data.Cleaning/clean_room.R")
source("./Data.Cleaning/clean_weather.R")

clean <- function(room, weather, stop_for_missing_rooms) {

    ## Room
    room_output = data.frame()

    for (r in room) {
        each_output = clean_room(r)
        room_output = rbind(room_output, each_output)
    }
    date_range = range(as.Date(room_output$RoundDateTime))
    sat_date = seq(date_range[1], date_range[2], by="day")[weekdays(seq(date_range[1], date_range[2], by="day"))=='Saturday']
    # sat_date = date_range[2]

    # check existence of pending_room.csv
    pending_file_path = "./Data/Room/pending_room.csv"
    pending_file_exist = file.exists(pending_file_path)
    if (pending_file_exist) {
        pending = read.csv(pending_file_path)
        room_output = rbind(room_output, pending)
    }
    # output unmatched data to pending file
    keep = subset(room_output, as.Date(RoundDateTime) <= sat_date)
    drop = subset(room_output, as.Date(RoundDateTime) > sat_date)
    room_output = keep
    drop = na.omit(drop)

    output_pending = drop[order(drop$RoundDateTime, drop$Room),]
    dup_ind = which(duplicated(output_pending[, c("RoundDateTime", "Room")]))
    if (length(dup_ind) != 0)
        output_pending = output_pending[-dup_ind, ]

    # check existence of missing.csv
    missing_file_path = "./Data/Room/missing.csv"
    missing_file_exist = file.exists(missing_file_path)
    if (missing_file_exist) {
        missing = read.csv(missing_file_path)
        keep = subset(missing, as.Date(RoundDateTime) <= sat_date)
        output_missing = subset(missing, as.Date(RoundDateTime) > sat_date)
        if (nrow(keep) != 0)
            room_output = rbind(room_output, keep)
    }

    room_output = room_output[order(room_output$RoundDateTime, room_output$Room),]
    dup_ind = which(duplicated(room_output[, c("RoundDateTime", "Room")]))
    if (length(dup_ind) != 0)
        room_output = room_output[-dup_ind, ]

    ## Weather
    weather_output = data.frame()
    for (w in weather)
    {
        each_output = clean_weather(w)
        weather_output = rbind(weather_output, each_output)
    }
    weather_output = subset(weather_output, RoundDateTime <= sat_date)
    weather_output = weather_output[order(weather_output$RoundDateTime),]
    dup_ind = which(duplicated(weather_output$RoundDateTime))
    if (length(dup_ind) != 0)
        weather_output = weather_output[-dup_ind, ]

    # merge
    final = merge(room_output, weather_output, by=c("RoundDateTime"), sort=FALSE, all=TRUE)

    na_room = which(is.na(final$Room))
    if (length(na_room) != 0)
        final = final[-na_room, ]

    final = final[order(final$RoundDateTime, final$Room), ]
    dup_ind = which(duplicated(final[, c("Room", "RoundDateTime")]))
    if (length(dup_ind) != 0)
        final = final[-dup_ind, ]

    #final = na.omit(final)

    source("./Data.Cleaning/check_missingrooms.R")
    check_missingrooms(final, stop_for_missing_rooms)


    if (nrow(output_pending) != 0) {
        write.csv(output_pending, pending_file_path, row.names = F)
    } else {
        file.remove(pending_file_path)
    }

    output_missing = get0("output_missing")
    if (!is.null(output_missing) && nrow(output_missing) != 0) {
        write.csv(na.omit(output_missing), missing_file_path, row.names = F)
    } else {
        file.remove(missing_file_path)
    }

    return(final)
}