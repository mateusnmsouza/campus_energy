check_missingrooms <- function(final, stop_for_missing_rooms) {
	room_randomization = read_csv("./Data.Processing/room_randomization.csv")
	room_randomization <- room_randomization %>% rename("Room"=room)
	final.tmp = inner_join(final, room_randomization, by="Room")
	# final.tmp <- subset(final.tmp, RoundDateTime >= as.Date("09/03/17", "%m/%d/%y"))

	allrooms = unique(room_randomization$Room)
	matchedrooms = unique(final.tmp$Room)
	nonexistrooms = setdiff(allrooms, matchedrooms)

	blankrooms = aggregate(final.tmp$CTL.STPT, by=list(final.tmp$Room), FUN=function(x) {sum(is.na(x))})
	blankrooms = blankrooms[which(blankrooms$x>50),]
	blankrooms = unique(blankrooms$Group.1)
	nonblankrooms = setdiff(allrooms, blankrooms)

	missingrooms = union(setdiff(matchedrooms, nonblankrooms), nonexistrooms)
	if (stop_for_missing_rooms & length(missingrooms) != 0) {
	    cat("Missing rooms:", sort(missingrooms), "\n")
	    stop("Missing Rooms Found.\n")
	}
}