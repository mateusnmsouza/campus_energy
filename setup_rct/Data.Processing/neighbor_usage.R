# test df
# dat = data.frame(
#     Room = c("1008A", "1013A", "2040B", "3023A"),
#     Usage = abs(rnorm(4, 25, 10))
# )

# input a three-column df with Room, TreatmentType and Usage
# return a four-column df with Room, Usage, Efficient.Neighbors, All.Neighbors
compute_neighbor_usage <- function(df, treatment) {
	df = as.data.frame(df)
	for (each_room in df$Room) {
		if (df$TreatmentType[df$Room == each_room] == treatment) {
			df$All.Neighbors[df$Room == each_room] = mean(df$Usage[df$Room != each_room], na.rm=T)
    		df$Efficient.Neighbors[df$Room == each_room] = quantile(df$Usage[df$Room != each_room], 0.15, na.rm=T)
		}
	}
	return(df)
}

# input a five-column df with Room, TreatmentType, Usage, Efficient.Neighbors, All.Neighbors
# return a four-column df with Room, Standing, ComparePercent, TreatmentType
compare_to_neighbor <- function(df, treatment_list_path=NULL) {
	min.ind = apply(df[,-c(1,2)], 1, function(x){which.min(x)})
	max.ind = apply(df[,-c(1,2)], 1, function(x){which.max(x)})
	res.ind = vector()
	res.ind[min.ind == 1] = 1
	res.ind[min.ind != 1] = 2
	res.ind[max.ind == 1] = 3
	df = transform(df, Standing=c("Great", "Good", "Bad")[res.ind])

	# equal cases
	df$Standing[df$Usage == df$Efficient.Neighbors] = "Good"
	df$Standing[df$Usage == df$All.Neighbors] = "Bad"

	## compute comparison
	great.dat = df %>% filter(Standing == "Great") %>%
	    mutate(ComparePercent = round((Efficient.Neighbors - Usage) / Efficient.Neighbors * 100, digits=2))
	good.dat = df %>% filter(Standing == "Good") %>%
	    mutate(ComparePercent = round((Usage - Efficient.Neighbors) / Efficient.Neighbors * 100, digits=2))
	bad.dat = df %>% filter(Standing == "Bad") %>%
	    mutate(ComparePercent = round((Usage - All.Neighbors) / All.Neighbors * 100, digits=2))
	df <- rbind(great.dat, good.dat, bad.dat)

	# drop usage variables and sort by room
	df = df[, -grep("Usage|Neighbors", colnames(df))]
	df = df[order(df$Room), ]

	# merge with TreatmentType
	#######################

	return(df)
}