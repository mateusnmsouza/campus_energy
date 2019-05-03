compute_usage <- function(final, room_randomization) {
	# Create hourofsample, to which we will aggregate data 
	final$hourofsample.f = as.factor(substr(final$RoundDateTime, 1, 13))

	#### Create hourly means for setpoints
	hourly_stpt = aggregate(final$CTL.STPT, final[, c("Room", "hourofsample.f")], mean)
	hourly_stpt <- hourly_stpt %>% rename("CTL.STPT" = x)
	hourly_stpt = merge(hourly_stpt, room_randomization, by="Room")

	#### Adjust the outdoor temperaure data
	weatherdata = unique(final[, c("hourofsample.f", "HOURLYDRYBULBTEMPF")])

	#### Merge hourly room info with weather data
	usage_predictions = merge(hourly_stpt, weatherdata, by="hourofsample.f")

	#### Generate variable identifying if thermostat is above/below outside temperature
	usage_predictions$outdiffs = usage_predictions$CTL.STPT - usage_predictions$HOURLYDRYBULBTEMPF

	#### Creating the 1 degree bins of "outdiffs"
	usage_predictions$outdiffs.binned = ceiling((usage_predictions$outdiffs-0.5))

	#################### GENERATE THE PREDICTIONS FOR ENERGY USAGE ############################
	## Steam
	usage_predictions$Hourly_STEAM_Usage = 0
	usage_predictions$Hourly_STEAM_Usage = ifelse(usage_predictions$outdiffs.binned>0 & usage_predictions$simple_room_type=="Single Bedroom", 9.6*0.0472917*usage_predictions$outdiffs.binned,usage_predictions$Hourly_STEAM_Usage)
	usage_predictions$Hourly_STEAM_Usage = ifelse(usage_predictions$outdiffs.binned>0 & usage_predictions$simple_room_type=="Double Bedroom", 9.6*0.0378844*usage_predictions$outdiffs.binned,usage_predictions$Hourly_STEAM_Usage)

	## Chilled Water
	usage_predictions$Hourly_CHW_Usage = 0
	usage_predictions$Hourly_CHW_Usage = ifelse(usage_predictions$outdiffs.binned<0 & usage_predictions$simple_room_type=="Single Bedroom", (-0.7942)*usage_predictions$outdiffs.binned,usage_predictions$Hourly_CHW_Usage)
	usage_predictions$Hourly_CHW_Usage = ifelse(usage_predictions$outdiffs.binned<0 & usage_predictions$simple_room_type=="Double Bedroom", (-0.9678)*usage_predictions$outdiffs.binned,usage_predictions$Hourly_CHW_Usage)

	### Total Energy
	usage_predictions$Hourly_ENERGY_Usage = (usage_predictions$Hourly_STEAM_Usage + usage_predictions$Hourly_CHW_Usage) / 1000

	return(data.frame(Room=usage_predictions$Room, DateTime=usage_predictions$hourofsample.f, Usage=usage_predictions$Hourly_ENERGY_Usage))
}
