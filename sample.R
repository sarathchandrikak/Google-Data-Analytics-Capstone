
#heartrate_second$Time <- dmy_hms(heartrate_second$Time) # Converts to posix datetime data type
#heartrate_second$Date <- as.Date(heartrate_second$Time)
#heartrate_second$time <- format(heartrate_second$Time, "%H:%M:%S")