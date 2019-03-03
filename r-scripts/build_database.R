#dependencies
library(lubridate)
library(plyr)
library(stringr)

#contruct dataframe
files <- list.files(getwd(), pattern=".csv$", recursive=T)
files.list <- as.list(files)
data.list <- lapply(files, read.table, sep=",", header=T, skip=0)
data <- do.call(rbind, data.list)

#parse data
names(data) <- str_to_lower(names(data), locale="en")
data <- cbind(data, data.frame(do.call('rbind', str_split(data$date, " "))))
names(data)[12:16] <- c("wday", "mday", "month.abb", "year", "wnum")
data$date <- str_c(data$month.abb, data$mday, data$year, sep="-")
data$date <- as.POSIXct(data$date, format="%b-%d-%Y")
data$wday <- str_sub(data$wday, 2, -2)
data <- cbind(data, data.frame(do.call("rbind", str_split(data$ft, "-"))))
names(data)[17:18] <- c("score.1", "score.2")
data$score.1 <- as.numeric(str_sub(data$score.1, 1, 2)) #eliminate asterisks
data$score.2 <- as.numeric(str_sub(data$score.2, 1, 2)) #eliminate asterisks
data$result.1 <- NA
data$result.2 <- NA
data$result.1[data$score.1 > data$score.2] <- "win"
data$result.1[data$score.1 == data$score.2] <- "tie"
data$result.1[data$score.1 < data$score.2] <- "loss"
data$result.2[data$result.1 == "win"] <- "loss"
data$result.2[data$result.1 =="tie"] <- "tie"
data$result.2[data$result.1 == "loss"] <- "win"

data$team.1 <- str_sub(data.frame(do.call('rbind', str_split(as.character(data$team.1), "\\(")))[,1],1,-2)
data$team.2 <- str_sub(data.frame(do.call('rbind', str_split(as.character(data$team.2), "\\(")))[,1],1,-2)

data$month <- month(data$date)

data <- data[,c("stage", "round", "date", "year", "month", "mday", "wday", 
                "team.1", "team.2", "score.1", "score.2", "conf.1", "conf.2",
                "result.1", "result.2")]
