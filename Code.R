library(data.table)
library(dplyr)

#Calling in the dataset
data <- fread('./nfl_team_stats_2002-2021.csv')

data$diff <- (data$score_away - data$score_home)
data$winner <- 1
data$winner[data$diff < 0] <- data$home[data$diff < 0]
data$winner[data$diff > 0] <- data$away[data$diff > 0]
data$loser <- 1
data$loser[data$diff > 0] <- data$home[data$diff > 0]
data$loser[data$diff < 0] <- data$away[data$diff < 0]
data$tied[data$diff == 0] <- 1
data$winner[data$winner == 1] <- NA
data$loser[data$loser == 1] <- NA

#Seperating data into each respective season
nfl_2002 <- data[data$date %between% c("2002-08-01", "2003-04-01")]
#nfl_2003 <- data[data$date %between% c("2003-08-01", "2004-04-01")]

teams <- as.data.table(unique(nfl_2002$away))

wins <- as.data.table(table(nfl_2002$winner))
loses <- as.data.table(table(nfl_2002$loser))
ties <- as.data.table(table(nfl_2002$tied))
teams$wins <- table(nfl_2002$winner)

wins <- cbind(wins, loses$N)
colnames(wins) <- c('Teams, Wins, Loses')

