library(data.table)
library(dplyr)
library(ggplot2)
library(xgboost)

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
nfl_2003 <- data[data$date %between% c("2003-08-01", "2004-04-01")]

#######################2002##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2002$winner))
loses <- as.data.table(table(nfl_2002$loser))

season_2002 <- cbind(wins, loses$N)
colnames(season_2002) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2002$Ties <- 0
x <- na.omit(nfl_2002$away[nfl_2002$tied == 1])
y <- na.omit(nfl_2002$home[nfl_2002$tied == 1])
tied_teams <- c(x,y)

season_2002$Ties[season_2002$Teams == tied_teams] <- 1

season_2002$win_percent <- season_2002$Wins/(season_2002$Wins+season_2002$Loses+season_2002$Ties)


arr1 <- c()
arr2 <- c()

for (n in season_2002$Teams){
  arr1 <- append(arr1, sum(nfl_2002$score_away[which(nfl_2002$away == n)]))
}

for (n in season_2002$Teams){
  arr2 <- append(arr2, sum(nfl_2002$score_home[which(nfl_2002$home == n)]))
}

season_2002$total_points <- mapply("+", arr1, arr2)


#################################################

######################2003##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2003$winner))
loses <- as.data.table(table(nfl_2003$loser))

season_2003 <- cbind(wins, loses$N)
colnames(season_2003) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2003$Ties <- 0
x <- na.omit(nfl_2002$away[nfl_2003$tied == 1])
y <- na.omit(nfl_2002$home[nfl_2003$tied == 1])
tied_teams <- c(x,y)

season_2003$Ties[season_2003$Teams == tied_teams] <- 1

season_2003$win_percent <- season_2003$Wins/(season_2003$Wins+season_2003$Loses+season_2003$Ties)

arr1 <- c()
arr2 <- c()

for (n in season_2003$Teams){
  arr1 <- append(arr1, sum(nfl_2003$score_away[which(nfl_2003$away == n)]))
}

for (n in season_2003$Teams){
  arr2 <- append(arr2, sum(nfl_2003$score_home[which(nfl_2003$home == n)]))
}

season_2003$total_points <- mapply("+", arr1, arr2)
#################################################
season_2002$super_bowl_winner <- 0
season_2002$super_bowl_winner[season_2002$Teams == "Buccaneers"] <- 1

droped<- season_2002$Teams

drops <- c("Teams")
season_2002<-season_2002[, !drops, with = FALSE]

mylogit <- glm(formula = super_bowl_winner ~ Wins + Loses + Ties + win_percent + total_points, data = season_2002)


season_2003$super_bowl_winner <- predict(mylogit, newdata = season_2002)

season_2002$Teams <- droped

ggplot(season_2003, aes(x = reorder(Teams, super_bowl_winner), y = super_bowl_winner)) +
  geom_point(aes(colour = Teams)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none") + 
  xlab("NFL Teams") + 
  ylab("Predicted Winner") + 
  ggtitle("2003 Super Bowl Winner Prediction")
