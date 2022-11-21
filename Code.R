# Attaching necessary libraries
library(data.table)
library(dplyr)
library(xgboost)

# Reading in the data
data <- fread('./nfl_team_stats_2002-2021.csv')

# Creating data for wins, losses, and ties
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
nfl_2004 <- data[data$date %between% c("2004-08-01", "2005-04-01")]
nfl_2005 <- data[data$date %between% c("2005-08-01", "2006-04-01")]

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

# Win percentages
season_2002$win_percent <- season_2002$Wins/(season_2002$Wins+season_2002$Loses+season_2002$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2002$Teams){
  arr1 <- append(arr1, sum(nfl_2002$score_away[which(nfl_2002$away == n)]))
}

for (n in season_2002$Teams){
  arr2 <- append(arr2, sum(nfl_2002$score_home[which(nfl_2002$home == n)]))
}

season_2002$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2002$Teams){
  arr1 <- append(arr1, sum(nfl_2002$rushing_yards_away[which(nfl_2002$away == n)]))
}

for (n in season_2002$Teams){
  arr2 <- append(arr2, sum(nfl_2002$rushing_yards_home[which(nfl_2002$home == n)]))
}

season_2002$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2002$Teams){
  arr1 <- append(arr1, sum(nfl_2002$passing_yards_away[which(nfl_2002$away == n)]))
}

for (n in season_2002$Teams){
  arr2 <- append(arr2, sum(nfl_2002$passing_yards_home[which(nfl_2002$home == n)]))
}

season_2002$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2002$total_yards <- season_2002$total_rushing_yards + season_2002$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2002$Teams){
  arr1 <- append(arr1, sum(nfl_2002$turnovers_away[which(nfl_2002$away == n)]))
}

for (n in season_2002$Teams){
  arr2 <- append(arr2, sum(nfl_2002$turnovers_home[which(nfl_2002$home == n)]))
}

season_2002$total_turnovers <- mapply("+", arr1, arr2)

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

# Calculating team records
for (n in season_2003$Teams){
  arr1 <- append(arr1, sum(nfl_2003$score_away[which(nfl_2003$away == n)]))
}

for (n in season_2003$Teams){
  arr2 <- append(arr2, sum(nfl_2003$score_home[which(nfl_2003$home == n)]))
}

season_2003$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2003$Teams){
  arr1 <- append(arr1, sum(nfl_2003$rushing_yards_away[which(nfl_2003$away == n)]))
}

for (n in season_2003$Teams){
  arr2 <- append(arr2, sum(nfl_2003$rushing_yards_home[which(nfl_2003$home == n)]))
}

season_2003$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2003$Teams){
  arr1 <- append(arr1, sum(nfl_2003$passing_yards_away[which(nfl_2003$away == n)]))
}

for (n in season_2003$Teams){
  arr2 <- append(arr2, sum(nfl_2003$passing_yards_home[which(nfl_2003$home == n)]))
}

season_2003$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2003$total_yards <- season_2003$total_rushing_yards + season_2003$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2003$Teams){
  arr1 <- append(arr1, sum(nfl_2003$turnovers_away[which(nfl_2003$away == n)]))
}

for (n in season_2003$Teams){
  arr2 <- append(arr2, sum(nfl_2003$turnovers_home[which(nfl_2003$home == n)]))
}

season_2003$total_turnovers <- mapply("+", arr1, arr2)

#################################################

######################2004##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2004$winner))
loses <- as.data.table(table(nfl_2004$loser))

season_2004 <- cbind(wins, loses$N)
colnames(season_2004) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2004$Ties <- 0
x <- na.omit(nfl_2002$away[nfl_2004$tied == 1])
y <- na.omit(nfl_2002$home[nfl_2004$tied == 1])
tied_teams <- c(x,y)

season_2004$Ties[season_2004$Teams == tied_teams] <- 1

season_2004$win_percent <- season_2004$Wins/(season_2004$Wins+season_2004$Loses+season_2004$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2004$Teams){
  arr1 <- append(arr1, sum(nfl_2004$score_away[which(nfl_2004$away == n)]))
}

for (n in season_2004$Teams){
  arr2 <- append(arr2, sum(nfl_2004$score_home[which(nfl_2004$home == n)]))
}

season_2004$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2004$Teams){
  arr1 <- append(arr1, sum(nfl_2004$rushing_yards_away[which(nfl_2004$away == n)]))
}

for (n in season_2004$Teams){
  arr2 <- append(arr2, sum(nfl_2004$rushing_yards_home[which(nfl_2004$home == n)]))
}

season_2004$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2004$Teams){
  arr1 <- append(arr1, sum(nfl_2004$passing_yards_away[which(nfl_2004$away == n)]))
}

for (n in season_2004$Teams){
  arr2 <- append(arr2, sum(nfl_2004$passing_yards_home[which(nfl_2004$home == n)]))
}

season_2004$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2004$total_yards <- season_2004$total_rushing_yards + season_2004$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2004$Teams){
  arr1 <- append(arr1, sum(nfl_2004$turnovers_away[which(nfl_2004$away == n)]))
}

for (n in season_2004$Teams){
  arr2 <- append(arr2, sum(nfl_2004$turnovers_home[which(nfl_2004$home == n)]))
}

season_2004$total_turnovers <- mapply("+", arr1, arr2)

#################################################

######################2005##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2005$winner))
loses <- as.data.table(table(nfl_2005$loser))

season_2005 <- cbind(wins, loses$N)
colnames(season_2005) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2005$Ties <- 0
x <- na.omit(nfl_2002$away[nfl_2005$tied == 1])
y <- na.omit(nfl_2002$home[nfl_2005$tied == 1])
tied_teams <- c(x,y)

season_2005$Ties[season_2005$Teams == tied_teams] <- 1

season_2005$win_percent <- season_2005$Wins/(season_2005$Wins+season_2005$Loses+season_2005$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2005$Teams){
  arr1 <- append(arr1, sum(nfl_2005$score_away[which(nfl_2005$away == n)]))
}

for (n in season_2005$Teams){
  arr2 <- append(arr2, sum(nfl_2005$score_home[which(nfl_2005$home == n)]))
}

season_2005$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2005$Teams){
  arr1 <- append(arr1, sum(nfl_2005$rushing_yards_away[which(nfl_2005$away == n)]))
}

for (n in season_2005$Teams){
  arr2 <- append(arr2, sum(nfl_2005$rushing_yards_home[which(nfl_2005$home == n)]))
}

season_2005$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2005$Teams){
  arr1 <- append(arr1, sum(nfl_2005$passing_yards_away[which(nfl_2005$away == n)]))
}

for (n in season_2005$Teams){
  arr2 <- append(arr2, sum(nfl_2005$passing_yards_home[which(nfl_2005$home == n)]))
}

season_2005$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2005$total_yards <- season_2005$total_rushing_yards + season_2005$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2005$Teams){
  arr1 <- append(arr1, sum(nfl_2005$turnovers_away[which(nfl_2005$away == n)]))
}

for (n in season_2005$Teams){
  arr2 <- append(arr2, sum(nfl_2005$turnovers_home[which(nfl_2005$home == n)]))
}

season_2005$total_turnovers <- mapply("+", arr1, arr2)

#################################################

fwrite(season_2002, "season_2002.csv")
fwrite(season_2003, "season_2002.csv")
fwrite(season_2004, "season_2002.csv")
fwrite(season_2005, "season_2005.csv")

