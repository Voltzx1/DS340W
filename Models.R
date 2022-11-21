library(ggplot2)

#load in our nicely cleaned data
data <- fread('./season_2002.csv')
data <- fread('./season_2003.csv')
data <- fread('./season_2004.csv')
data <- fread('./season_2005.csv')

################## 2003 Prediction ###################
season_2002$super_bowl_winner <- 0
season_2002$super_bowl_winner[season_2002$Teams == "Buccaneers"] <- 1

droped<- season_2002$Teams

drops <- c("Teams")
season_2002<-season_2002[, !drops, with = FALSE]

mylogit <- glm(formula = super_bowl_winner ~ Wins + Loses + Ties + win_percent + total_points + total_rushing_yards
               + total_passing_yards + total_yards, data = season_2002)

# Predicting the Super Bowl winner
season_2003$super_bowl_winner <- predict(mylogit, newdata = season_2002)

season_2002$Teams <- droped

# Creating our plot
ggplot(season_2003, aes(x = reorder(Teams, super_bowl_winner), y = super_bowl_winner)) +
  geom_point(aes(colour = Teams)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none") + 
  xlab("NFL Teams") + 
  ylab("Predicted Winner") + 
  ggtitle("2003 Super Bowl Winner Prediction")
######################################################

################## 2004 Prediction ###################
season_2003$super_bowl_winner <- 0
season_2003$super_bowl_winner[season_2003$Teams == "Patriots"] <- 1

droped<- season_2003$Teams

drops <- c("Teams")
season_2003<-season_2003[, !drops, with = FALSE]

mylogit <- glm(formula = super_bowl_winner ~ Wins + Loses + Ties + win_percent + total_points + total_rushing_yards
               + total_passing_yards + total_yards, data = season_2003)

# Predicting the Super Bowl winner
season_2004$super_bowl_winner <- predict(mylogit, newdata = season_2003)

season_2003$Teams <- droped

# Creating our plot
ggplot(season_2004, aes(x = reorder(Teams, super_bowl_winner), y = super_bowl_winner)) +
  geom_point(aes(colour = Teams)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none") + 
  xlab("NFL Teams") + 
  ylab("Predicted Winner") + 
  ggtitle("2004 Super Bowl Winner Prediction")

###################################################

################## 2004 Prediction ###################
season_2004$super_bowl_winner <- 0
season_2004$super_bowl_winner[season_2004$Teams == "Patriots"] <- 1

droped<- season_2004$Teams

drops <- c("Teams")
season_2003<-season_2004[, !drops, with = FALSE]

mylogit <- glm(formula = super_bowl_winner ~ Wins + Loses + Ties + win_percent + total_points + total_rushing_yards
               + total_passing_yards + total_yards, data = season_2004)

# Predicting the Super Bowl winner
season_2005$super_bowl_winner <- predict(mylogit, newdata = season_2004)

season_2004$Teams <- droped

# Creating our plot
ggplot(season_2005, aes(x = reorder(Teams, super_bowl_winner), y = super_bowl_winner)) +
  geom_point(aes(colour = Teams)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none") + 
  xlab("NFL Teams") + 
  ylab("Predicted Winner") + 
  ggtitle("2005 Super Bowl Winner Prediction")

###################################################
