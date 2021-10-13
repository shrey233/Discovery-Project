# Load in data per game
games <- read.csv("fifa2018Fairness.csv")


# Count number of games each team plays.
numgames <- table(c(games$Home.Team.Name,games$Away.Team.Name))

# Final position is 17-32 (or 24.5) if played 3 games (i.e. did not leave group stage)
# Final position is 9-16 (or 12.5) if played 4 games (i.e. reached round of 16)
# Final position is 5-8 (or 6.5) if played 5 games (i.e. reached quarter final)
# Final position is 1-4 if played 7 games (i.e. reached semi final)
# Can determine difference between 1st, 2nd, 3rd or 4th using other data set...

teams <- data.frame(name=names(numgames),numgames=as.vector(numgames),finalposition=NA)

teams$finalposition <- ifelse(teams$numgames==3,24.5,teams$finalposition)
teams$finalposition <- ifelse(teams$numgames==4,12.5,teams$finalposition)
teams$finalposition <- ifelse(teams$numgames==5,6.5,teams$finalposition)

## Note: need other data set to determine finalposition for 1-4
