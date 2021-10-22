library(dplyr)

ranks <- read.csv("final_team_rankings_mod.csv")

generateFairness <- function(year) {
  # Load in data per game
  games <<- read.csv(
    paste("fifa", year, "Fairness.csv", sep = ""),
    header = TRUE)
  
  # Make Data frame that only includes group games
  groupgames <- games[grepl("Group",games$Stage),]
  
  # Function to calculate Fairness index per team
  fairness <- function(teamname) {
    return( sum( groupgames$away_total_points[groupgames$Home.Team.Name==teamname],
                 groupgames$home_total_points[groupgames$Away.Team.Name==teamname] ) ) 
  }
  
  # Create data frame of teams
  teamsYearH <- data.frame(name = levels(factor(games$Home.Team.Name)))
  teamsYearA <- data.frame(name = levels(factor(games$Away.Team.Name)))
  
  teamsYear <- rbind(teamsYearH, teamsYearA)
  
  teamsYear <- (teamsYear[!duplicated(teamsYear$name),,drop=TRUE])
  
  teamsYear <- as.data.frame(teamsYear)
  
  colnames(teamsYear) <- c("name")
  
  # Apply function to each team in the world cup
  teamsYear$fairness = sapply(teamsYear$name,fairness)
  
  #year <- 1994
  
  # Fill a column with the current year so it can be accessed
  teamsYear$year <- year
  
  gamesRanks <- games[c("Home.Team.Name","home_rank")]
  
  gamesRanksH <- games[c("Home.Team.Name","home_rank")]
  gamesRanksA <- games[c("Away.Team.Name","away_rank")]
  
  colnames(gamesRanksA) <- c("Home.Team.Name","home_rank")
  
  gamesRanks <- rbind(gamesRanksH, gamesRanksA)
  
  
  fairnessRanks <- merge(x = teamsYear,
                         y = gamesRanks[!duplicated(gamesRanks$Home.Team.Name), ],
                         by.x="name", by.y="Home.Team.Name",
                         x.all=FALSE, y.all=FALSE, no.dups = TRUE)
  
  # Count number of games each team plays.
  numgames <- table(c(games$Home.Team.Name,games$Away.Team.Name))
  
  numgames.df <- as.data.frame(numgames)
  
  # Final position is 17-32 (or 24.5) if played 3 games (i.e. did not leave group stage)
  # Final position is 9-16 (or 12.5) if played 4 games (i.e. reached round of 16)
  # Final position is 5-8 (or 6.5) if played 5 games (i.e. reached quarter final)
  # Final position is 1-4 if played 7 games (i.e. reached semi final)
  # Can determine difference between 1st, 2nd, 3rd or 4th using other data set...
  
  teams <- data.frame(name=names(numgames),numgames=as.vector(numgames),finalposition=NA)
  
  teams$finalposition <- ifelse(teams$numgames==3,24.5,teams$finalposition)
  teams$finalposition <- ifelse(teams$numgames==4,12.5,teams$finalposition)
  teams$finalposition <- ifelse(teams$numgames==5,6.5,teams$finalposition)
  
  ranksYear <- subset(ranks, Year == year)
  
  teams$finalposition <- ifelse(teams$name==ranksYear$Champion,1,teams$finalposition)
  teams$finalposition <- ifelse(teams$name==ranksYear$Runner.Up,2,teams$finalposition)
  teams$finalposition <- ifelse(teams$name==ranksYear$Third,3,teams$finalposition)
  teams$finalposition <- ifelse(teams$name==ranksYear$Fourth,4,teams$finalposition)
  
  teamsPos <- teams[,1-3]
  
  fairnessResultsRanks <- merge(x = fairnessRanks,
                         y = teamsPos,
                         by.x="name", by.y="name",
                         x.all=FALSE)
  
  fairnessResultsRanks <- merge(x = fairnessResultsRanks,
                                y = numgames.df,
                                by.x="name", by.y="Var1",
                                x.all=FALSE, y.all=FALSE, no.dups = TRUE)
  
  # Change order of cols
  fairnessResultsRanks <- fairnessResultsRanks[, c(3,1,6,2,4,5)]
  
  return(fairnessResultsRanks)
  
}

fifa1994Fairness <- generateFairness(1994)
fifa1998Fairness <- generateFairness(1998)
fifa2002Fairness <- generateFairness(2002)
fifa2006Fairness <- generateFairness(2006)
fifa2010Fairness <- generateFairness(2010)
fifa2014Fairness <- generateFairness(2014)
fifa2018Fairness <- generateFairness(2018)

