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
  teams <- data.frame(name = levels(factor(games$Home.Team.Name)))
  
  
  # Apply function to each team in the world cup
  teams$fairness = sapply(teams$name,fairness)
  
  return(teams)
}

fifa1994Fairness <- generateFairness(1994)
fifa1998Fairness <- generateFairness(1998)
fifa2002Fairness <- generateFairness(2002)
fifa2006Fairness <- generateFairness(2006)
fifa2010Fairness <- generateFairness(2010)
fifa2014Fairness <- generateFairness(2014)

