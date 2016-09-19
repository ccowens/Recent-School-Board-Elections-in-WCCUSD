#
all_data <- read.csv("Recent WCCUSD Elections.csv", stringsAsFactors = FALSE)

# Make adjustments to read-in data
all_data <- within (all_data, {
  Ballot.Designation <- ifelse(Ballot.Designation == "No ballot designation", "", Ballot.Designation) 
  Incumbent <- ifelse(Incumbent == "Yes", TRUE, FALSE)
  Gender <- as.factor(Gender)
  Votes <- as.integer(gsub(",", "", Votes))
  Total.Votes.Cast <- as.integer(gsub(",", "", Total.Votes.Cast))
  Percent.of.Vote <- as.numeric(gsub("%", "", Percent.of.Vote))
  Won <- ifelse(Won == "Yes", TRUE, FALSE)
  
  #add extra column for unique candidate identifier
  PersonID <- paste0(Last.Name, substr(gsub(".*, ", "", First.Name), 1, 1))
})

