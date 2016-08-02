if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}

all_data <- read.csv("Recent WCCUSD Elections.csv", stringsAsFactors = FALSE)

# Make adjustments to rad in data
all_data <- within (all_data, {
  Incumbent <- ifelse(Incumbent == "Yes", TRUE, FALSE)
  Gender <- as.factor(Gender)
  Votes <- as.integer(gsub(",", "", Votes))
  Total.Votes.Cast <- as.integer(gsub(",", "", Total.Votes.Cast))
  Percent.of.Vote <- as.numeric(gsub("%", "", Percent.of.Vote))
  Won <- ifelse(Won == "Yes", TRUE, FALSE)
})

# create year-by-year summary
by_year <- all_data %>%
  group_by(Year) %>%
  summarise(Open.Seats = min(Openings), 
            Running = n(),  
            Competition = 1/(min(Openings)/n()),
            Voting = min(Total.Votes.Cast)/min(Openings),
            Incumbents.Running = sum(Incumbent)/min(Openings),
            Incumbents.Success = sum(Incumbent & Won) / sum(Incumbent),
            New.Members = sum(!Incumbent & Won)
            )

