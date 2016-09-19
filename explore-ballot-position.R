source("read-in-data.R")

if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

#ballot position

position_data <- filter(all_data, !is.na(Ballot.Position))
position_data <- mutate(position_data,
                        Ballot.Position.Insideness = pmin(Ballot.Position-1, Number.of.Candidates-Ballot.Position)) %>%
  select(Ballot.Position, Ballot.Position.Insideness, Won)

ggplot(summarise(group_by(position_data,Ballot.Position.Insideness), Success.Rate = sum(Won)/n())) +
  ggtitle("Likelihood of Winning (0..1) for Each Ballot Position by Distance from First or Last") +
  aes(x =  Ballot.Position.Insideness, y = Success.Rate) + 
  theme_minimal() + 
  geom_point()
if(!dir.exists("graphics")) dir.create("graphics") 
ggsave("ballot-position-insideness.png", path="graphics")

ggplot(summarise(group_by(position_data,Ballot.Position), Success.Rate = sum(Won)/n())) +
  ggtitle("Likelihood of Winning (0..1) for Each Position in Listing on Ballot") +
  aes(x =  Ballot.Position, y = Success.Rate) + 
  theme_minimal() +
  geom_point()
if(!dir.exists("graphics")) dir.create("graphics") 
ggsave("ballot-position.png", path="graphics")


