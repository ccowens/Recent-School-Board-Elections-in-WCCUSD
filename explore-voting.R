source("read-in-data.R")

if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}


# create year-by-year summary
by_election <- all_data %>%
  group_by(Year) %>%
  summarise(Open.Seats = sum(Won), 
            Candidates = n(),
            Competition = Candidates/Open.Seats,
            Voting.On.Race = as.integer(min(Total.Votes.Cast)/Open.Seats),
            Incumbent.Running.Rate = sum(Incumbent)/Open.Seats,
            Incumbent.Success.Rate = sum(Incumbent & Won) / sum(Incumbent),
            New.Members = sum(!Incumbent & Won)
  )

ggplot(data = by_election) +
  ggtitle("Total Voting on School Board Race") +
  theme_minimal() +
  aes(x = Year, y = Voting.On.Race) + 
  geom_text(aes(label=Year, color="red")) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position="none") +
  scale_y_continuous(breaks = seq(10000,60000,10000), limits=c(20000,60000))
