if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tm)) {install.packages("tm"); library(tm)}
if(!require(formattable)) {install.packages("formattable"); library(formattable)}
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

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

by_candidate <- all_data %>%
  group_by(PersonID) %>%
  summarise(Times.Ran = n(),
            Times.Won = sum(Won),
            Times.Lost = Times.Ran-Times.Won,
            Success.Rate = percent(Times.Won/Times.Ran, digits = 0)
            ) %>%
  arrange(desc(Success.Rate), desc(Times.Won), desc(Times.Lost))
formattable(by_candidate, list(Times.Won = color_bar("lightgray", proportion)), caption="WCCUSD Candidates 1995- Sorted by Success then Victories")


by_incumbent <- filter(all_data, Incumbent) %>%
  group_by(PersonID) %>%
  summarise(Times.Ran = n(),
            Times.Won = sum(Won),
            Times.Lost = Times.Ran-Times.Won,
            Success.Rate = percent(Times.Won/Times.Ran, digits = 1)
  ) %>%
  arrange(desc(Success.Rate), desc(Times.Won), desc(Times.Lost))

last_row <- summarise(by_incumbent, n(), sum(Times.Ran), sum(Times.Won), sum(Times.Lost), percent(sum(Times.Won)/sum(Times.Ran)))
colnames(last_row) <- colnames(by_incumbent)
last_row_num <- as.integer(last_row[, 1]) + 1 
last_row[, 1] <- ""
by_incumbent <- rbind(by_incumbent,last_row)


formattable(by_incumbent, list(  Times.Won = color_bar("lightgray", proportion),
                                 area(row = last_row_num) ~ formatter("span", style = "font-weight: bold"),
                                 area(row = last_row_num, col = 1:4) ~ formatter("span", style = "font-style: italic"),
                                 area(row = 1:(last_row_num-1), col = 5) ~ formatter("span", style = "color: white")
), 
            caption="WCCUSD Incumbent Candidates 1995 till Now Sorted by Success then Victories")


#look at ballot designations

#figure out the most successful full designations
designation_winners <- count(filter(all_data, Won), Ballot.Designation)
designation_losers <- count(filter(all_data, !Won), Ballot.Designation)
designation_success <- full_join(designation_winners, designation_losers, by = "Ballot.Designation")
designation_success[is.na(designation_success)] <- 0

designation_success <-  transmute(designation_success, Ballot.Designation,
                            Tries = n.x+n.y, 
                            Success.Rate = n.x/Tries) %>% 
                        filter(Tries > 1) %>%
                        arrange(desc(Success.Rate),desc(Tries))


#figure out the most successful terms in designations
all_data$Designation.Cleaned <- gsub("/|,", " ", all_data$Ballot.Designation)

term_winners <- Corpus(VectorSource(select(filter(all_data, Won), Designation.Cleaned))) %>%
  TermDocumentMatrix() %>%
  as.matrix() %>%
  data.frame()
term_winners <- cbind(rownames(term_winners), term_winners)
colnames(term_winners) <- c("Term", "n")

term_losers <- Corpus(VectorSource(select(filter(all_data, !Won), Designation.Cleaned))) %>%
  TermDocumentMatrix() %>%
  as.matrix() %>%
  data.frame()
term_losers <- cbind(rownames(term_losers), term_losers)
colnames(term_losers) <- c("Term", "n")

term_success <- full_join(term_winners, term_losers, by = "Term")
term_success[is.na(term_success)] <- 0

term_success <- transmute(term_success, Term,
                                  Tries = n.x+n.y, 
                                  Success.Rate = n.x/Tries) %>% 
                filter(Tries > 1) %>%    
                arrange(desc(Success.Rate),desc(Tries))
term_success$Success.Rate <- percent(term_success$Success.Rate)


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

ggplot(summarise(group_by(position_data,Ballot.Position), Success.Rate = sum(Won)/n())) +
  ggtitle("Likelihood of Winning (0..1) for Each Position in Listing on Ballot") +
  aes(x =  Ballot.Position, y = Success.Rate) + 
  theme_minimal() +
  geom_point()
