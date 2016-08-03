if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tm)) {install.packages("tm"); library(tm)}

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
})

# create year-by-year summary
by_election <- all_data %>%
  group_by(Year) %>%
  summarise(Open.Seats = sum(Won), 
            Candidates = n(),
            Competition = Candidates/Open.Seats,
            Voting.On.Race = min(Total.Votes.Cast)/Open.Seats,
            Incumbent.Running.Rate = sum(Incumbent)/Open.Seats,
            Incumbent.Success.Rate = sum(Incumbent & Won) / sum(Incumbent),
            New.Members = sum(!Incumbent & Won)
            )

#look at ballot designations

#figure out the most successful full designations
designation_winners <- count(filter(all_data, Won), Ballot.Designation)
designation_losers <- count(filter(all_data, !Won), Ballot.Designation)
designation_success <- full_join(designation_winners, designation_losers, by = "Ballot.Designation")
designation_success[is.na(designation_success)] <- 0

designation_success <-  transmute(designation_success, Ballot.Designation,
                            Tries = n.x+n.y, 
                            Success.Rate = n.x/Tries) %>% 
                        arrange(desc(Success.Rate),desc(Tries))



all_data$Designation.cleaned <- gsub("/|,", " ", all_data$Ballot.Designation)

terms <- Corpus(VectorSource(select(filter(all_data, Won), Designation.Cleaned))) %>%
TermDocumentMatrix() %>%
as.matrix() %>%
data.frame()

terms <- cbind(rownames(terms), terms)
colnames(terms) <- c("Term","Frequency")
arrange(terms, desc(Frequency))

terms <- Corpus(VectorSource(select(filter(all_data, !Won), Designation.No.Slash))) %>%
  TermDocumentMatrix() %>%
  as.matrix() %>%
  data.frame()

terms <- cbind(rownames(terms), terms)
colnames(terms) <- c("Term","Frequency")
arrange(terms, desc(Frequency))
