if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tm)) {install.packages("tm"); library(tm)}


source("read-in-data.R")

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

