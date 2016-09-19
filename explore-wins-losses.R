source("read-in-data.R")

if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(formattable)) {install.packages("formattable"); library(formattable)}



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

