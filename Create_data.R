
# Housekeeping ------------------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

# Libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, Rsolnp, lubridate, ggplot2, plotly, dygraphs, purrr)

# Functions ---------------------------------------------------------------

season_func <- function(Date){
  if (Date < ymd("2010-06-01")){
    return("Season 2009/10")
  } else if (Date < ymd("2011-06-01")){
    return("Season 2010/11")
  } else if (Date < ymd("2012-06-01")){
    return("Season 2011/12")
  } else if (Date < ymd("2013-06-01")){
    return("Season 2012/13")
  } else if (Date < ymd("2014-06-01")){
    return("Season 2013/14")
  } else if (Date < ymd("2015-06-01")){
    return("Season 2014/15")
  } else if (Date < ymd("2016-06-01")){
    return("Season 2015/16")
  } else if (Date < ymd("2017-06-01")){
    return("Season 2016/17")
  } else if (Date < ymd("2018-06-01")){
    return("Season 2017/18")
  } else if (Date < ymd("2019-06-01")){
    return("Season 2018/19")
  } else if (Date < ymd("2020-06-01")){
    return("Season 2019/20")
  }
}


# Create data -------------------------------------------------------------

EPL1516 <- read_csv("MatchBetData/EPL1516.csv") %>% mutate(Date = dmy(Date))
ECH1516 <- read_csv("MatchBetData/ECH1516.csv") %>% mutate(Date = dmy(Date))
EPL1617 <- read_csv("MatchBetData/EPL1617.csv") %>% mutate(Date = dmy(Date))
ECH1617 <- read_csv("MatchBetData/ECH1617.csv") %>% mutate(Date = dmy(Date))
EPL1718 <- read_csv("MatchBetData/EPL1718.csv") %>% mutate(Date = dmy(Date))
ECH1718 <- read_csv("MatchBetData/ECH1718.csv") %>% mutate(Date = dmy(Date))
EPL1819 <- read_csv("MatchBetData/EPL1819.csv") %>% mutate(Date = dmy(Date))
ECH1819 <- read_csv("MatchBetData/ECH1819.csv") %>% mutate(Date = dmy(Date))

Match_data <- EPL1516 %>%
  select(Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, `BbAv>2.5`, `BbAv<2.5`) %>%
  # bind_rows(select(ECH1516, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG)) %>%
  bind_rows(select(EPL1617, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, `BbAv>2.5`, `BbAv<2.5`)) %>%
  # bind_rows(select(ECH1617, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG)) %>%
  bind_rows(select(EPL1718, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, `BbAv>2.5`, `BbAv<2.5`)) %>%
  # bind_rows(select(ECH1718, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG)) %>%
  bind_rows(select(EPL1819, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, `BbAv>2.5`, `BbAv<2.5`)) %>%
  # bind_rows(select(ECH1819, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG)) %>%
  arrange(Date) %>%
  mutate(TotalGoals = FTHG + FTAG) %>%
  mutate(Time = as.double((Date - (Date[1] - days(1))) / 3.5)) %>%     # Half weeks time intervals 
  mutate(MatchID = row_number())

names(Match_data) <- c("Div", "Date", "HomeTeam", "AwayTeam", "HomeGoals", "AwayGoals", "AvgMore25", "AvgLess25", "TotalGoals", "Time", "MatchID")

Season <- rep(NA, nrow(Match_data))

for (i in 1:nrow(Match_data)) {
  Season[i] <- season_func(Match_data$Date[i])
}

Match_data <- Match_data %>% 
  bind_cols("Season" = Season) %>% 
  select(MatchID, Div, Season, Date, Time, HomeTeam, AwayTeam, HomeGoals, AwayGoals, TotalGoals, AvgLess25, AvgMore25)

# write_csv(Match_data, path = "MatchBetData/EnglishMatchData.csv")

save(Match_data, file = "MatchBetData/EnglishMatchData.Rdata")

