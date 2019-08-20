
# Housekeeping ------------------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

# Libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, Rsolnp, lubridate, ggplot2, plotly, dygraphs, purrr, timetk, pracma, tidyquant, TTR)

source("football_prob_func.R")

# Data --------------------------------------------------------------------

load(file = "MatchBetData/EnglishMatchData.Rdata")
load(file = "ParameterData/Rho_Homeadvantage_df.Rdata")
load(file = "ParameterData/Alpha_beta_df.Rdata")

teams <- Match_data$HomeTeam %>% unique() %>% sort()

matches <- list()
for(i in teams){
  matches[[i]] <- full_join(Match_data %>%
                              filter(HomeTeam == i), Match_data %>%
                              filter(AwayTeam == i)) %>%
    arrange(Date)
}

Promotion_teams <- list(
  "Season 2019/20" = c("Norwich", "Sheffield Utd", "Aston Villa"), # Check Sheffield's name in teams
  "Season 2018/19" = c("Wolves", "Cardiff", "Fulham"),
  "Season 2017/18" = c("Newcastle", "Brighton", "Huddersfield"),
  "Season 2016/17" = c("Burnley", "Middlesbrough", "Hull"),
  "Season 2015/16" = c("Bournemouth", "Watford", "Norwich"),
  "Season 2014/15" = c("Leicester", "Burnley", "QPR"),             # Check QPR's name in teams
  "Season 2013/14" = c("Cardiff", "Hull", "Crystal Palace"),
  "Season 2012/13" = c("Reading", "Southampton", "West Ham")
)


# Moving Average ----------------------------------------------------------

Alpha_beta_df %>% 
  mutate(Value = round(Value, digits = 2)) %>% 
  filter(Type == "Alpha") %>% 
  filter(Team == "Man United" | Team == "Man City" | Team == "Liverpool") %>% 
  ggplot(aes(x = Time, y = Value, col = Team)) + geom_line() + 
  geom_ma(ma_fun = SMA, n = 10) + geom_ma(ma_fun = SMA, n = 50)

Alpha_beta_df %>% 
  mutate(Value = round(Value, digits = 2)) %>% 
  filter(Type == "Beta") %>% 
  filter(Team == "Man United" | Team == "Man City" | Team == "Liverpool") %>% 
  ggplot(aes(x = Time, y = Value, col = Team)) + geom_line() + 
  geom_ma(ma_fun = SMA, n = 10) + geom_ma(ma_fun = SMA, n = 50)

ManUtd <- Alpha_beta_df %>% 
  spread(Type, Value) %>% 
  filter(Team  == "Man United") %>% 
  select(-Team)

ManUtd <- ManUtd %>% 
  bind_cols(tibble("MA10_Alpha" = movavg(ManUtd$Alpha, 10, type = "s"))) %>% 
  bind_cols(tibble("MA10_Beta" = movavg(ManUtd$Beta, 10, type = "s"))) %>% 
  bind_cols(tibble("MA50_Alpha" = movavg(ManUtd$Alpha, 50, type = "s"))) %>% 
  bind_cols(tibble("MA50_Beta" = movavg(ManUtd$Beta, 50, type = "s")))

ManUtd %>% 
  gather(Type, Value, 2:7) %>% 
  ggplot(aes(x = Time, y = Value, col = Type)) + geom_line()

Dates <- ymd("2019-01-02"):ymd("2019-05-12") %>% 
  as_date() %>% enframe(name = NULL) %>% 
  bind_cols("Time" = round(seq(355.4286, 392.5714, length.out = 131), digits = 2))
names(Dates) <- c("Date", "Time")

tester2 <- matches[["Man United"]][135:152,] %>% 
  mutate(Time = round(Time, digits = 2)) %>% 
  full_join(Dates) %>% 
  arrange(Date)

tester <- tester2 %>% 
  full_join(ManUtd[297:334,], by = "Time") %>% 
  arrange(Time) %>% 
  select(Date, Alpha, Beta, MA10_Alpha, MA10_Beta, MA50_Alpha, MA50_Beta)

for (i in 3:nrow(tester)) {
  if (is.na(tester$Alpha[i])) {
    tester$Alpha[i] <- tester$Alpha[i - 1]
  } 
  
  if (is.na(tester$Beta[i])) {
    tester$Beta[i] <- tester$Beta[i - 1]
  }
  
  if (is.na(tester$MA10_Alpha[i])) {
    tester$MA10_Alpha[i] <- tester$MA10_Alpha[i - 1]
  }
  
  if (is.na(tester$MA10_Beta[i])) {
    tester$MA10_Beta[i] <- tester$MA10_Beta[i - 1]
  }
  
  if (is.na(tester$MA50_Alpha[i])) {
    tester$MA50_Alpha[i] <- tester$MA50_Alpha[i - 1]
  }
  
  if (is.na(tester$MA50_Beta[i])) {
    tester$MA50_Beta[i] <- tester$MA50_Beta[i - 1]
  }
}

dyg <- tester %>% na.omit() %>% timetk::tk_xts(select = 2:5, date_var = Date) %>% dygraph() %>% 
  dyAnnotation("2019-01-13", text = "W", tooltip = "1359, Away, Win, 1-0", attachAtBottom = TRUE)

dyg <- dyg %>% dyAnnotation("2019-01-19", text = "W", tooltip = "1364, Home, Win, 2-1", attachAtBottom = TRUE)

matches[["Man United"]][138:152,] %>% nrow()

for (i in 1:15) {
  if(matches[["Man United"]][138:152,]$HomeTeam[i] == "Man United" & matches[["Man United"]][138:152,]$HomeGoals[i] > matches[["Man United"]][138:152,]$AwayGoals[i]){
    dyg <- dyg %>% dyAnnotation(matches[["Man United"]][138:152,]$Date[i], text = "W", tooltip = paste(matches[["Man United"]][138:152,]$MatchID[i], ", Home Win, ", matches[["Man United"]][138:152,]$HomeGoals[i], "-", matches[["Man United"]][138:152,]$AwayGoals[i], sep = ""), attachAtBottom = TRUE)
    print("Home Win")
  } else if(matches[["Man United"]][138:152,]$AwayTeam[i] == "Man United" & matches[["Man United"]][138:152,]$HomeGoals[i] < matches[["Man United"]][138:152,]$AwayGoals[i]){
    dyg <- dyg %>% dyAnnotation(matches[["Man United"]][138:152,]$Date[i], text = "W", tooltip = paste(matches[["Man United"]][138:152,]$MatchID[i], ", Away Win, ", matches[["Man United"]][138:152,]$HomeGoals[i], "-", matches[["Man United"]][138:152,]$AwayGoals[i], sep = ""), attachAtBottom = TRUE)
    print("Away Win")
  } else if(matches[["Man United"]][138:152,]$HomeTeam[i] == "Man United" & matches[["Man United"]][138:152,]$HomeGoals[i] < matches[["Man United"]][138:152,]$AwayGoals[i]){
    dyg <- dyg %>% dyAnnotation(matches[["Man United"]][138:152,]$Date[i], text = "L", tooltip = paste(matches[["Man United"]][138:152,]$MatchID[i], ", Home Loss, ", matches[["Man United"]][138:152,]$HomeGoals[i], "-", matches[["Man United"]][138:152,]$AwayGoals[i], sep = ""), attachAtBottom = TRUE)
    print("Home Loss")
  } else if(matches[["Man United"]][138:152,]$AwayTeam[i] == "Man United" & matches[["Man United"]][138:152,]$HomeGoals[i] > matches[["Man United"]][138:152,]$AwayGoals[i]){
    dyg <- dyg %>% dyAnnotation(matches[["Man United"]][138:152,]$Date[i], text = "L", tooltip = paste(matches[["Man United"]][138:152,]$MatchID[i], ", Away Loss, ", matches[["Man United"]][138:152,]$HomeGoals[i], "-", matches[["Man United"]][138:152,]$AwayGoals[i], sep = ""), attachAtBottom = TRUE)
    print("Away Loss")
  } else {
    dyg <- dyg %>% dyAnnotation(matches[["Man United"]][138:152,]$Date[i], text = "D", tooltip = paste(matches[["Man United"]][138:152,]$MatchID[i], ", Draw, ", matches[["Man United"]][138:152,]$HomeGoals[i], "-", matches[["Man United"]][138:152,]$AwayGoals[i], sep = ""), attachAtBottom = TRUE)
    print("Draw")
  }
}

# Returns quick -----------------------------------------------------------

ManUtd$Alpha %>% diff() %>% ts() %>% plot()
