
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

matches[["Man United"]][135:152,] %>% full_join(Dates) %>% arrange(Date) %>% na.omit() -> tester

tester %>% timetk::tk_xts(select = 2:8, date_var = Date)


dygraph(ManUtd) %>% 
  dyAnnotation(108, text = "W", tooltip = "Win", attachAtBottom = TRUE) %>% 
  dyAnnotation(120.57, text = "D", tooltip = "Draw", attachAtBottom = TRUE)


# Returns quick -----------------------------------------------------------

ManUtd$Alpha %>% diff() %>% ts() %>% plot()
