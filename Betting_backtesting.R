
# Housekeeping ------------------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

# Libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, Rsolnp, lubridate, ggplot2, plotly, dygraphs, purrr)

source("football_prob_func.R")

# Data --------------------------------------------------------------------

load(file = "MatchBetData/EnglishMatchData.Rdata")
load(file = "ParameterData/Rho_Homeadvantage_df.Rdata")
load(file = "ParameterData/Alpha_beta_df.Rdata")

teams <- Match_data$HomeTeam %>% unique() %>% sort()

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

# Plotting parameters -----------------------------------------------------

p <- Alpha_beta_df %>% 
  mutate(Value = round(Value, digits = 2)) %>% 
  filter(Type == "Alpha") %>% 
  filter(Team == "Man United" | Team == "Liverpool" | Team == "Tottenham" | Team == "Man City" | Team == "Arsenal" | Team == "Chelsea") %>% 
  ggplot(aes(x = Time, y = Value, col = Team)) + geom_line()

ggplotly(p)

q <- Alpha_beta_df %>% 
  filter(Type == "Beta") %>% 
  filter(Team == "Man United" | Team == "Liverpool" | Team == "Tottenham" | Team == "Man City" | Team == "Arsenal" | Team == "Chelsea") %>% 
  ggplot(aes(x = Time, y = Value, col = Team)) + geom_line()

ggplotly(q)

Rho_lambda_df %>% filter(Parameter == "HomeAdvantage") %>% ggplot(aes(x = Time, y = Value, col = Parameter)) + geom_line()
Rho_lambda_df %>% filter(Parameter == "Rho") %>% ggplot(aes(x = Time, y = Value, col = Parameter)) + geom_line()
Rho_lambda_df %>% filter(Parameter == "Rho", Time > 100) %>% ggplot(aes(x = Time, y = Value, col = Parameter)) + geom_line()

# Betting backtesting -----------------------------------------------------

bankroll = 500
wagering_size = 50
odds_placed = 0
lost = 0

for (t in 200:392){    # which time interval we want to backtest
  
  game_ind <- which(Match_data$Time < (t+1) & Match_data$Time > t)
  
  if (!is_empty(game_ind)) {
    
    for(i in game_ind){
      
      poisdf = data.frame(matrix(0, nrow = 10, ncol = 10))
      rownames(poisdf) = c("0 away", "1 away", "2 away", "3 away", "4 away","5 away","6 away", "7 away", "8 away", "9 away")
      colnames(poisdf) = c("0 home", "1 home", "2 home", "3 home", "4 home","5 home","6 home", "7 home", "8 home", "9 home")
      
      # home_ind <- which(teams %in% Match_data$HomeTeam[i])
      # away_ind <- which(teams %in% Match_data$AwayTeam[i])
      
      alpha_home <- Alpha_beta_df %>% filter(Time == t, Team == Match_data$HomeTeam[i], Type == "Alpha") %>% select(Value) %>% pull()
      beta_home <- Alpha_beta_df %>% filter(Time == t, Team == Match_data$HomeTeam[i], Type == "Beta") %>% select(Value) %>% pull()
      alpha_away <- Alpha_beta_df %>% filter(Time == t, Team == Match_data$AwayTeam[i], Type == "Alpha") %>% select(Value) %>% pull()
      beta_away <- Alpha_beta_df %>% filter(Time == t, Team == Match_data$AwayTeam[i], Type == "Beta") %>% select(Value) %>% pull()
      
      gam <- Rho_lambda_df %>% filter(Time == t, Parameter == "HomeAdvantage") %>% select(Value) %>% pull()
      rho <- Rho_lambda_df %>% filter(Time == t, Parameter == "Rho") %>% select(Value) %>% pull()
      
      for(a in 0:9){
        for(h in 0:9){
          poisdf[a+1,h+1] = football_prob(h, a, alpha_home, alpha_away, beta_home, beta_away, gam, rho)
        }
      }
      
      chance <- sum(poisdf[1,1], poisdf[1,2], poisdf[1,3], poisdf[2,1], poisdf[3,1], poisdf[2,2])   # below 2.5 goals
      
      bookmaker <- (1/Match_data$AvgLess25[i]) / (1/Match_data$AvgMore25[i] + 1/Match_data$AvgLess25[i])  # Real bookmaker's probability for less than 2.5 goals
      bookmaker2 <- (1/Match_data$AvgMore25[i]) / (1/Match_data$AvgMore25[i] + 1/Match_data$AvgLess25[i]) # Real bookmaker's probability for more than 2.5 goals
      
      if(chance - bookmaker > 0.07 & chance > 0.65 & !(Match_data$HomeTeam[i] %in% Promotion_teams[[Match_data$Season[i]]]) & !(Match_data$AwayTeam[i] %in% Promotion_teams[[Match_data$Season[i]]])){
        cat("Betting on", Match_data$HomeTeam[i], " vs. ", 
            Match_data$AwayTeam[i], "less than 2.5 goals at odds", 
            Match_data$AvgLess25[i],"=", bookmaker * 100, "%, with my prediction at", 
            chance * 100, "%", sep = " ")
        cat(sep = "\n")
        odds_placed = odds_placed +1
        
        if(Match_data$TotalGoals[i] < 3){
          bankroll = bankroll + wagering_size * (Match_data$AvgLess25[i]-1)
          cat("I won", wagering_size * (Match_data$AvgLess25[i]-1), 
              "and my current bankroll is", bankroll, sep = " ")
          cat(sep = "\n")
        }
        
        else{
          bankroll = bankroll - wagering_size
          lost = lost + 1
          cat("I lost and my current bankroll is", bankroll)
          cat(sep ="\n")
        }
        
      } 
      # else if((1-chance) - bookmaker2 > 0.05 & (1-chance) > 0.65){
      #   cat("I'm betting on", Match_data$HomeTeam[i], " vs. ", Match_data$AwayTeam[i], "more than 2.5 goals at odds", Match_data$AvgMore25[i],"=", bookmaker2 * 100, "%, with my prediction at", (1-chance) * 100, "%", sep = " ")
      #   cat(sep = "\n")
      #   odds_placed = odds_placed +1
      #   
      #   if(Match_data$TotalGoals[i] > 2){
      #     bankroll = bankroll + wagering_size * (Match_data$AvgMore25[i]-1)
      #     cat("I won", wagering_size * (Match_data$AvgMore25[i]-1), "and my current bankroll is", bankroll, sep = " ")
      #     cat(sep = "\n")
      #   }
      #   
      #   else{
      #     bankroll = bankroll - wagering_size
      #     lost = lost + 1
      #     cat("I lost and my current bankroll is", bankroll)
      #     cat(sep ="\n")
      #   }
      #   
      # }
      
    }
  }
}






