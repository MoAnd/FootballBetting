
# Housekeeping ------------------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

# Libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, Rsolnp, lubridate, ggplot2, plotly, dygraphs, purrr)

source("football_likelihood_func.R")
source("football_prob_func.R")

# Data --------------------------------------------------------------------

load(file = "MatchBetData/EnglishMatchData.Rdata")

teams <- Match_data$HomeTeam %>% unique() %>% sort()

# Running the likelihood and saving the different alpha/betas -------------

alpha <- rep(c(0.85, 1.15), length(teams)/2)
beta <- rnorm(length(teams), mean = 1, sd = 0.2)
rho <- 0.01
gam <- 1.4

theta <- c(alpha, beta, rho, gam)

for (k in c(387:393, 314:335)) {      # Chose which time interval we should test xi for. BEWARE: This takes forever!
  ind <- which(Match_data$Time <= k)
  
  test_list <- list()
  rho_vec <- c()
  gam_vec <- c()
  
  for ( i in c(0.001, 0.0065, 0.015, 0.02, 0.025, 0.03, 0.035, 0.05)) {     # Chose which xi we should test
    result_dyna <- solnp(pars = theta, fun = football_lik, eqfun = equal,
                         eqB = length(teams), LB = c(rep(0, (length(theta)-1)), 1),
                         UB = rep(100, length(theta)),
                         df = Match_data[ind,], teams = teams, xi = i)
    
    teamdf <- tibble(teams, result_dyna$pars[1:length(teams)], result_dyna$pars[(length(teams)+1):(2*length(teams))])
    colnames(teamdf) = c("Team", "Alpha", "Beta")
    
    test_list[[paste0("teamdf_", i)]] <- teamdf
    rho_vec <- c(rho_vec, result_dyna$pars[(length(theta) - 1)])
    gam_vec <- c(gam_vec, result_dyna$pars[(length(theta))])
    
  }
  
  xi_tester <- test_list
  
  save(xi_tester, file = paste0("Xi_test_data/xi_tester_", k, ".Rdata"))
  save(rho_vec, file = paste0("Xi_test_data/xi_tester_rho_", k, ".Rdata"))
  save(gam_vec, file = paste0("Xi_test_data/xi_tester_gam_", k, ".Rdata"))
}


# Creating the profile likelihood -----------------------------------------

S_full <- list()

for (t in c(280:289, 380:386)) {
  
  load(paste0("Xi_test_data/xi_tester_", t, ".Rdata"))
  load(paste0("Xi_test_data/xi_tester_rho_", t, ".Rdata"))
  load(paste0("Xi_test_data/xi_tester_gam_", t, ".Rdata"))
  
  game_ind <- which(Match_data$Time <= (t+1) & Match_data$Time > t)
  
  if (!is_empty(game_ind)) {
    
    S_list <- list()
    
    for (j in 1:8){
      
      S <- 0
      
      for(i in game_ind){
        
        poisdf = data.frame(matrix(0, nrow = 10, ncol = 10))
        rownames(poisdf) = c("0 away", "1 away", "2 away", "3 away", "4 away","5 away","6 away", "7 away", "8 away", "9 away")
        colnames(poisdf) = c("0 home", "1 home", "2 home", "3 home", "4 home","5 home","6 home", "7 home", "8 home", "9 home")
        
        home_ind <- which(teams %in% Match_data$HomeTeam[i])
        away_ind <- which(teams %in% Match_data$AwayTeam[i])
        
        alpha_home <- xi_tester[[j]][home_ind, 2] %>% pull()
        beta_home <- xi_tester[[j]][home_ind, 3] %>% pull()
        alpha_away <- xi_tester[[j]][away_ind, 2] %>% pull()
        beta_away <- xi_tester[[j]][away_ind, 3] %>% pull()
        
        gam <- gam_vec[j]
        rho <- rho_vec[j]
        
        for(a in 0:9){
          for(h in 0:9){
            poisdf[a+1,h+1] = football_prob(h, a, alpha_home, alpha_away, beta_home, beta_away, gam, rho)
          }
        }
        
        chance <- sum(poisdf[1,1], poisdf[1,2], poisdf[1,3], poisdf[2,1], poisdf[3,1], poisdf[2,2]) # below 2.5 goals
        
        S <- S + (Match_data$TotalGoals[i] < 2.5) * log(chance) + (Match_data$TotalGoals[i] > 2.5) * log(1-chance)
        
      }
      
      S_list[[paste0(j)]] <- S
      
    }
  }
  
  S_full[[paste0(t)]] <- S_list
  
}

S_full %>% unlist() %>% as.vector() %>% matrix(ncol = 8, byrow = TRUE) %>% colSums()  # ...and the results are in!
