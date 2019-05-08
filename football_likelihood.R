## Housekeeping ----
rm(list = ls()) # Clear all
graphics.off() # Close all
# ctrl + l # Clears console

## Libraries ----
library(tidyverse)
library(Rsolnp)
library(lubridate)

## Import data ----
E0 <- read_csv("~/Downloads/E0.csv") %>% mutate(Date = dmy(Date))
#first20 <- E0 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG); first20 <- first20[1:20,]


## Teams & Matches ----
teams <- c()
for(i in E0$HomeTeam[1:40]){
  if(!(i %in% teams)){
    teams <- c(teams, i)
  }
}
teams <- sort(teams)

matches <- list("Arsenal" = tibble(), "Bournemouth" = tibble(), "Brighton" = tibble(), "Burnley" = tibble(), "Chelsea" = tibble(),
                "Crystal Palace" = tibble(), "Everton" = tibble(), "Huddersfield" = tibble(), "Leicester" = tibble(), 
                "Liverpool" = tibble(), "Man City" = tibble(), "Man United" = tibble(), "Newcastle" = tibble(), "Southampton" = tibble(), 
                "Stoke" = tibble(), "Swansea" = tibble(),  "Tottenham" = tibble(), "Watford" = tibble(), "West Brom" = tibble(),
                "West Ham" = tibble())

for(i in teams){
  matches[[i]] <- full_join(E0 %>% filter(HomeTeam == i), E0 %>% filter(AwayTeam == i)) %>% arrange(Date)
}


## Win/lose ----
winlose <- list("Arsenal" = c(), "Bournemouth" = c(), "Brighton" = c(), "Burnley" = c(), "Chelsea" = c(),
                "Crystal Palace" = c(), "Everton" = c(), "Huddersfield" = c(), "Leicester" = c(), 
                "Liverpool" = c(), "Man City" = c(), "Man United" = c(), "Newcastle" = c(), "Southampton" = c(), 
                "Stoke" = c(), "Swansea" = c(),  "Tottenham" = c(), "Watford" = c(), "West Brom" = c(),
                "West Ham" = c())

for(i in teams){
  for(j in 1:nrow(E0)){
    if(i == E0[j,3]){
      if(E0[j,5] > E0[j,6]){
        winlose[[i]] <- c(winlose[[i]], 3)
      } else if (E0[j,5] < E0[j,6]) {
        winlose[[i]] <- c(winlose[[i]], 0)
      } else{
        winlose[[i]] <- c(winlose[[i]], 1)
      }
    } else if(i == E0[j,4]){
      if(E0[j,5] > E0[j,6]){
        winlose[[i]] <- c(winlose[[i]], 0)
      } else if (E0[j,5] < E0[j,6]) {
        winlose[[i]] <- c(winlose[[i]], 3)
      } else{
        winlose[[i]] <- c(winlose[[i]], 1)
      }
    } 
  }
}

## Likelihood ----

tau_xy <- function (x, y, lambda, mu, rho) {
  if (x == 0 & y == 0) {
    return(1 - lambda * mu * rho)
  } else if (x == 0 & y == 1) {
    return(1 + lambda * rho)
  } else if (x == 1 & y == 0) {
    return(1 + mu * rho)
  } else if (x == 1 & y == 1) {
    return(1 - rho)
  } else {
    return(1)
  }
}

alpha <- rep(c(0.85, 1.15), 10)
beta <- rep(1/20, 20)
rho <- 0.01
gam <- 1.4

theta <- c(alpha, beta, rho, gam)



# football_lik_stat <- function(theta, dat){
#   N <- nrow(dat) # Number of games
#   L <- 0 # Initialization of the likelihood 
#   
#   for (i in 1:N) {
#     
#     home_ind <- which(teams == dat$HomeTeam[i]) # Index of the home team
#     away_ind <- which(teams == dat$AwayTeam[i]) # Index of the away team
#     
#     lambda <- theta[home_ind] * theta[20 + away_ind] * theta[42]
#     mu <- theta[away_ind] * theta[20 + home_ind]
#     
#     L <- L + (log(tau_xy(x = dat$FTHG[i], y = dat$FTAG[i], lambda = lambda, mu = mu, rho = theta[41])) -
#               lambda + log(lambda ^ dat$FTHG[i]) - mu + log(mu ^ dat$FTAG[i]))
#   }
#   
#   return(-L)
# }

equal <- function(theta, dat) {
  sum(theta[1:20])
}

# result_stat <- solnp(pars = theta, fun = football_lik_stat, eqfun = equal, eqB = 20, LB = rep(0, 42), UB = rep(100,42), dat = E0)
# 
# teamdf <- tibble(teams, result_stat$pars[1:20], result_stat$pars[21:40]) 
# colnames(teamdf) = c("Team", "Alpha", "Beta")


phi <- function(t, xi){
  exp(-xi * t)
}


football_lik_dyna <- function(theta, dat){
  N <- nrow(dat) # Number of games
  L <- 0 # Initialization of the likelihood 
  
  for (i in 1:N) {
    
    home_ind <- which(teams == dat$HomeTeam[i]) # Index of the home team
    away_ind <- which(teams == dat$AwayTeam[i]) # Index of the away team
    
    lambda <- theta[home_ind] * theta[20 + away_ind] * theta[42]
    mu <- theta[away_ind] * theta[20 + home_ind]
    
    L <- L + ((log(tau_xy(x = dat$FTHG[i], y = dat$FTAG[i], lambda = lambda, mu = mu, rho = theta[41])) -
                lambda + log(lambda ^ dat$FTHG[i]) - mu + log(mu ^ dat$FTAG[i])) * phi(t = floor((N-i)/10), xi = 0.0065))
  }
  
  return(-L)
}


result_dyna <- solnp(pars = theta, fun = football_lik_dyna, eqfun = equal, 
                     eqB = 20, LB = rep(0, 42), UB = rep(100,42), dat = E0[1:340,])

teamdf_dyna <- tibble(teams, result_dyna$pars[1:20], result_dyna$pars[21:40]) 
colnames(teamdf_dyna) = c("Team", "Alpha", "Beta")

# compadf <- tibble(teams, result_dyna$pars[1:20], result_stat$pars[1:20], result_dyna$pars[21:40], result_stat$pars[21:40]) 
# colnames(compadf) = c("Team", "Alpha dynamic", "Alpha static", "Beta dynamic", "Beta static")


# theta_list <- list()
# 
# for(i in 1:38){
#   result_dyna <- solnp(pars = theta, fun = football_lik_dyna, 
#                        eqfun = equal, eqB = 20, LB = rep(0, 42), 
#                        UB = rep(100,42), dat = E0[1:(10*i),])
#   theta_list[[paste("theta", i, sep = "_")]] <- result_dyna$pars
#   print(i)
# }

# for(i in 1:38){
#   param <- theta_list[[paste("theta", i, sep = "_")]]
#   teamdf_dyna <- tibble(teams, param[1:20], param[21:40]) 
#   colnames(teamdf_dyna) = c("Team", "Alpha", "Beta")
#   dyna_par[[paste("theta", i, sep = "_")]] <- teamdf_dyna
# }

## Prob function ----

football_prob <- function (x, y, alpha_home, alpha_away, beta_home, beta_away, gam, rho) {
  
  lambda <- alpha_home * beta_away * gam
  mu <- alpha_away * beta_home
  
  tau_xy <- tau_xy(x, y, lambda, mu, rho)
  
  P_xy <- tau_xy * ((lambda ^ x * exp(- lambda)) / factorial(x)) * ((mu ^ y * exp(- mu)) / factorial(y))
  
  return(P_xy)
  
}

# football_prob(1, 1, 0.7118135, 0.8869163, 0.8580254, 1.3931570, 1.326699e+00, 3.104326e-06)

played = 0
wait = 70
bankroll = 600
wagering_size = 50
odds_placed = 0
lost = 0

b = c(0.05,0.7)

for(i in 1:380){
  
  poisdf = data.frame(matrix(0, nrow = 7, ncol = 7))
  rownames(poisdf) = c("0 away", "1 away", "2 away", "3 away", "4 away","5 away","6 away")
  colnames(poisdf) = c("0 home", "1 home", "2 home", "3 home", "4 home","5 home","6 home")
  
  home_ind <- which(teams %in% E0$HomeTeam[i])
  away_ind <- which(teams %in% E0$AwayTeam[i])
  
  if(played > wait){
    
    if(i %% 5 == 0){
      result_dyna <- solnp(pars = theta, fun = football_lik_dyna, eqfun = equal, 
                           eqB = 20, LB = rep(0, 42), UB = rep(100,42), dat = E0[1:i,])
      
      teamdf_dyna <- tibble(teams, result_dyna$pars[1:20], result_dyna$pars[21:40]) 
      colnames(teamdf_dyna) = c("Team", "Alpha", "Beta")
    }
    
    alpha_home <- teamdf_dyna[home_ind, 2]
    beta_home <- teamdf_dyna[home_ind, 3]
    alpha_away <- teamdf_dyna[away_ind, 2]
    beta_away <- teamdf_dyna[away_ind, 3]
    
    gam <- result_dyna$pars[42]
    rho <- result_dyna$pars[41]
    
    for(a in 0:6){
      for(h in 0:6){
        poisdf[a+1,h+1] = football_prob(h, a, alpha_home, alpha_away,beta_home, beta_away,gam, rho)
      }
    }
    
    chance <- sum(poisdf[1,1], poisdf[1,2], poisdf[1,3], poisdf[2,1], poisdf[3,1], poisdf[2,2])
    
    bookmaker <- (1/E0$`BbMx<2.5`[i]) / (1/E0$`BbMx>2.5`[i] + 1/E0$`BbMx<2.5`[i])
    
    # cat("Match: ", E0$HomeTeam[i], " vs. ", E0$AwayTeam[i], ". My model predicts ", chance * 100, 
    #     "% for less than 2.5 goals in the game. Bookmakers put the chance at ", 
    #     bookmaker*100, "%. The real match ended:", E0$FTHG[i], "-", E0$FTAG[i], sep = "")
    # cat(sep = "\n")
    # cat("Betting?:", (chance > bookmaker + 0.04 & chance > 0.6), sep = "")
    # cat("---------------------", sep = "\n")
    
    # Backtesting
    
    if(chance > bookmaker + b[1] & chance > b[2]){
      cat("I'm betting on", E0$HomeTeam[i], " vs. ", E0$AwayTeam[i], "less than 2.5 goals at odds", E0$`BbMx<2.5`[i],"=", bookmaker * 100, "%, with my prediction at", chance * 100, "%", sep = " ")
      cat(sep = "\n")
      odds_placed = odds_placed +1
      
      if(E0$FTHG[i]+E0$FTAG[i] < 3){
        bankroll = bankroll + wagering_size * (E0$`BbMx<2.5`[i]-1)
        cat("I won", wagering_size * (E0$`BbMx<2.5`[i]-1), "and my current bankroll is", bankroll, sep = " ")
        cat(sep = "\n")
      }
      
      else{
        bankroll = bankroll - wagering_size
        lost = lost + 1
        cat("I lost and my current bankroll is", bankroll)
        cat(sep ="\n")
      }
      
    }
    
  }
  played = played + 1
}




