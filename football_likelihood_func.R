
# Likelihood helper functions --------------------------------------------------------

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

phi <- function(t, xi){
  exp(-xi * t)
}

equal <- function(theta, df, teams, xi = 0.0065) {
  sum(theta[1:length(teams)])
}

# Dynamic likelihood function ---------------------------------------------

football_lik <- function(theta, df, teams, xi = 0.0065){
  N <- nrow(df)          # Number of games
  nt <- length(teams)    # Number of teams
  L <- 0                 # Initialization of the likelihood 
  for (i in 1:N) {
    
    home_ind <- which(teams == df$HomeTeam[i]) # Index of the home team
    away_ind <- which(teams == df$AwayTeam[i]) # Index of the away team
    
    lambda <- theta[home_ind] * theta[nt + away_ind] * theta[length(theta)]
    mu <- theta[away_ind] * theta[nt + home_ind]
    
    L <- L + ((log(tau_xy(x = df$HomeGoals[i], y = df$AwayGoals[i], lambda = lambda, mu = mu, rho = theta[length(theta) - 1])) -
                 lambda + log(lambda ^ df$HomeGoals[i]) - mu + log(mu ^ df$AwayGoals[i])) * phi(t = (df$Time[N] - df$Time[i]), xi = xi))
  }
  return(-L)
}
