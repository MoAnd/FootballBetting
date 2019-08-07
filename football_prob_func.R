
# Tau function ------------------------------------------------------------

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

# Football probability function -------------------------------------------

football_prob <- function (x, y, alpha_home, alpha_away, beta_home, beta_away, gam, rho) {
  
  lambda <- alpha_home * beta_away * gam
  mu <- alpha_away * beta_home
  
  tau_xy <- tau_xy(x, y, lambda, mu, rho)
  
  P_xy <- tau_xy * ((lambda ^ x * exp(- lambda)) / factorial(x)) * ((mu ^ y * exp(- mu)) / factorial(y))
  
  return(P_xy)
  
}
