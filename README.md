# Football Betting Model
This is my implementation of a football betting algorithm. The algorithm is not done and is currently under work (whenever I get time/motivation), however, the parameter estimates via the likelihood function is completed. 

The main sources for the development of this implementation has been:
- Dixon, Mark J., and Stuart G. Coles. "Modelling association football scores and inefficiencies in the football betting market." Journal of the Royal Statistical Society: Series C (Applied Statistics) 46.2 (1997): 265-280.
- How to Create a Football Betting Model - http://www.bestbettingonline.com/strategy/create-model/.
- Poisson Distribution: Predict the score in soccer betting - https://www.pinnacle.com/en/betting-articles/Soccer/how-to-calculate-poisson-distribution/MD62MLXUMKMXZ6A8.

Data downloaded from https://www.football-data.co.uk/

### Introduction to the Scripts
- [*Create_data.R*](https://github.com/MoAnd/FootballBetting/blob/master/Create_data.R) is for collecting and assembling the match and betting data.
- [*football_likelihood_func.R*](https://github.com/MoAnd/FootballBetting/blob/master/football_likelihood_func.R) & [*football_prob_func.R*](https://github.com/MoAnd/FootballBetting/blob/master/football_prob_func.R) are just functions to be source (the likelihood, helpers, and the score probability functions).
- [*Optimization.R*](https://github.com/MoAnd/FootballBetting/blob/master/Optimization.R) is for running the optimization of the likelihood function, i.e. finding the team parameters. 
- [*Xi_testing.R*](https://github.com/MoAnd/FootballBetting/blob/master/Xi_testing.R) is a (very slow) script to test different values of the xi parameter.
- [*Betting_backtesting.R*](https://github.com/MoAnd/FootballBetting/blob/master/Betting_backtesting.R) is the actual backtesting on the betting strategy using the parameters found in the maximazation of the likelihood.
