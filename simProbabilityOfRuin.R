rm(list = ls()); #remove previously saved objects from current workspace
source('gamblersRuin.R')
simProbabilityOfRuin <- function(gamblerInitialCap, bankInitialCap, theta, n){
  numberOfRuins = 0; #number of games the gambler is ruined/bankrupt
  Xn = numeric(); #initialise vector to store simulated path
  
  for(i in 1:n){
    #simulate gambler's ruin
    Xn = simGamblersRuin(gamblerInitialCap, bankInitialCap, theta);
    
    if(Xn[length(Xn)] == 0){ #if gambler is bankrupt add 1 to numberOfRuins
      numberOfRuins = numberOfRuins + 1;
    }
    
  }
  
  #return the probability of ruin
  return(numberOfRuins/n);
  
}

