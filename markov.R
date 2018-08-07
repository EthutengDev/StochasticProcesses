#function for calculating matrix powers
matrixPower = source('matrixPower.R');

#markov chain n-step probability distribution given initial distribution
markov <- function(initialDistribution, probabilityMatrix, n){
  
  #n-step probability matrix
  nstepProbabilityMatrix = matrixPower(probabilityMatrix, n);
  #probability distribution given initial distribution
  probabilityDistribution = initialDistribution %*% nstepProbabilityMatrix;
  
  return(probabilityDistribution);
  
}
 

P <- matrix(c(1,0,0,0,0,0,0.06,0.03,0.91,0,
              0,0,0.06,0,0.03,0.91,0,0,0.04,0,0,0.03,0.93,0,
              0.04,0,0,0,0.03,0.93,0,0,0,0,0,1),nrow=6,byrow=T)
states <- c("Drop","Fr","So","Jr","Se","Grad")
init <- c(0,1,0,0,0,0)
