rm(list = ls()); #remove previously saved objects from current workspace
simGamblersRuin <- function(gamblerInitialCap, bankInitialCap, theta){
  #gambler's initial capital X0 = a
  a = gamblerInitialCap;
  
  #bank's initial capital B
  B = bankInitialCap;
  
  #total capital involved in the game
  K = a + B;
  
  Xn = numeric();
  Xn[1] = a;
  
  i = 2;
  while (Xn[i-1] > 0 && Xn[i-1] < K) {
    Xn[i] = Xn[i-1] + sample(c(1, -1), size = 1, prob = c(theta, 1-theta));
    i = i + 1;
  }
  
  return(Xn);
  
}

gamblersRuinResults <- function(gamblerInitialCap, bankInitialCap, theta){
  Xn = simGamblersRuin(gamblerInitialCap, bankInitialCap, theta);
  
  if(Xn[length(Xn)] == 0){
    print('Gambler is Bankrupt!');
  } else {
    print('Bank is Bankrupt!');
  }
  
  return(Xn);
}
