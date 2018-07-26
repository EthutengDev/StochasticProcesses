matrixPower <- function(mat, n){
  matPow = mat; #initialize matrix
  if(n == 0){
    
    #return matrix to the power 0
    return(1);
    
  }else if(n == 1){
    
    #return matrix to the power of 1 (i.e. initial matrix)
    return(matPow);
    
  } else {
    for (i in 1:(n-1)) {
      
      #calculate matrix powers
      matPow = matPow %*% mat;
      
    }
    #return mat to the power n
    return(matPow);
    
  }
}
