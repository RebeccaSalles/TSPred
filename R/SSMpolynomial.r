
#' @keywords internal
SSMpolynomial <- 
function(y, ord, H=0, Q=NA){ #Fun??o feita com base no c?digo do dlmodeler
  if( ord<0 ) stop("Order must be >= 0")
  m <- ord+1
  if( length(Q)!=1 & length(Q)!=m ) stop("SigmaQ has wrong dimension: should be of size ",m)
  d <- 1
  
  a0 <- matrix(0,m,1)
  P0 <- diag(0,m,m)
  P0inf <- diag(m)
  
  Tt <- diag(1,m,m)
  if( m>1 ) for( i in 1:(m-1) ) Tt[i,i+1] <- 1
  Rt <- diag(1,m,m)
  Qt <- diag(Q,m) #diag(sigmaQ^2,m)
  
  Zt <- matrix(c(1,rep(0,m-1)),d,m)
  Ht <- H #matrix(sigmaH^2,d,d)
  
  SSM <- SSModel(y ~ -1 + SSMcustom(
      Z=Zt, # observation
      T=Tt, # transition
      R=Rt, # state disturbance selection
      Q=Qt, # state disturbance covariance
      a1=a0, # initial state
      P1=P0, # initial state covariance
      P1inf=P0inf, # diffuse part of P1
      n=1
    ),
    H=Ht # observation disturbance
  )
  
  return(SSM)
}