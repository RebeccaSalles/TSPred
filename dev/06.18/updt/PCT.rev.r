PCT.rev <- function(p,x0){
  xt <- x0*(1+p[1])
  for(i in 2:length(p)) xt <- c(xt, (1+p[i])*xt[length(xt)] )
  xt
}