mlm_io <- function(sw) 
{
  input <- sw[,1:ncol(sw)-1]
  output <- sw[,ncol(sw)]
  return (list(input=input, output=output))
}