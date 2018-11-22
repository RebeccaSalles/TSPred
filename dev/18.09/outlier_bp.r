outliers_bp <- function(data, alpha = 1.5)
{
  org = nrow(data)
  q = as.data.frame(lapply(data, quantile))
  n = ncol(data)
  for (i in 1:n)
  {
    IQR = q[4,i] - q[2,i]
    lq1 = q[2,i] - alpha*IQR
    hq3 = q[4,i] + alpha*IQR
    cond = data[,i] >= lq1 & data[,i] <= hq3
    data = data[cond,]
  }
  final = nrow(data)
  return (data)
}