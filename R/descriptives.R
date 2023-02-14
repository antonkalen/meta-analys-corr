descriptives <- function(x){
  
  m <-  x
  
  # Check class
  if (!(class(m)[1] %in% c("rma.mv", "rma"))){
    stop("x must be of class 'rma.mv'.")
  }
  
  # Check for three level model
  if (m$sigma2s != 2){
    stop("The model you provided does not seem to be a three-level model. This function can only be used for three-level models.")
  }
  
  # Check for right specification (nested model)
  if (sum(grepl("/", as.character(m$random[[1]]))) < 1){
    stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
  }
  
  # Calculate I2
  ## Get variance diagonal and calculate total variance
  n = m$k.eff
  vector.inv.var = 1/(diag(m$V))
  sum.inv.var = sum(vector.inv.var)
  sum.sq.inv.var = (sum.inv.var)^2
  vector.inv.var.sq = 1/(diag(m$V)^2)
  sum.inv.var.sq = sum(vector.inv.var.sq)
  num = (n-1)*sum.inv.var
  den = sum.sq.inv.var - sum.inv.var.sq
  est.samp.var = num/den
  
  ## Calculate variance proportions
  i2_lvl2=((m$sigma2[2])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  i2_lvl3=((m$sigma2[1])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  
  
  # Data frame to return
  descrip <- data.frame(
    k_studies = m$s.nlevels[[1]],
    n_es = m$s.nlevels[[2]],
    tau2_lvl2 = m$sigma2[[1]],
    tau2_lvl3 = m$sigma2[[2]],
    i2_tot = i2_lvl2 + i2_lvl3,
    i2_lvl2 = i2_lvl2,
    i2_lvl3 = i2_lvl3
  )
  
  descrip
}
