pairwise_tests <- function(x) {
  
  df <- clubSandwich::Wald_test(
    x,
    vcov = "CR2", 
    clubSandwich::constrain_pairwise(".*", reg_ex = TRUE),
    tidy = TRUE
  )
  
  # Remove unnecessary columns
  df <- subset(df, select = -c(test, df_num))
  
  
  # change F-stat to t-stat
  names(df)[2] <- "tstat"
  df$tstat <- sqrt(df$tstat)
  
  # Adjust p-values
  df$p_val_corrected <- p.adjust(df$p_val, method = "BH")
  
  df
}
