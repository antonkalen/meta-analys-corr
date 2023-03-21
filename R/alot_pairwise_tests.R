alot_pairwise_tests <- function(x) {
  
  coef_names <- names(coef(x))
  coef_names <- sub(":.*", ":", coef_names)
  coef_names <- coef_names[duplicated(coef_names)]
  coef_names <- unique(coef_names)

  
  df <- coef_names |> 
    purrr::map(
      purrr::possibly(
      \(coef_name) {
        clubSandwich::Wald_test(
          x,
          vcov = "CR2", 
          clubSandwich::constrain_pairwise(coef_name, reg_ex = TRUE),
          tidy = TRUE
        )
      }
      )
    ) |> 
    purrr::list_rbind()
  

  
  # Remove unnecessary columns
  df <- subset(df, select = -c(test, df_num))
  
  # change F-stat to t-stat
  names(df)[2] <- "tstat"
  df$tstat <- sqrt(df$tstat)
  
  # Adjust p-values
  df$p_val_corrected <- p.adjust(df$p_val, method = "BH")
  
  df
}