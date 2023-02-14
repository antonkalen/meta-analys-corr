quality_factor_comp <- function(x) {
  
  coef_names <- names(coef(x))
  coef_names <- sub(":.*", ":", coef_names)
  coef_names <- coef_names[duplicated(coef_names)]
  coef_names <- unique(coef_names)
  
  results <- coef_names |> 
    purrr::map(
      purrr::possibly(
      \(coef_name) {
        clubSandwich::Wald_test(
          x,
          vcov = "CR2", 
          clubSandwich::constrain_equal(coef_name, reg_ex = TRUE),
          tidy = TRUE
        )
      }
      )
    )
  
  names(results) <- coef_names
  
  df <- purrr::list_rbind(results, names_to = "name")
  
  # Adjust p-values
  df$p_val_corrected <- p.adjust(df$p_val, method = "BH")
  
  df
}