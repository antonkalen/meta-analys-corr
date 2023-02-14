get_coefs <- function(model, data, study_id = "id"){
  
  k <- length(unique(data[[study_id]]))
  
  if (k > 1) {
    model <- metafor::robust(model, cluster = data[[study_id]], clubSandwich = TRUE)
  }
  
  df <- broom::tidy(model, conf.int = TRUE)
  df$estimate <- metafor::transf.ztor(df$estimate)
  df$conf.low <- metafor::transf.ztor(df$conf.low)
  df$conf.high <- metafor::transf.ztor(df$conf.high)
  
  
  cbind(df, df = model$dfs)
  
}

get_coefs_2 <- function(model) {
  
  df <- clubSandwich::conf_int(model, vcov = "CR2", p_values = TRUE)
  df$beta <- metafor::transf.ztor(df$beta)
  df$CI_L <- metafor::transf.ztor(df$CI_L)
  df$CI_U <- metafor::transf.ztor(df$CI_U)
  
  df
  
}