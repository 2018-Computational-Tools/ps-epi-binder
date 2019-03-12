geom_lm <- function(lm_fit, level = 0.68)  {
  # lm_fit is a model from lm(). 
  # level is a confidence level for predict(). 
  # Use level = 0.68 for 1 sigma, level = 0.95 for 2 sigma, etc. 
  
  # Calculate fitted values, confidence intervals, and prediction intervals. 
  predict <- suppressWarnings(
      tibble(
          x = unlist(lm_fit$model[2]), 
          y = unlist(lm_fit$model[1]),
          fit = predict(lm_fit),
          lwr_conf = predict(lm_fit, interval = "confidence", level = level)[, 2],
          upr_conf = predict(lm_fit, interval = "confidence", level = level)[, 3],
          lwr_pred = predict(lm_fit, interval = "prediction", level = level)[, 2],
          upr_pred = predict(lm_fit, interval = "prediction", level = level)[, 3]
      ))
  
  list(
    # Lower confidence interval
    geom_line(mapping = aes(x = x, y = lwr_conf), 
              data = predict, 
              color = "blue",
              linetype = 2),
    
    # Upper confidence interval
    geom_line(mapping = aes(x = x, y = upr_conf), 
              data = predict, 
              color = "blue",
              linetype = 2),
    
    # Lower prediction interval
    geom_line(mapping = aes(x = x, y = lwr_pred), 
              data = predict, 
              color = "red",
              linetype = 2),
    
    # Upper prediction interval
    geom_line(mapping = aes(x = x, y = upr_pred), 
              data = predict, 
              color = "red",
              linetype = 2),
    
    # Linear fit
    geom_abline(slope = coef(lm_fit)[2], 
                intercept = coef(lm_fit)[1], 
                color = "blue", 
                size = 1), 
    
    labs(
      title = paste("Y =", 
                    signif(lm_fit$coef[2], 5), 
                    "* X +", 
                    signif(lm_fit$coef[1], 5)),
      subtitle = paste("Adjusted R2 = ", 
                       signif(summary(lm_fit)$adj.r.squared, 5))) 
    
  )
}