if(!require("jtools")){install.packages("jtools")}


fct_model <- function(data){
  
  lm = lm(log_exp ~ log_qt_habitantes, data = data)
  lm_summary = summary(lm)
  pp_lm_summary = jtools::summ(lm, digits = 5)
  
  df_stat = data.frame(
    cd_micro = data$cd_micro,
    nm_micro = data$nm_micro,
    area=data$area,
    cd_sh2=data$cd_sh2,
    b1_est=lm_summary$coefficients[2,1],
    b1_stde=lm_summary$coefficients[2,2],
    b1_tvalue=lm_summary$coefficients[2,3],
    b1_pvalue=lm_summary$coefficients[2,4],
    b0_est=lm_summary$coefficients[1,1],
    b0_std_e=lm_summary$coefficients[1,2],
    b0_tvalue=lm_summary$coefficients[1,3],
    b0_pvalue=lm_summary$coefficients[1,4],
    fstat=lm_summary$fstatistic[['value']],
    fstat_pvalue=pf(lm_summary$fstatistic[['value']], lm_summary$fstatistic[['numdf']], lm_summary$fstatistic[['dendf']], lower.tail = F),
    r2 = lm_summary$r.squared,
    r2_adj = lm_summary$adj.r.squared,
    bj_pvalue = tseries::jarque.bera.test(lm$residuals)$p.value,
    # sw_pvalue = shapiro.test(lm$residuals)$p.value,
    n = length(lm_summary$residuals),
    residuals = lm_summary$residuals
  )
  
  return(df_stat)
}