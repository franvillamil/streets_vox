# Quiet function
quiet = function(x){
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# Function to get clustered SEs
cluster_se = function(orig_model, clustervar){
  orig_data = eval(orig_model$call$data, envir=.GlobalEnv)
  if("glm" %in% class(orig_model)){
    m_cse = glm.cluster(data = orig_data,
      formula = formula(orig_model),
      cluster = clustervar, family = "binomial")
  } else {
    m_cse = lm.cluster(data = orig_data,
      formula = formula(orig_model),
      cluster = clustervar)
  }
  if(!all(coefficients(orig_model) == coef(m_cse))){
    stop(paste0("Different coefficients?! for model: ",
      as.character(formula(orig_model)[3])))}
  return(quiet(summary(m_cse)[, "Std. Error"]))
}

# weird issue with lm.cluster
# (https://github.com/alexanderrobitzsch/miceadds/issues/18)
wgt__ = NULL

# Heteroskedasticity robust SE
robust_se = function(m){
  s = coeftest(m, vcov = vcovHC(m, type = "HC0"))
  return(s[, "Std. Error"])
}
