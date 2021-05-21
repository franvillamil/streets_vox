## Replication "Do TJ policies cause backlash? Evidence from street name changes in Spain"
## January 2021
## File: SUPPLEMENTARY FUNCTIONS (for 'analyses.R')
## ---------------------------------------

did_sim = function(m, fs_chg_var, other_vars, FE = NULL){

  print("(Remember: Dependent variable must be binary 0/1)")

  # Get models coefficients
  coefs = names(m$coefficients)

  # Get election values
  if (!"factor(election)" %in% names(m$xlevels)){
    stop("election interaction must be >factor(election)<")
  } else {
    elec = m$xlevels[["factor(election)"]]
  }

  # Create matrix
  mat = expand.grid(fs_chg = 0:1, elec = elec)

  # Expand for interaction (election) terms
  mat = cbind(mat,
    matrix(rep(0, (length(elec)) * nrow(mat)),
      ncol = length(elec),
      dimnames = list(NULL, elec)) )
  for(i in elec){mat[ mat$elec == i ,i] = 1}

  # Assigning proper names
  names(mat)[names(mat) == "fs_chg"] = fs_chg_var
  names(mat)[names(mat) %in% elec] = paste0("factor(election)",
      names(mat)[names(mat) %in% elec])

  # Rest of covariates
  mat = cbind(mat,
    other_vars[rep(1, nrow(mat)), ])

  # Interactions (elections)
  coef_int = coefs[grepl(":", coefs)]
  for(i in coef_int){
    tmp = apply(mat[,unlist(str_split(i, ":"))], 1, prod)
    mat = cbind(mat, tmp)
    names(mat)[names(mat) == "tmp"] = i
  }

  # Intercept
  mat$Intercept = 1
  names(mat)[names(mat) == "Intercept"] = "(Intercept)"

  # If FE, assign reference category
  if(!is.null(FE)){
    coef_FE = coefs[grepl(paste0("factor\\(", FE, "\\)"), coefs)]
    matFE = matrix(0, ncol = length(coef_FE), nrow = nrow(mat),
      dimnames = list(NULL, coef_FE))
    mat = cbind(mat, matFE)
  }

  # Checking if any variable is missing
  if(!all(coefs %in% names(mat))){stop(paste0("Missing variables!: ",
      paste(coefs[!coefs %in% names(mat)], collapse = " ")))
  } else {
    row_index = paste("fs_chg", mat[, fs_chg_var],
      "elec", mat[, "elec"], sep = "_")
    mat = mat[, match(coefs, names(mat))]
  }

  # Simulation
  coefs = m$coefficients
  covmat = vcov(m)
  # Random draws of coefficients
  betadraw = mvrnorm(n = 1000, mu = coefs, Sigma = covmat)

  # Create list of point estimates
  estimates = vector("list", length(row_index))
  names(estimates) = row_index

  for(i in 1:length(row_index)){
    name = row_index[i]
    estimates[[name]] = betadraw %*% as.numeric(mat[i,])
  }

  # OUTPUT
  return(estimates)

}

calculate_did_estimate = function(
  depvar_label, simulations, elec_t0, elec_t1, return = "summary"){

  # Check 1
  if(!all(c(is.character(elec_t0), is.character(elec_t1)))){
    stop("elec_t0 and elec_t1 must be character vectors")
  }

  # Check 2
  elec_dates = unique(gsub("fs_chg_(0|1)_elec_", "", names(simulations)))
  if(!elec_t0 %in% elec_dates){stop("t0 election not in simulations?")}
  if(!elec_t1 %in% elec_dates){stop("t1 election not in simulations?")}

  effect_t0 = simulations[[paste0("fs_chg_1", "_elec_", elec_t0)]] -
    simulations[[paste0("fs_chg_0", "_elec_", elec_t0)]]

  effect_t1 = simulations[[paste0("fs_chg_1", "_elec_", elec_t1)]] -
    simulations[[paste0("fs_chg_0", "_elec_", elec_t1)]]

  did_estimate = effect_t1 - effect_t0

  did_estimate = data.frame(did_est_sim = effect_t1 - effect_t0)
  did_estimate$depvar = depvar_label
  did_estimate$time = paste(elec_t0, elec_t1, sep = "_to_")


  if(return == "all"){
    return(did_estimate)
  } else if(return == "summary"){
    did_estimate = did_estimate %>%
      group_by(depvar, time) %>%
      summarize(
        mean = mean(did_est_sim),
        upr = as.numeric(quantile(did_est_sim, 0.975)),
        lwr = as.numeric(quantile(did_est_sim, 0.025)),
        upr90 = as.numeric(quantile(did_est_sim, 0.95)),
        lwr90 = as.numeric(quantile(did_est_sim, 0.05))
      ) %>% as.data.frame()
    return(did_estimate)
  }

}
