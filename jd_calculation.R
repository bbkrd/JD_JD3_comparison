# Version 0.4 (2021-03-15)


library(RJDemetra)
suppressPackageStartupMessages(library(RJDemetra3))

# Load & check data ###################################
if (exists("workspace")) {
  wk <- load_workspace(workspace)
  ts <- unlist(get_ts(wk), recursive = FALSE)
} else if (exists("data_file")) {
  ts <- load(data_file)
  ts <- get(ts)
  ts_format <- unique(sapply(ts, class))
  if (length(ts_format) < 1 | ts_format != "ts") {
    stop("Please provide your data in ts format.")
  }
} else {
  stop("Please provide the link either to your workspace in the object \"workspace\" or
       to a rda file with your data in ts format in the object \"data_file\".")
}
  # Periodicity
  frequ        <- unique(sapply(ts, frequency))
  if(any(frequ != 12 & frequ != 4)) stop("Only monthly and quarterly series are currently supported by JD3.")

  # Zeros
  if (exists("ts_warn")) rm(ts_warn)
  if(any(sapply(ts, function(z) any(z == 0)))) ts_warn <- "Some series contain zeros."
  # Missing values
  if(any(sapply(ts, function(z) any(is.na(z))))) {
    if(exists("ts_warn")) {
      ts_warn <- paste(ts_warn, "Some series contain missing values.")
    } else ts_warn <- "Some series contain missing values."}
  # Negative values
  if(any(sapply(ts, function(z) any(z < 0)))) {
    if(exists("ts_warn")) {
      ts_warn <- paste(ts_warn, "Some series contain negative values.")
    } else ts_warn <- "Some series contain negative values."}
  if(exists("ts_warn")) {
    ts_warn <- paste(ts_warn, "Calculation of growth rates might be affected misleadingly.")
    warning(ts_warn)
    }


# Test of specification ############################
if (METHOD == "X13") {
    valid_spec <- SPEC %in% c("X11", "RSA0", "RSA1", "RSA2", "RSA2c", "RSA3", "RSA4", "RSA4c", "RSA5", "RSA5c")
} else if (METHOD == "TS"){
    valid_spec <- SPEC %in% c("RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5", "RSAfull")
} else {
    stop("Please specify for METHOD either \"X13\" or \"TS\".")
}
if (!valid_spec) {
    stop("Please specify in SPEC either
             {X11, RSA0, RSA1, RSA2, RSA3, RSA4, RSA5}          for METHOD \"X13\"
          or      {RSA0, RSA1, RSA2, RSA3, RSA4, RSA5, RSAfull} for METHOD \"TS\".")
}


# Names of series ############################
if(length(names(ts))!=length(ts)) names(ts) <- paste("Series", c(1:length(ts)))


# Seasonal adjustment ############################
if (METHOD == "X13") {
    jd2 <- lapply(ts, function(z){RJDemetra::x13(z,SPEC)})                                                       # JD+2
    jd3 <- lapply(ts, function(z){RJDemetra3::x13(z,SPEC)})                                                      # JD+3
} else {
    jd2 <- lapply(ts, function(z){RJDemetra::tramoseats(z,SPEC)})                                                # JD+2
    jd3 <- lapply(ts, function(z){RJDemetra3::tramoseats(z,SPEC)})                                               # JD+3
}

# seasonally adjusted series
sa_jd2 <- lapply(jd2, function(z){z$final$series[,2]})                                                            # JD+2
if (METHOD == "X13") {                                                                                            # JD+3
  sa_jd3 <- lapply(jd3, function(z){z$final$d11final})
} else {
  sa_jd3 <- lapply(jd3, function(z){z$final$sa$data})
}

# monthly growth rates
jd2_gr <- lapply(sa_jd2, function(z){(z/lag(z, -1)-1)*100})                                                           # JD+2
jd3_gr <- lapply(sa_jd3, function(z){(z/lag(z, -1)-1)*100})                                                           # JD+3


# extract further information ############################
    # series names
    jd_names <- names(jd2)

    # transformation
    jd2_log <- sapply(jd2, function(z){ifelse(isTRUE(z$regarima$model$spec_rslt[3][[1]]), "Log", "None")})        # JD+2
    jd3_log <- sapply(jd3, function(z){ifelse(isTRUE(z$preprocessing$log), "Log", "None")})                       # JD+3

    # mean
    jd2_mean <- sapply(jd2, function(z){ifelse(isTRUE(z$regarima$model$spec_rslt[4][[1]]), "const", "")})         # JD+2
    jd3_mean <- sapply(jd3, function(z){ifelse(rownames(z$preprocessing$variables[1,]) == "const", "const", "")}) # JD+3

    # ARIMA model
    jd2_model <- sapply(jd2, function(z){paste("(",
                                               z$regarima$arma["p"],
                                               z$regarima$arma["d"],
                                               z$regarima$arma["q"],
                                               ") (",
                                               z$regarima$arma["bp"],
                                               z$regarima$arma["bd"],
                                               z$regarima$arma["bq"],
                                               ")", sep = "")})                                                   # JD+2
    jd3_model <- sapply(jd3, function(z){paste("(",
                                               z$preprocessing$sarima$p,
                                               z$preprocessing$sarima$d,
                                               z$preprocessing$sarima$q,
                                               ") (",
                                               z$preprocessing$sarima$bp,
                                               z$preprocessing$sarima$bd,
                                               z$preprocessing$sarima$bq,
                                               ")", sep = "")})                                                   # JD+3

    # Number of outliers
    jd2_outl <- sapply(jd2, function(z){z$regarima$model$spec_rslt$Outliers})                                     # JD+2
    jd3_outl <- sapply(jd3, function(z){(sum(z$preprocessing$variables$type == "AO")
                                         + sum(z$preprocessing$variables$type == "LS")
                                         + sum(z$preprocessing$variables$type == "TC"))})                         # JD+3

    # Trading days
    jd2_td <- sapply(jd2, function(z){ifelse(z$regarima$model$spec_rslt$`Trading days` > 0, "TD", "")})           # JD+2
    jd3_td <- sapply(jd3, function(z){ifelse(any(z$preprocessing$variables$type == "TD"), "TD", "")})             # JD+3

    # Easter
    jd2_east <- sapply(jd2, function(z){ifelse(isTRUE(z$regarima$model$spec_rslt$Easter), "Easter", "")})         # JD+2
    jd3_east <- sapply(jd3, function(z){ifelse(any(z$preprocessing$variables$type == "EASTER"), "Easter", "")})   # JD+3

    # leap year
    jd2_leap <- sapply(jd2, function(z){ifelse(isTRUE(z$regarima$model$spec_rslt$`Leap year`), "LP", "")})        # JD+2
    jd3_leap <- sapply(jd3, function(z){ifelse(any(z$preprocessing$variables$type == "LP"), "LP", "")})           # JD+3

    # seasonal filter
    if(METHOD=="X13") {
      jd2_sf <- sapply(jd2, function(z){z$decomposition$s_filter})                                                # JD+2
      jd3_sf <- rep(NA, length(jd2_sf)); names(jd3_sf) <- names(jd2_sf)                                           # not reported in JD+3
    } else {
      jd3_sf <- jd2_sf <- rep(NA, length(jd_names)); names(jd2_sf) <- names(jd_names)                             # not specified for TS
    }

    # trend filter
    if(METHOD=="X13") {
      jd2_tf <- as.numeric(sapply(jd2, function(z){gsub("-","",substr(z$decomposition$t_filter, 1, 2))}))         # JD+2
      jd3_tf <- sapply(jd3, function(z){z$decomposition$final_henderson})                                         # JD+3
    } else {
      jd3_tf <- jd2_tf <- rep(NA, length(jd_names)); names(jd2_sf) <- names(jd_names)                             # not specified for TS
    }

    # log-likelihood
    jd2_ll <- sapply(jd2, function(z){z$regarima$loglik[1]})                                                      # JD+2
    jd3_ll <- sapply(jd3, function(z){z$preprocessing$likelihood$ll})                                             # JD+3

    # BIC
    jd2_bicc <- sapply(jd2, function(z){z$regarima$loglik[7]})                                                    # JD+2
    jd3_bicc <- sapply(jd3, function(z){z$preprocessing$likelihood$bicc})                                         # JD+3


# calculations for Table 1 ############################
mdgr   <- mapply(function(z1, z2){mean(z1-z2,                    trim=TRIM,na.rm=T)}, jd2_gr,   jd3_gr)   # average difference in growth rates
madgr  <- mapply(function(z1, z2){mean(abs(z1-z2),               trim=TRIM,na.rm=T)}, jd2_gr,   jd3_gr)   # average absolute difference in growth rates
cohgr  <- mapply(function(z1, z2){mean(ifelse(z1*z2<=0,0,1)*100, trim=TRIM,na.rm=T)}, jd2_gr,   jd3_gr)   # coherence rate of growth rates
mpd    <- mapply(function(z1, z2){mean((z1-z2)/z2,               trim=TRIM,na.rm=T)}, sa_jd2,   sa_jd3)   # mean percentage difference
mapd   <- mapply(function(z1, z2){mean(abs((z1-z2)/z2),          trim=TRIM,na.rm=T)}, sa_jd2,   sa_jd3)   # mean absolute percentage difference

ll_dif    <- mapply(function(z1, z2){(z1-z2)*100/z2}, jd2_ll, jd3_ll)      # Percentage difference of log-likelihood
trans_sh  <- sum(jd3_log   == jd2_log)   *100/length(jd3_log)              # Share of same transformations
model_sh  <- sum(jd3_model == jd2_model) *100/length(jd3_model)            # Share of same ARIMA models
td_sh     <- sum(jd3_td    == jd2_td)    *100/length(jd3_td)               # Share of same trading day regressors included
lp_sh     <- sum(jd3_leap  == jd2_leap)  *100/length(jd3_leap)             # Share of same leap year regressors included
east_sh   <- sum(jd3_east  == jd2_east)  *100/length(jd3_east)             # Share of same easter regressors included
out_dif   <- mapply(function(z1, z2){(z1-z2)}, jd2_outl, jd3_outl)         # Difference of outliers identified
tf_sh     <- sum(jd3_tf    == jd2_tf)    *100/length(jd3_tf)               # Share of same trend filter used


# Table 1
    # - This table with summaries of the quality measures will be reported
tab1 <- data.frame(c("Average difference between growth rates (%)",
                     "Average absolute difference between growth rates (%)",
                     "Average coherence between growth rate signs (%)",
                     "Average percentage differences in SA series (%)",
                     "Average absolute percentage differences in SA series (%)",
                     "Average difference between log-likelihoods (%)",
                     "Share of same transformations (%)",
                     "Share of same ARIMA models (%)",
                     "Share of same trading day regressors included (%)",
                     "Share of same leap year regressor included (%)",
                     "Share of same easter regressor included (%)",
                     "Average difference in number of outliers identified"),
                   t(rbind(round(colMeans(cbind(mdgr, madgr, cohgr, mpd, mapd,
                                                ll_dif, trans_sh, model_sh, td_sh, lp_sh,
                                                east_sh, out_dif)), DIGI))))
colnames(tab1) <- c("Indicator", "")


# Table 2
    # - This table shows a time series specific comparison of selected parameters.
    # It will not be reported and is forseen for internal analysis of the detected differences.
tab2 <- data.frame(jd_names, jd2_log, jd2_mean, jd2_model, jd2_sf, jd2_tf,
                   jd3_log, jd3_mean, jd3_model, jd3_sf, jd3_tf,
                   row.names = NULL)
colnames(tab2) <- c("Series", rep(c("Log", "Mean", "Model", "SF", "TF"),2))

# Table 3
    # - This table shows a time series specific comparison of the seasonally adjusted series.
    # It will not be reported and is forseen for internal analysis of the detected differences.
tab3 <- data.frame(jd_names, round(cbind(mdgr, madgr, cohgr, mpd, mapd), DIGI),
                   row.names = NULL)
colnames(tab3) <- c("Series", "MDGR","MADGR","COHGR","MPD","MAPD")




