#***************************** function_helpers *******************************#
#* STEP 2
#* 
#* Description: 
#* This Script generates the helper functions used in further scripts, mainly
#* for visualizations. 
#*
#* Usage: 
#* sourced in further scripts
#* 
#******************************************************************************#
#*
#***************************** helper functions *******************************#

# Make a single comparison plot
single_plot <- function(df, number) {
  p <- ggplot(df, aes(x = intmid, y = fit)) +
    geom_hline(yintercept = 1, col = "red") +
    geom_step(aes(x = intmid, y = fit)) +
    geom_step(aes(x = intmid, y = lo), linetype = "dotted") +
    geom_step(aes(x = intmid, y = hi), linetype = "dotted") +
    ylab("Hazard Ratio") + xlab("Tage nach ICU Aufnahme") +
    ggtitle(paste(number)) +
    scale_y_continuous(limits = c(0.15, 4), trans = "log1p",
                       breaks = c(0.15, 0.5, 1, 1.5, 2, 4))
  p
}

# adjusted single_plot for PowerPoint
single_plot_powerpoint <- function(df, number) {
  p <- ggplot(df, aes(x = intmid, y = fit)) +
    geom_hline(yintercept = 1, col = "red") +
    geom_step(aes(x = intmid, y = fit)) +
    geom_step(aes(x = intmid, y = lo), linetype = "dotted") +
    geom_step(aes(x = intmid, y = hi), linetype = "dotted") +
    ylab("Hazard Ratio") + xlab("Tage nach ICU Aufnahme") +
    ggtitle(paste("early standard vs. early high")) +
    scale_y_continuous(limits = c(0.15, 4), trans = "log1p",
                       breaks = c(0.15, 0.5, 1, 1.5, 2, 4)) +
    scale_x_continuous(limits = c(0, 60), 
                       breaks = c(20, 40, 60)) +
    themes_plot4
  p
}

# Make six frames
make_six_frames <- function(
  m, patient, daily, ll_fun, type,
  var1 = "calCat2", var2 = "calCat3",
  effect = "calcat", surv_max = 60, min_surv = 5) {
  only_high_ped <- prepare_predict_frame(
    patient, daily, ll_fun,
    scheme = list(C2 = 0, C3 = 1),
    surv_max = surv_max, type, var1, var2)
  
  only_med_ped <- prepare_predict_frame(
    patient, daily, ll_fun,
    scheme = list(C2 = 1, C3 = 0),
    surv_max = surv_max, type, var1, var2)
  
  only_low_ped <- prepare_predict_frame(
    patient, daily, ll_fun,
    scheme = list(C2 = 0, C3 = 0),
    surv_max = surv_max, type, var1, var2)
  
  low_to_med_ped <- prepare_predict_frame(
    patient, daily, ll_fun,
    scheme = list(C2 = c(rep(0, 4), rep(1, 7)), C3 = 0),
    surv_max = surv_max, type, var1, var2)
  
  med_to_high_ped <- prepare_predict_frame(
    patient, daily, ll_fun,
    scheme = list(C2 = c(rep(1, 4), rep(0, 7)), C3 = c(rep(0, 4), rep(1, 7))),
    surv_max = surv_max, type, var1, var2)
  
  comp1 <- as.data.frame(
    getEffectDiffs(m, only_low_ped, low_to_med_ped, effectname = effect))
  comp2 <- as.data.frame(
    getEffectDiffs(m, low_to_med_ped, only_med_ped, effectname = effect))
  comp3 <- as.data.frame(
    getEffectDiffs(m, only_low_ped, only_med_ped, effectname = effect))
  comp4 <-
    as.data.frame(
      getEffectDiffs(m, only_med_ped, med_to_high_ped, effectname = effect))
  comp5 <- as.data.frame(
    getEffectDiffs(m, med_to_high_ped, only_high_ped, effectname = effect))
  comp6 <- as.data.frame(
    getEffectDiffs(m, only_med_ped, only_high_ped, effectname = effect))
  res <- list(comp1, comp2, comp3, comp4, comp5, comp6)
  attr(res, "exampledf") <- only_high_ped
  res
}

# Prepare data frame for prediction
#
# Arguments:
# p: the patient data set (one row per patient)
# d: the daily data set (one row per day in ICU)
# scheme: the nutrition protocol
# surv_max: the maximally considered follow up
prepare_predict_frame <- function(
  p,
  d,
  ll_fun,
  scheme = list(C2 = NULL, C3 = NULL),
  surv_max = 60,
  type,
  var1,
  var2) {
  p <- p[1, ]
  p$CombinedID = 1
  p$PatientDied <- 1
  p$PatientDischarged <- 1
  p$event <- surv_max
  d <- d[1:11, ]
  d$CombinedID <- 1
  d$Study_Day <- 1:11
  d$OralIntake <- 0
  if (var1 %in% c("calCat2", "calCat3")) {
    d$calCat2 <- scheme[[1]]
    d$calCat3 <- scheme[[2]]
  } else {
    d$proteinCat2 <- scheme[[1]]
    d$proteinCat3 <- scheme[[2]]
  }
  ll <- get_laglead(0:surv_max,
                    tz = 1:11,
                    ll_fun = ll_fun)
  if (type == "1") {
    final_ped <- as_ped(
      data    = p,
      formula = Surv(event, PatientDied) ~ Year + DiagID2 + AdmCatID + Gender +
        ApacheIIScore + BMI + Propofol2_4 + inMV2_4 + OralIntake2_4 + PN2_4 + Age +
        CombinedicuID + icuByDummy,
      cut     = 0:surv_max, id = "CombinedID") %>%
      add_cumulative_eff_vec(d, var1, var2, LL = ll)
  } else {
    final_ped <- as_ped(data    = p,
                        formula = Surv(event, PatientDischarged) ~ Year + DiagID2 + AdmCatID + Gender +
                          ApacheIIScore + BMI + Propofol2_4 + inMV2_4 + OralIntake2_4 + PN2_4 + Age +
                          CombinedicuID + icuByDummy,
                        cut     = 0:surv_max, id = "CombinedID") %>%
      add_cumulative_eff_vec(daily_set = d, var1, var2, LL = ll)
  }
  final_ped$icuByDummy <- 0
  final_ped$int_mid <- 0.5 * (final_ped$tstart + final_ped$tend)
  final_ped$CombinedicuID <- 11325L
  final_ped <- final_ped[final_ped$tend > 4, ]
  final_ped
}

# Improvised function adding cumulative effects to the PED.
add_cumulative_eff_vec <- function(
  ped,
  daily_set,
  varname1,
  varname2,
  LL) {
  
  ped           <- ped[ped$CombinedID %in% daily_set$CombinedID, ]
  daily_set     <- daily_set[daily_set$CombinedID %in% ped$CombinedID, ]
  grouped_ped   <- ped %>% group_by(CombinedID)
  nested_ped    <- grouped_ped %>% tidyr::nest()
  grouped_daily <- daily_set %>% group_by(CombinedID)
  nested_daily  <- grouped_daily %>% tidyr::nest()
  t_matrix      <- matrix(rep(ped$tend), ncol = 11, nrow = nrow(ped)) - 0.5
  tz_matrix     <- matrix(1:11, ncol = 11, nrow = nrow(ped), byrow = TRUE) + 1
  var1_matrix   <- matrix(1, nrow = nrow(ped), ncol = 11)
  var2_matrix   <- matrix(1, nrow = nrow(ped), ncol = 11)
  LL_matrix     <- matrix(0, nrow = nrow(ped), ncol = 11)
  max_LL_matrix <- matrix(c(LL$LL), nrow = length(unique(LL$t)), byrow = TRUE)
  max_LL_matrix <- max_LL_matrix[2:(max(ped$tend) + 1), ]
  ids <- unique(ped$CombinedID)
  for (i in 1:length(ids)) {
    number <- which(attr(grouped_ped, "groups")$CombinedID == ids[i])
    cur_rows <- attr(grouped_ped, "groups")$.rows[[number]]
    matching <- which(nested_daily$CombinedID == ids[i])
    cur_data <- nested_daily$data[[matching]]
    var1_vec <- rep(NA, 11)
    var2_vec <- rep(NA, 11)
    var1_vec[1:nrow(cur_data)] <- dplyr::pull(cur_data, varname1)
    var2_vec[1:nrow(cur_data)] <- dplyr::pull(cur_data, varname2)
    var1_matrix[cur_rows, ] <- matrix(rep(var1_vec, length(cur_rows)),
                                      nrow = length(cur_rows), byrow = TRUE)
    var2_matrix[cur_rows, ] <- matrix(rep(var2_vec, length(cur_rows)),
                                      nrow = length(cur_rows), byrow = TRUE)
    LL_matrix[cur_rows, ] <- max_LL_matrix[1:length(cur_rows), ]
  }
  t_matrix[is.na(var1_matrix)] <- 0
  tz_matrix[is.na(var1_matrix)] <- 0
  var1_matrix[is.na(var1_matrix)] <- 0
  var2_matrix[is.na(var2_matrix)] <- 0
  ped$t <- t_matrix
  ped$tz <- tz_matrix
  ped$var1 <- var1_matrix
  ped$var2 <- var2_matrix
  ped$LL <- LL_matrix
  colnames(ped)[colnames(ped) == "var1"] <- varname1
  colnames(ped)[colnames(ped) == "var2"] <- varname2
  ped
}

# Compute effect differences as in Bender et al. 2018
# Taken from ELRA paper
getEffectDiffs <- function(
  m,
  newdata1,
  newdata2,
  effectname = "calCat") {
  
  ## deal with fits created with old mgcv before ti() was implemented
  ## when loaded with newer versions that have ti(): add the missing "mc"-slot
  if(!is.null(mgcv::ti)){
    m$smooth <- lapply(m$smooth, function(trm){
      if("margin" %in% names(trm) & is.null(trm$mc)){
        trm$mc <- rep(TRUE, length(trm$margin))
        #message("added mc slot to ", trm$label)
      }
      trm
    } )
  }
  
  useColumns <- grep(effectname, names(m$coefficients))
  covCoefs <- m$Vp[useColumns, useColumns]
  X1 <- predict(m, newdata = newdata1, type = "lpmatrix")[,useColumns]
  X2 <- predict(m, newdata = newdata2, type = "lpmatrix")[,useColumns]
  X <- X2 - X1
  fit <- drop(X %*% m$coefficients[useColumns])
  se <- sqrt(rowSums((X %*% covCoefs) * X))
  hi <- fit + 2 * se
  lo <- fit - 2 * se
  cbind(intmid = newdata1$int_mid, fit = exp(fit), se = se, hi = exp(hi),
        lo = exp(lo))
  
}
#******************************************************************************#
#************************ helper functions for plots **************************#

## adapted for mgcv gams from https://gist.github.com/dsparks/818976


### Categorial
coefPlotGAM_cat <- function(
  models = list(),
  data,
  alpha       = 0.05,
  modelnames  = NULL,
  col0        = "black",
  intercept   = FALSE,
  nrow.legend = 2,
  legend.position = "bottom",
  mod.cols    = c("#E41A1C", "#377EB8"),
  exclude     = c("ApacheIIScore", 
                  "inMV2_4",
                  "Propofol2_4",
                  "OralIntake2_4",
                  "PN2_4"),
  prettify    = TRUE) {
  # models must be a list()
  
  models <- lapply(models, function(z) {
    if( is.character(z) ) {
      summary(readRDS(z))
    }
    else {
      if( !(class(z)[1] == "summary.gam") ) {
        summary(z)
      }
      else z
    }
  })
  
  Multiplier <- qnorm(1 - alpha / 2)
  
  
  ## prep coefficient table, exclude not needed vars
  CoefficientTables <- lapply(models, "[[", i = "p.table")
  if( !intercept )
    CoefficientTables <- lapply(CoefficientTables, function(z) z[-1, ])
  if( !is.null(exclude) ) {
    
    for (i in seq_along(exclude)) {
      CoefficientTables[[1]] <- CoefficientTables[[1]][-grep(exclude[i], row.names(CoefficientTables[[1]])), ]
      CoefficientTables[[2]] <- CoefficientTables[[2]][-grep(exclude[i], row.names(CoefficientTables[[2]])), ]
      }
    } 
  
  
  
  for(i in seq_along(CoefficientTables)) {
    CoefficientTables[[i]] <- cbind.data.frame(CoefficientTables[[i]],
                                               vars=rownames(CoefficientTables[[i]]))
    if( prettify ) {
      df.i <- prettify_labels(CoefficientTables[[i]])
      CoefficientTables[[i]] <- merge(CoefficientTables[[i]], df.i, by="vars")
    } else{
      colnames(CoefficientTables[[i]])["vars"] <- "IV"
    }
    CoefficientTables[[i]]$Model <- factor(modelnames[i], rev(modelnames))
  }
  
  matrix.of.models <- do.call(rbind, CoefficientTables)
  matrix.of.models <- matrix.of.models[, c("Estimate", "Std. Error", "IV", "Model", "z value")]
  colnames(matrix.of.models) <- c("Estimate", "StandardError", "IV", "Model", "Standardized")
  matrix.of.models[["Multiplier"]] <- Multiplier

  p <- ggplot(data = matrix.of.models) +
    geom_hline(yintercept = 0, alpha = 0.8, lty = 1, col = col0 ) +
    theme_bw() +
    xlab("") +
    coord_flip()
  
  if(length(models) > 1) {
    p + geom_pointrange(aes(y = Estimate, x = IV,
                            color = Model,
                            ymax = Estimate + Multiplier * StandardError,
                            ymin = Estimate - Multiplier * StandardError),
                        position=position_dodge(width=.6)) +
      scale_color_manual("Model", values = rev(mod.cols)) +
      guides(color = guide_legend(nrow = nrow.legend, reverse=TRUE)) +
      theme(legend.position=legend.position)
    
    
  } else {
    p + geom_pointrange(aes(
      y = Estimate, x = IV,
      ymax = Estimate + Multiplier * StandardError,
      ymin = Estimate - Multiplier * StandardError)) +
      theme(legend.position="none") +
      scale_color_manual(values=mod.col)
  }
  

}

### metric variables:

coefPlotGAM_met <- function(
  models = list(),
  data,
  alpha       = 0.05,
  modelnames  = NULL,
  col0        = "black",
  intercept   = FALSE,
  nrow.legend = 2,
  legend.position = "bottom",
  mod.cols    = c("#E41A1C", "#377EB8"),
  exclude     = c("Year2008",
                  "Year2009",
                  "Year2011",
                  "Year2013",
                  "Year2014",
                  "DiagID2Cardio-Vascular",
                  "DiagID2Respiratory",
                  "DiagID2Gastrointestinal",
                  "DiagID2Neurologic",
                  "DiagID2Sepsis",
                  "DiagID2Orthopedic/Trauma",
                  "DiagID2Metabolic",
                  "DiagID2Renal",
                  "AdmCatIDSurgical/Elective",
                  "AdmCatIDSurgical/Emeregency",
                  "GenderMale"
                  ),
  prettify    = TRUE) {
  # models must be a list()
  
  models <- lapply(models, function(z) {
    if( is.character(z) ) {
      summary(readRDS(z))
    }
    else {
      if( !(class(z)[1] == "summary.gam") ) {
        summary(z)
      }
      else z
    }
  })
  
  Multiplier <- qnorm(1 - alpha / 2)
  
  
  ## prep coefficient table, exclude not needed vars
  CoefficientTables <- lapply(models, "[[", i = "p.table")
  if( !intercept )
    CoefficientTables <- lapply(CoefficientTables, function(z) z[-1, ])
  if( !is.null(exclude) ) {
    
    for (i in seq_along(exclude)) {
      CoefficientTables[[1]] <- CoefficientTables[[1]][-grep(exclude[i], row.names(CoefficientTables[[1]])), ]
      CoefficientTables[[2]] <- CoefficientTables[[2]][-grep(exclude[i], row.names(CoefficientTables[[2]])), ]
      }
    } 

 
  for(i in seq_along(CoefficientTables)) {
    CoefficientTables[[i]] <- cbind.data.frame(CoefficientTables[[i]],
                                               vars=rownames(CoefficientTables[[i]]))
    if( prettify ) {
      df.i <- prettify_labels(CoefficientTables[[i]])
      CoefficientTables[[i]] <- merge(CoefficientTables[[i]], df.i, by="vars")
    } else{
      colnames(CoefficientTables[[i]])["vars"] <- "IV"
    }
    CoefficientTables[[i]]$Model <- factor(modelnames[i], rev(modelnames))
  }
  
  matrix.of.models <- do.call(rbind, CoefficientTables)
  matrix.of.models <- matrix.of.models[, c("Estimate", "Std. Error", "IV", "Model", "z value")]
  colnames(matrix.of.models) <- c("Estimate", "StandardError", "IV", "Model", "Standardized")
  matrix.of.models[["Multiplier"]] <- Multiplier

  p <- ggplot(data = matrix.of.models) +
    geom_hline(yintercept = 0, alpha = 0.8, lty = 1, col = col0 ) +
    theme_bw() +
    xlab("") +
    coord_flip()
  
  if(length(models) > 1) {
    p + geom_pointrange(aes(y = Standardized, x = IV,
                            color = Model,
                            ymax = Standardized + Multiplier * StandardError,
                            ymin = Standardized - Multiplier * StandardError),
                        position=position_dodge(width=.6)) +
      ylab("Standardized Estimate") +
      scale_color_manual("Model", values = rev(mod.cols)) +
      guides(color = guide_legend(nrow = nrow.legend, reverse=TRUE)) +
      theme(legend.position=legend.position)
    
    
  } else {
    p + geom_pointrange(aes(
      y = Standardized, x = IV,
      ymax = Standardized + Multiplier * StandardError,
      ymin = Standardized - Multiplier * StandardError)) +
      theme(legend.position="none") +
      scale_color_manual(values=mod.col)
  }
  
  
}
# Function for Labels: Fixed Effects

prettify_labels <- function(coeftab) {
  
  labels <- c(
    "Intercept",
    "Year of therapy: 2008",
    "Year of therapy: 2009",
    "Year of therapy: 2011",
    "Year of therapy: 2013",
    "Year of therapy: 2014",
    "Apache II Score",
    "Admission diagnosis: Cardio-Vascular",
    "Admission diagnosis: Respiratory",
    "Admission diagnosis: Gastrointestinal",
    "Admission diagnosis: Neurologic",
    "Admission diagnosis: Sepsis",
    "Admission diagnosis: Orthopedic/Trauma",
    "Admission diagnosis: Metabolic",
    "Admission diagnosis: Renal",
    "Admission: Surgical/Elective",
    "Admission category: Surgical/Emergency",
    "Gender: Male",
    "#days with MV\n (up to day 4)",
    "#days with Propofol\n (up to day 4)",
    "#days with Oral Intake\n (up to day 4)",
    "#days Parenteral Nutrition\n (up to day 4)",
    "Apache II Score:t")
  
  vars <- c(
    "(Intercept)",
    "Year2008",
    "Year2009",
    "Year2011",
    "Year2013",
    "Year2014",
    "ApacheIIScore",
    "DiagID2Cardio-Vascular",
    "DiagID2Respiratory",
    "DiagID2Gastrointestinal",
    "DiagID2Neurologic",
    "DiagID2Sepsis",
    "DiagID2Orthopedic/Trauma",
    "DiagID2Metabolic",
    "DiagID2Renal",
    "AdmCatIDSurgical/Elective",
    "AdmCatIDSurgical/Emeregency",
    "GenderMale",
    "inMV2_4",
    "Propofol2_4",
    "OralIntake2_4",
    "PN2_4",
    "ApacheIIScore:int_mid")
  
  df.labs <- cbind.data.frame(vars = as.factor(vars), IV = as.factor(labels))
  for( i in labels ) {
    df.labs$IV <- relevel(df.labs$IV, ref = i)
  }
  
  df.labs[match(rownames(coeftab), df.labs$vars), ]
  
}

# Plot: CIF

get_cif_cs <- function(
  newdata,
  object1,
  object2,
  protocol_name = "",
  n_sim = 500L,
  time_var = "int_mid",
  alpha = 0.05,
  cause_names = c("death", "discharge")
) {
  
  newdata <- newdata %>%
    mutate(intlen = tend - tstart)
  
  coefs_1        <- coef(object1)
  V_1            <- object1$Vp
  sim_coef_mat_1 <- mvtnorm::rmvnorm(n_sim, mean = coefs_1, sigma = V_1)
  
  coefs_2        <- coef(object2)
  V_2            <- object2$Vp
  sim_coef_mat_2 <- mvtnorm::rmvnorm(n_sim, mean = coefs_2, sigma = V_2)
  
  hazards <- purrr:::map2(
    .x = list(object1, object2),
    .y = list(sim_coef_mat_1, sim_coef_mat_2),
    .f = ~{
      .df <- newdata %>% arrange(.data[[time_var]], .by_group = TRUE)
      X <- predict(.x, .df, type = "lpmatrix")
      apply(.y, 1, function(z) exp(X %*% z))
    }
  )
  
  overall_survivals <- apply(
    Reduce("+", hazards),
    2,
    function(z) exp(-cumsum(z * newdata[["intlen"]])))
  
  hps <- purrr::map(hazards, ~ .x * (overall_survivals - 1e-10))
  cifs <- purrr::map(hps, ~ apply(.x, 2, function(z) cumsum(z * newdata[["intlen"]])))
  names(cifs) <- cause_names
  cifs_df <- purrr::imap_dfr(
    .x = cifs,
    .f = ~{
      newdata[["cif"]]       <- rowMeans(.x)
      newdata[["cif_lower"]] <- apply(.x, 1, quantile, alpha/2)
      newdata[["cif_upper"]] <- apply(.x, 1, quantile, 1-alpha/2)
      newdata[["cause"]] <- .y
      newdata[["protocol"]] <- protocol_name
      newdata
    }
  )
}


get_cif_df <- function(
  comparison_list,
  object1,
  object2,
  comparison_name = "",
  n_sim = 500L,
  time_var = "int_mid",
  alpha = 0.05,
  cause_names = c("death", "discharge")) {
  
  .out <- purrr::imap_dfr(
    .x = comparison_list,
    .f = ~{
      get_cif_cs(.x, object1 = object1, object2 = object2, protocol_name = .y,
                 n_sim = n_sim, time_var = time_var, alpha = alpha, cause_names = cause_names)
    })
  .out[["comparison"]] <- comparison_name
  .out
  
}


#************************ Settings: Labels, Size ******************************#
# themes_plot for single plots (Bericht)
themes_plot <- theme(plot.title = element_text(size = (40), family = "Times New Roman"), 
                     plot.subtitle = element_text(size = (35), family = "Times New Roman"), 
                     axis.title = element_text(size = (40), family = "Times New Roman"), 
                     axis.text = element_text(size = 30, family = "Times New Roman"), 
                     legend.key.size = unit(0.5, 'cm'),
                     legend.title = element_text(size = 40, family = "Times New Roman"),
                     legend.text = element_text(size = 40, family = "Times New Roman"))

# themes_plot2 all comparisons (Bericht)
themes_plot2 <- theme(plot.title = element_text(size = (20), family = "Times New Roman"), 
                      plot.subtitle = element_text(size = (15), family = "Times New Roman"), 
                      axis.title = element_text(size = (20), family = "Times New Roman"), 
                      axis.text = element_text(size = 15, family = "Times New Roman"), 
                      legend.key.size = unit(0.5, 'cm'),
                      legend.title = element_text(size = 20, family = "Times New Roman"),
                      legend.text = element_text(size = 20, family = "Times New Roman"))


# themes_plot3 all comparisons (PowerPoint)
themes_plot3 <- theme(plot.title = element_text(size = (14)), 
                      plot.subtitle = element_text(size = (13)), 
                      axis.title = element_text(size = (14)), 
                      axis.text = element_text(size = 12), 
                      legend.key.size = unit(0.5, 'cm'),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size = 20))

# themes_plot4 for stepwise hazard ratio (PowerPoint)
themes_plot4 <- theme(plot.title = element_text(size = (24)), 
                      axis.title = element_text(size = (24)), 
                      axis.text = element_text(size = 20))

theme_set(theme_bw())
