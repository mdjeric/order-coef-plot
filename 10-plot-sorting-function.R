sort_models <- function(MDS) {
  ########  REORDERING FACTORS #######
  # Function that reorders factors in multiple models for 
  # better visualisation
  # Required var names:
  # - 'full_factor' output name of the factor from lm(...);
  # - 'var' name of the variable - it has to be ordered! (alphabetic~ascending);
  # - 'factor' probably more precise of the level,
  #       it is not used in this step, since it is possible that two
  #       same levels for two different variables exist;
  # - 'AME' either average marginal effect or coefficient, numeric.
  
  
  
  
  ## 1 # assign the number of models each factor is involved to var n_mod
  MDS$n_mod <- 0
  
  TMP <- MDS %>%
    group_by(full_factor) %>%
    summarise(count = n())
  
  TMP <- as.data.frame(TMP)
  
  # because plot will be rotated, reverse the numbers so the factor with most
  # models has the lowest value - thus will be on the bottom of the plot
  
  TMP$count <- (TMP$count - (max(TMP$count) + 1)) * (-1)
  
  for (i in c(1:nrow(TMP))) {
    MDS[MDS$full_factor == TMP[i, 1], "n_mod"] <- as.numeric(TMP[i, 2])
  }
  
  ## 2 # now order according to mean estimator for each factor to n_fact
  
  MDS$n_fact <- 0
  
  TMP <- MDS %>%
    group_by(full_factor) %>%
    summarise(mean = mean(AME))
  
  TMP <- data.frame(full_factor = TMP$full_factor,
                    mean = TMP$mean)
  
  # take ranking, i.e. order of each mean 
  TMP$ordr <- rank(TMP$mean)
  
  for (i in c(1:nrow(TMP))) {
    MDS[MDS$full_factor == TMP[i, 1], "n_fact"] <- as.numeric(TMP[i, 3])
  }
  
  ## 3 # finally, the same just for whole variables
  MDS$n_var <- 0
  
  TMP <- MDS %>%
    group_by(var) %>%
    summarise(mean = mean(AME))
  
  TMP <- data.frame(var = TMP$var,
                    mean = TMP$mean)
  
  TMP$ordr <- rank(TMP$mean)
  for (i in c(1:nrow(TMP))) {
    MDS[MDS$var == TMP[i, 1], "n_var"] <- as.numeric(TMP[i, 3])
  }
  
  ### ordr # NOW CREATER ONE VARIABLE
  # it synthesises all the previous rankings according to priority
  
  MDS$ordr <- (MDS$n_mod * 10000) + (MDS$n_var * 100) + MDS$n_fact
  #MDS$ordr <- as.integer(MDS$ordr)    # this is probably unnecessary transformation 
  #MDS$ordr <- as.factor(MDS$ordr)     # this too
  
  # this is final variable that will be used for "relevel" in ggplot
  MDS$ordr <- as.integer(rank(MDS$ordr))
  
  ### v_ordr # REORDER LEVELS WITHIN var VARIABLE
  # first position of each variable according to model and mean estimators
  MDS$v_ord <- (MDS$n_mod * 10000) + (MDS$n_var * 100)
  MDS$v_ord <- as.integer(MDS$v_ord)
  # reduce the repeated values
  MDS_n <- data.frame(var = MDS$var,
                      v_ord = MDS$v_ord)
  MDS_n <- unique(MDS_n)
  # sort according to alphabet (the way they are in the original DF)
  MDS_n <- MDS_n[order(MDS_n$var), ]
  # extract numeric factor for releveling of the variable factor, it has to be
  # descenting order, because plot is rotated. And relevel factor.
  reord_lev <- order(MDS_n$v_ord, decreasing = TRUE)
  MDS$var <- factor(MDS$var, levels(MDS$var)[reord_lev])
  return(MDS)
}