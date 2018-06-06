#######################################
#                                     #
#     REG AND PREPARING LABELS        #
#                                     #
#######################################

## Load data  -------------------------------------------------------
# in previous step how to get here from GSS2014.sav

load("GSS14.Rdata")

## Regression -------------------------------------------------------
#  For full diagnostics, see full MA paper replication

GSS_14$r_res16_c <- relevel(GSS_14$r_res16, ref = "CITY")
GSS_14$polviews_m <- relevel(GSS_14$polviews, ref = "MODERATE")
GSS_14$r_religion_m <- relevel(GSS_14$r_religion, ref = "MOD&LIB")

clm_small <- lm(criteria_plus ~ age + sex + race + immigrant +
                  veteran + prt_ba + r_degree + 
                  r_coninc + r_srcbelt, data = GSS_14)
clm_small_res16 <- update(clm_small, ~ . + r_res16_c)
clm_full <- update(clm_small_res16, ~ . + r_religion_m + r_region_4 +
                    polviews_m)



#### PREPARING FOR PLOT ========================================================

#### alternative easier way, not completed -------------------------------------
# mc_s <- tidy(clm_small)
# mc_r <- tidy(clm_small_res16)
# mc_f <- tidy(clm_full)
# rename variable names
# mc_s <- plyr::rename(mc_s, c("term"="factor", "estimate"="AME"))
# mc_r <- plyr::rename(mc_r, c("term"="factor", "estimate"="AME"))
# mc_f <- plyr::rename(mc_f, c("term"="factor", "estimate"="AME"))
# mc_s$factor <- as.factor(mc_s$factor)
# mc_r$factor <- as.factor(mc_r$factor)
# mc_f$factor <- as.factor(mc_f$factor)
# remove intercepts
# mc_s <- subset(mc_s, factor != "(Intercept)")
# mc_r <- subset(mc_r, factor != "(Intercept)")
# mc_f <- subset(mc_f, factor != "(Intercept)")

#### Extract coefficients (ame) ------------------------------------------------
# first extract coefficients (av. marg. effects) for all thre models
# that are goind to be plotted
# for whatever reason I am using margins from margins package, which
# makes things a bit more complicated, and longer
# it would have been much easier with "tidy" from broom
# mc_s - small model; mc_r - model with childhood place; mc_f - full model
mc_s <- as.data.frame(summary(margins(clm_small)))
mc_r <- as.data.frame(summary(margins(clm_small_res16)))
mc_f <- as.data.frame(summary(margins(clm_full)))


#### Fix the labels and prepare them for ordering and poloting ----------------

# copy of the full name of factors from lm models which will be used later
mc_s$full_factor <- mc_s$factor
mc_r$full_factor <- mc_r$factor
mc_f$full_factor <- mc_f$factor

# create new variable with names of the variables from the model
mc_s$var <- mc_s$factor
mc_r$var <- mc_r$factor
mc_f$var <- mc_f$factor

# asing model names, so they can be distinguished
mc_f$model <- as.factor(c("Large"))
mc_s$model <- as.factor(c("Reduced"))
mc_r$model <- as.factor(c("W/ childhood"))

## for regular confusing model
MU <- rbind(mc_f, mc_s)
MU <- rbind(MU, mc_r)
MU$full_factor <- as.factor(MU$full_factor)
MU$factor <- as.factor(MU$factor)
MU$var <- as.factor(MU$var)

## Return to fixing names -----------------------------------------------
# This whole process is tiredsome and manual, I am working on
# making a function for it

# first, variable names

mc_s$var <- c(
  "Age", "Immigrant", "Parents' education", "Income",
  "Education", "Education", "SMSA", "SMSA", "SMSA", "Race",
  "Race", "Sex", "Veteran"
)
mc_r$var <- c(
  "Age", "Immigrant", "Parents' education", "Income", "Education",
  "Education", "Childhood", "Childhood", "SMSA", "SMSA", "SMSA",
  "Race", "Race", "Sex", "Veteran"
)
mc_f$var <- c(
  "Age", "Immigrant", "Political views", "Political views",
  "Political views", "Political views", "Political views",
  "Political views", "Parents' education", "Income", "Education",
  "Education", "Region", "Region", "Region", "Religion",
  "Religion", "Religion", "Religion", "Religion", "Childhood",
  "Childhood", "SMSA", "SMSA", "SMSA", "Race", "Race", "Sex", "Veteran"
)

## remove variable name from the name of the factor

mc_s$factor <- c(
  "One year older", "Yes", "Either has BA", "Increase of $20,000",
  "BA or more", "HS or JC", "Other urban", "Rural",
  "Suburban (top 100)", "Black", "Other", "Female", "Yes"
)
mc_r$factor <- c(
  "One year older", "Yes", "Either has BA", "Increase of $20,000",
  "BA or more", "HS or JC", "Large city or suburbs", "Rural",
  "Other urban", "Rural", "Suburban (top 100)", "Black",
  "Other", "Female", "Yes"
)
mc_f$factor <- c(
  "One year older", "Yes", 
  "Conservative", "Extremly Liberal",
  "Extremly Conservative", "Liberal", 
  "Slightly Conservative", "Slightly Liberal", 
  "Either has BA", "Increase of $20,000",
  "BA or more", "HS or JC", 
  "Northeast", "South",
  "West", "Cath. or orth.", 
  "Christian NGG", "Luth., Epsic., or Morm.",
  "None, other, or Jewish", "Sect. or Bapt.",
  "Large city or suburbs","Rural", 
  "Other urban", "Rural", 
  "Suburban (top 100)", 
  "Black","Other",
  "Female", "Yes"
)

## assign model name as a new variable to each mdodel
mc_s$model <- as.factor(c("Reduced"))
mc_r$model <- as.factor(c("W/ childhood"))
mc_f$model <- as.factor(c("Large"))


### COMBINE three models to one DF for ggplot ------------------------

MB <- rbind(mc_f, mc_s)
MB <- rbind(MB, mc_r)

## make all the labels to be factors
MB$full_factor <- as.factor(MB$full_factor)
MB$factor <- as.factor(MB$factor)
MB$var <- as.factor(MB$var)

# make backup
MB_backup <- MB # MB <- MB_backup

# Make a copy of the data that will be used with different desing
MN <- MB_backup