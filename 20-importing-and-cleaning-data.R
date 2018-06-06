##### OPTIONAL ###########################################################
#
# To replicate plots, this step is optional, since cleaned and fixed
# data frame for analysis is saved in repository.
#
# The reason for this is that GSS2014.sav file is too big for github
#

############ IMPORTING DATA ############
#
# importing two datasets, so there is no need to manually deal with different levels of 
# missingdata, in one all missing are as NA, in other, they have individual codes

GSS_14 <- read.spss("GSS2014.sav", to.data.frame = TRUE, trim.factor.names = TRUE,
                    trim_values = TRUE, use.missings = TRUE)
GSS_14_MISS <- read.spss("GSS2014.sav", to.data.frame = TRUE, trim.factor.names = TRUE,
                         trim_values = TRUE, use.missings = FALSE)

# Recoding religion
GSS_14_MISS$religion <- rec_relig_12(GSS_14_MISS$relig, GSS_14_MISS$denom, GSS_14_MISS$other)
GSS_14$religion <- GSS_14_MISS$religion

# selecting only cases that did the ISSP module on nationalism 
GSS_14$clseusa_miss <- GSS_14_MISS$clseusa
rm_cases <- which(GSS_14$clseusa_miss == "IAP")
GSS_14 <- GSS_14[-rm_cases,]


# Create vectors with names of variables to be used in analysis and select them
var_n_criteria <- c("ambornin", "amcit", "amlived", "amenglsh",
                    "amchrstn", "amgovt", "amfeel", "amancstr")
var_n_other_plus <- c("amproud1")	# to remove those who say are not american
var_other <- c("year", "sex", "coninc", "age", "born", "race", "citizen", "parcit",
               "region", "religion", "padeg", "madeg", "polviews", "degree", "srcbelt",
               "res16", "xnorcsiz", "size", "vetyears")
var_all_14 <- c(var_other, var_n_criteria, var_n_other_plus)

GSS_14 <- GSS_14[var_all_14]

# Remove 50 cases that answered question 'How proud are you of being American?'
# as 'I AM NOT AMERICAN' 
GSS_14 <- subset(GSS_14, ((GSS_14$amproud1 != "I AM NOT AMERICAN") | is.na(GSS_14$amproud1)))
GSS_14$amproud1 <- droplevels(GSS_14$amproud1)

# Create a backup copy of the dataset
GSS_2014_BACKUP <- GSS_14
# GSS_14 <- GSS_2014_BACKUP


###### RECODING #####
#
#
#

# urban rural residence
GSS_14$r_srcbelt <- Recode(GSS_14$srcbelt, "c('SUBURB, 12 LRGST','SUBURB, 13-100')='SUBURB100'; 
                           'OTHER URBAN'='OTH URBAN';'OTHER RURAL'='RURAL'; else='CITY100'",
                           levels = c("CITY100", "SUBURB100", "OTH URBAN", "RURAL"))
# place where lived until age of 16
GSS_14$r_res16 <- Recode(GSS_14$res16, "c('CITY GT 250000','BIG-CITY SUBURB')='LRG CITY & SUB';
                         c('50000 TO 250000','TOWN LT 50000')='CITY';
                         c('FARM','COUNTRY,NONFARM')='RURAL'",
                         levels = c("LRG CITY & SUB", "CITY", "RURAL"))
# religion
GSS_14$r_religion <- Recode(GSS_14$religion, "
                            c('Sectarian Protestant', 'Baptist')='SECT&BAPT';
                            c('Moderate Protestant', 'Liberal Protestant')='MOD&LIB';
                            c('Lutheran', 'Episcopalian', 'Mormon')='LUTH&EPI&MORM';
                            c('Jewish', 'Other religion', 'None')='NONE&OR&JEW';
                            c('Catholic and Orthodox')='CATH&ORTH';
                            c('Christian, no group given')='CHR-NGG';
                            c('Dont know','No answer')=NA")
# summary(GSS_14$race)	# no recoding
# summary(GSS_14$sex)	# no recoding
# summary(GSS_14$age)	# no recoding
# but also age into decades to see if there is a break
GSS_14$year_num <- as.character(GSS_14$year)
GSS_14$year_num <- as.numeric(GSS_14$year_num)
GSS_14$age <- as.numeric(GSS_14$age)
GSS_14$age <- GSS_14$age + 17
GSS_14$age_born <- GSS_14$year_num - GSS_14$age
GSS_14$age_born <- GSS_14$age_born - 1900
GSS_14$age_born <- GSS_14$age_born/10
GSS_14$age_born <- trunc(GSS_14$age_born)
GSS_14$age_born[GSS_14$age_born < 3] <- 2 # grouping 1910 & 1920s
GSS_14$age_born <- (GSS_14$age_born*10) + 1900
GSS_14$age_bd <- as.factor(GSS_14$age_born)

GSS_14$veteran <- Recode(GSS_14$vetyears, "'NONE'='NO'; else='YES'")
# immigration background
GSS_14$pcitizens <- Recode(GSS_14$parcit, "'BOTH WERE CITIZENS OF AMERICA'='YES'; NA=NA; else='NO'")
GSS_14$immigrant <- !((GSS_14$citizen %in% c("YES")) & (GSS_14$parcit %in% c("BOTH WERE CITIZENS OF AMERICA"))
                      & (GSS_14$born %in% c("YES")))
GSS_14$immigrant[is.na(GSS_14$citizen) & is.na(GSS_14$parcit) & is.na(GSS_14$born)] <- NA
GSS_14$immigrant <- as.factor(GSS_14$immigrant)
# parents' education
GSS_14$prt_ba <- GSS_14$madeg %in% c("BACHELOR", "GRADUATE") | GSS_14$padeg %in% c("BACHELOR", "GRADUATE")
GSS_14$prt_ba[is.na(GSS_14$madeg) & is.na(GSS_14$padeg)] <- NA
GSS_14$prt_ba <- factor(GSS_14$prt_ba)
# region
GSS_14$r_region_4 <- Recode(GSS_14$region, "c('NEW ENGLAND','MIDDLE ATLANTIC')='NORTHEAST';
                            c('E. NOR. CENTRAL','W. NOR. CENTRAL')='MIDWEST';
                            c('SOUTH ATLANTIC','E. SOU. CENTRAL','W. SOU. CENTRAL')='SOUTH';
                            c('MOUNTAIN','PACIFIC')='WEST'")
# political views - although unrecoded will be used
GSS_14$r_polviews <- Recode(GSS_14$polviews, "c('EXTREMELY LIBERAL','LIBERAL','SLIGHTLY LIBERAL')='LIBERAL';
                            c('SLGHTLY CONSERVATIVE','CONSERVATIVE','EXTRMLY CONSERVATIVE')='CONSERVATIVE'",
                            levels = c("LIBERAL", "MODERATE", "CONSERVATIVE"))
# education
GSS_14$r_degree <- Recode(GSS_14$degree, "c('JUNIOR COLLEGE','HIGH SCHOOL')='HS OR JC';
                          c('BACHELOR','GRADUATE')='BA OR MORE';
                          c('LT HIGH SCHOOL')='LT HS'",
                          levels = c("LT HS", "HS OR JC", "BA OR MORE"))
# income in $20k
GSS_14$r_coninc <- as.numeric(as.character(GSS_14$coninc)) / 20000

# calculate criteria of belonging (factor analysis in full thesis)
GSS_14$criteria_plus <- average.excluding(sapply(GSS_14[var_n_criteria], as.numeric), 6)
GSS_14$criteria_plus_05 <- round(GSS_14$criteria_plus/0.5)*0.5	# rounded to nearest 0.5
GSS_14$criteria_plus_1 <- round(GSS_14$criteria_plus)			# rounded to whole number

##### Reducing to regession N
var_regression_14 <- c("criteria_plus", "age", "r_res16", "r_religion", "race", "sex",
                       "veteran", "immigrant", "prt_ba", "r_region_4", "polviews", "r_degree",
                       "r_coninc", "r_srcbelt")

# Remove the one outlier
# see decision to remove the case #1234 in replication code for the whole thesis
GSS_14 <- subset(GSS_14, rownames(GSS_14) != "2451")

# remove all other cases that have missing values
GSS_14$missing <- apply(GSS_14[,var_regression_14], 1, function(x) sum(is.na(x)))
GSS_14 <- subset(GSS_14, missing == 0)


## save the data so  
save(GSS_14, file = "GSS14.Rdata")