
rm(list = ls(all.names = TRUE))

library(readr)
library(dplyr)
library(stats)
library(car)
library(lmtest)

# Load the data
fileHome <- 'G:/SNHU work/Applied Economics/ECO-620 Applied Econometrics I/Final Project/'
rawData <- read_csv(paste0(fileHome,'youth-dataset-long-form.csv'))

data <- rawData %>%
    filter(COUNTRY_OF_BIRTH %in% c("IN THE US", "IN OTHER COUNTRY")) %>%
    mutate(
        INCOME = as.numeric(TNFI_),
        FULL_YEAR_OF_BIRTH = ifelse(YEAR_OF_BIRTH > 20, 1900 + YEAR_OF_BIRTH, 2000 + YEAR_OF_BIRTH),
        AGE = YEAR - FULL_YEAR_OF_BIRTH,
        RACE = factor(SAMPLE_RACE),
        FAMSIZE = as.numeric(FAMSIZE_),
        EDU_DEGREE_ABBR = factor(
            case_when(
                grepl("None", EDU_DEGREE, ignore.case = TRUE) ~ 'None',
                grepl("Diploma", EDU_DEGREE, ignore.case = TRUE) ~ 'HS',
                grepl("Associate", EDU_DEGREE, ignore.case = TRUE) ~ 'AA',
                grepl("Bachelor", EDU_DEGREE, ignore.case = TRUE) & grepl("Arts", EDU_DEGREE, ignore.case = TRUE) ~ 'BA',
                grepl("Bachelor", EDU_DEGREE, ignore.case = TRUE) & grepl("Science", EDU_DEGREE, ignore.case = TRUE) ~ 'BS',
                grepl("Master", EDU_DEGREE, ignore.case = TRUE) ~ 'Masters',
                grepl("Professional", EDU_DEGREE, ignore.case = TRUE) ~ 'Professional',
                grepl("Doctor", EDU_DEGREE, ignore.case = TRUE) ~ 'PhD',
                TRUE ~ 'Other'
            )
        ),
        STANDARDIZED_TNFI = scale(TNFI_)
    ) %>%
    filter(INCOME > 0) %>%
    mutate(
        LOG_INCOME = log(INCOME + 1),  # Calculate LOG_INCOME now that INCOME is guaranteed to be positive
        EDU_DEGREE_BINARY = as.integer(EDU_DEGREE %in% c("Bachelor of Arts Degree (BA)",
                                                         "Bachelor of Science (BS)",
                                                         "Master's Degree (MA, MBA, MS, MSW)",
                                                         "Doctoral Degree (PhD)",
                                                         "Professional Degree (MD, LLD, DDS)")),
        EVER_DIVORCED_FACTOR = factor(
            ifelse(EVER_DIVORCED_ %in% c("yes", "no"), EVER_DIVORCED_, "other"),
            levels = c("yes", "no", "unknown")
        ),
        EVER_UNEMPLOYED_FACTOR = factor(
            ifelse(EVER_UNEMPLOYED_ %in% c("yes", "no"), EVER_UNEMPLOYED_, "other"),
            levels = c("yes", "no", "unknown")
        ),
        EVER_EDU_LOAN_FACTOR = factor(
            ifelse(EVER_EDU_LOAN %in% c("yes", "no"), EVER_EDU_LOAN, "other"),
            levels = c("yes", "no", "unknown")
        ),
        EVER_IN_POVERTY_FACTOR = factor(
            ifelse(EVER_IN_POVERTY %in% c("yes", "no"), EVER_IN_POVERTY, "unknown"),
            levels = c("yes", "no", "unknown")
        ),
        WHEN_IN_POVERTY_FACTOR = factor(WHEN_IN_POVERTY),
        AGE_1STCHILD_FACTOR = factor(AGE_1STCHILD),
        POVSTATUS_FACTOR = factor(POVSTATUS_),
        SAMPLE_SEX_FACTOR = factor(SAMPLE_SEX),
        SAMPLE_RACE_FACTOR = factor(SAMPLE_RACE),
        COUNTRY_OF_BIRTH_FACTOR = factor(COUNTRY_OF_BIRTH),
        URBAN_RURAL_FACTOR = factor(
            case_when(
                URBAN_RURAL_ == "1: URBAN" ~ "Urban",
                URBAN_RURAL_ == "0: RURAL" ~ "Rural",
                URBAN_RURAL_ == "2: UNKNOWN" | URBAN_RURAL_ %in% c("-5", "-3", "-4") ~ "Unknown",
                TRUE ~ "Unknown"
            ),
            levels = c("Urban", "Rural", "Unknown")
        ),
        MARITAL_STATUS_FACTOR = factor(
            case_when(
                MARSTAT_KEY_ == "0: 0  NEVER MARRIED" ~ "Never Married",
                MARSTAT_KEY_ == "1: 1  MARRIED" ~ "Married",
                MARSTAT_KEY_ == "2: 2  SEPARATED" ~ "Separated",
                MARSTAT_KEY_ == "3: 3  DIVORCED" ~ "Divorced",
                MARSTAT_KEY_ == "6: 6  WIDOWED" ~ "Widowed",
                MARSTAT_KEY_ %in% c("-3", "-2", "-4") ~ "Unknown",
                TRUE ~ "Unknown"
            ),
            levels = c("Never Married", "Married", "Separated", "Divorced", "Widowed", "Unknown")
        ),
        REGION_FACTOR = factor(
            case_when(
                REGION_ == "1: NORTHEAST" ~ "Northeast",
                REGION_ == "2: NORTH CENTRAL" ~ "North Central",
                REGION_ == "3: SOUTH" ~ "South",
                REGION_ == "4: WEST" ~ "West",
                REGION_ %in% c("-3", "-4") ~ "Unknown",
                TRUE ~ "Unknown"
            ),
            levels = c("Northeast", "North Central", "South", "West", "Unknown")
        ),
        HAVING_HEALTHPLAN_FACTOR = factor(
            case_when(
                HAVING_HEALTHPLAN == "Yes" ~ "Yes",
                HAVING_HEALTHPLAN == "No" ~ "No",
                is.na(HAVING_HEALTHPLAN) | HAVING_HEALTHPLAN %in% c("-1", "-2", "-3", "-4") ~ "Unknown",
                TRUE ~ "Unknown"
            ),
            levels = c("Yes", "No", "Unknown")
        ),
        TEMP_AGE = if_else(AGE == 0, NA_real_, AGE),
        STANDARDIZED_AGE = scale(TEMP_AGE, center = TRUE, scale = TRUE)
    ) %>%
    mutate(
        STANDARDIZED_AGE = if_else(is.na(STANDARDIZED_AGE),
                                   mean(STANDARDIZED_AGE, na.rm = TRUE),
                                   as.numeric(STANDARDIZED_AGE)
        ),
        EMPLOYED_FACTOR = factor(
            case_when(
                grepl("^[1234]", EMP_STATUS_) ~ as.character(EMP_STATUS_),
                TRUE ~ "unknown"
            ),
            levels = unique(c(as.character(EMP_STATUS_[grepl("^[1234]", EMP_STATUS_)]), "unknown"))
        )
    )

# removing unneeded variables
data <- data %>%
    select(-EMP_STATUS_, -EDU_LOAN_,
           -EVER_UNEMPLOYED_, -EVER_DIVORCED_,
           -EDU_DEGREE, -EVER_EDU_LOAN,
           -EVER_IN_POVERTY, -WHEN_IN_POVERTY,
           -AGE_1STCHILD, -URBAN_RURAL_,
           -MARSTAT_KEY_, -REGION_,
           -POVSTATUS_, -HAVING_HEALTHPLAN,
           -C1DOB_Y, -COUNTRY_OF_BIRTH,
           -SAMPLE_RACE, -SAMPLE_SEX,
           -TEMP_AGE, -URBAN_RURAL_,
           -EVER_IN_POVERTY, -TNFI_,
           -REGION_, -EVER_DIVORCED_FACTOR,
           -EMP_STATUS_)


# Proceed with the initial regression model
model1 <- lm(INCOME ~ EDU_DEGREE_ABBR + AGE + EMPLOYED_FACTOR + FAMSIZE, data = data)
summary(model1)
# Call:
# lm(formula = INCOME ~ EDU_DEGREE_ABBR + AGE + EMPLOYED_FACTOR +
#        FAMSIZE, data = data)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -149023  -19747   -4670    8625 1050207
#
# Coefficients:
#                                      Estimate Std.    Error t  value    Pr(>|t|)
# (Intercept)                              -38669.10     810.45 -47.713  < 2e-16 ***
#     EDU_DEGREE_ABBRBA                     18006.75     707.94  25.435  < 2e-16 ***
#     EDU_DEGREE_ABBRBS                     21449.30     596.49  35.959  < 2e-16 ***
#     EDU_DEGREE_ABBRHS                     -5273.17     460.60 -11.448  < 2e-16 ***
#     EDU_DEGREE_ABBRMasters                24939.99     689.08  36.193  < 2e-16 ***
#     EDU_DEGREE_ABBRNone                  -20695.87     601.04 -34.434  < 2e-16 ***
#     EDU_DEGREE_ABBROther                  -3032.72     614.49  -4.935 8.01e-07 ***
#     EDU_DEGREE_ABBRPhD                    48782.19    1834.80  26.587  < 2e-16 ***
#     EDU_DEGREE_ABBRProfessional           63182.20    1656.72  38.137  < 2e-16 ***
#     AGE                                    2097.23      21.26  98.641  < 2e-16 ***
#     EMPLOYED_FACTOR3: OUT OF LABOR FORCE  -8276.89     389.50 -21.250  < 2e-16 ***
#     EMPLOYED_FACTOR2: UNEMPLOYED          -8817.44     541.27 -16.290  < 2e-16 ***
#     EMPLOYED_FACTOR4: IN ACTIVE FORCES    -5783.76     742.31  -7.792 6.65e-15 ***
#     EMPLOYED_FACTORunknown                -1540.41     497.79  -3.094  0.00197 **
#     FAMSIZE                                4789.45      75.31  63.598  < 2e-16 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 58320 on 197512 degrees of freedom
# Multiple R-squared:  0.1594,	Adjusted R-squared:  0.1593
# F-statistic:  2675 on 14 and 197512 DF,  p-value: < 2.2e-16

# Final model
model_4 <- lm(LOG_INCOME ~ EDU_DEGREE_ABBR +
                  poly(STANDARDIZED_AGE, 2) +
                  poly(STANDARDIZED_TNFI, 2) +
                  URBAN_RURAL_FACTOR + MARITAL_STATUS_FACTOR +
                  SAMPLE_SEX_FACTOR + HOURS_WORKED_PER_WEEK_ +
                  SAMPLE_RACE_FACTOR +
                  FAMSIZE,
              data = data)

summary(model_4) # 0.6959
# Call:
# lm(formula = LOG_INCOME ~ EDU_DEGREE_ABBR + poly(STANDARDIZED_AGE,
#                                                  2) + poly(STANDARDIZED_TNFI, 2) + URBAN_RURAL_FACTOR + MARITAL_STATUS_FACTOR +
#        SAMPLE_SEX_FACTOR + HOURS_WORKED_PER_WEEK_ + SAMPLE_RACE_FACTOR +
#        FAMSIZE, data = data)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -8.7021 -0.1603  0.1356  0.3398  3.2990
#
# Coefficients:
#                                        Estimate Std.  Error    t value  Pr(>|t|)
# (Intercept)                                9.730e+00  6.049e-03 1608.434  < 2e-16 ***
# EDU_DEGREE_ABBRBA                         -1.189e-02  7.283e-03   -1.633 0.102502
# EDU_DEGREE_ABBRBS                         -1.816e-03  6.171e-03   -0.294 0.768604
# EDU_DEGREE_ABBRHS                         -6.140e-02  4.728e-03  -12.987  < 2e-16 ***
# EDU_DEGREE_ABBRMasters                    -1.694e-02  7.120e-03   -2.379 0.017359 *
# EDU_DEGREE_ABBRNone                       -3.257e-01  6.241e-03  -52.185  < 2e-16 ***
# EDU_DEGREE_ABBROther                      -2.476e-01  6.359e-03  -38.937  < 2e-16 ***
# EDU_DEGREE_ABBRPhD                        -1.216e-01  1.886e-02   -6.447 1.14e-10 ***
# EDU_DEGREE_ABBRProfessional               -2.794e-01  1.705e-02  -16.387  < 2e-16 ***
# poly(STANDARDIZED_AGE, 2)1                 3.698e+01  8.216e-01   45.012  < 2e-16 ***
# poly(STANDARDIZED_AGE, 2)2                -3.719e+01  6.404e-01  -58.075  < 2e-16 ***
# poly(STANDARDIZED_TNFI, 2)1                2.641e+02  6.872e-01  384.350  < 2e-16 ***
# poly(STANDARDIZED_TNFI, 2)2               -2.103e+02  7.095e-01 -296.418  < 2e-16 ***
# URBAN_RURAL_FACTORRural                   -7.055e-02  3.389e-03  -20.817  < 2e-16 ***
# URBAN_RURAL_FACTORUnknown                 -8.820e-02  7.887e-03  -11.183  < 2e-16 ***
# MARITAL_STATUS_FACTORMarried               1.933e-01  3.572e-03   54.119  < 2e-16 ***
# MARITAL_STATUS_FACTORSeparated            -9.845e-02  7.295e-03  -13.496  < 2e-16 ***
# MARITAL_STATUS_FACTORDivorced              6.599e-02  5.364e-03   12.302  < 2e-16 ***
# MARITAL_STATUS_FACTORWidowed               6.495e-02  1.811e-02    3.585 0.000337 ***
# MARITAL_STATUS_FACTORUnknown              -4.713e-03  1.335e-01   -0.035 0.971834
# SAMPLE_SEX_FACTORMALE                      6.431e-02  2.758e-03   23.313  < 2e-16 ***
# HOURS_WORKED_PER_WEEK_                     5.854e-04  4.275e-05   13.694  < 2e-16 ***
# SAMPLE_RACE_FACTORHISPANIC                 9.479e-02  4.270e-03   22.200  < 2e-16 ***
# SAMPLE_RACE_FACTORNON-BLACK, NON-HISPANIC  1.561e-01  3.412e-03   45.764  < 2e-16 ***
# FAMSIZE                                    5.298e-02  8.180e-04   64.770  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 0.5968 on 197502 degrees of freedom
# Multiple R-squared:  0.696,	Adjusted R-squared:  0.6959
# F-statistic: 1.884e+04 on 24 and 197502 DF,  p-value: < 2.2e-16

summary_model_4 <- summary(model_4)
# Accessing the coefficients for the factor levels of EDU_DEGREE_ABBR Since
# EDU_DEGREE_ABBR is a factor, use regular expression to match all levels of
# EDU_DEGREE_ABBR
factor_levels <- grep("EDU_DEGREE_ABBR",
                      rownames(summary_model_4$coefficients),
                      value = TRUE)
# Calculate t-statistics for all levels of the factor
t_statistics <- summary_model_4$coefficients[factor_levels, "Estimate"] /
    summary_model_4$coefficients[factor_levels, "Std. Error"]
# Output the t-statistics
t_statistics
# EDU_DEGREE_ABBRBA           EDU_DEGREE_ABBRBS           EDU_DEGREE_ABBRHS
#  -1.6328528                   -0.2942022                  -12.9871242
# EDU_DEGREE_ABBRMasters         EDU_DEGREE_ABBRNone        EDU_DEGREE_ABBROther
#  -2.3790418                  -52.1851371                  -38.9369376
# EDU_DEGREE_ABBRPhD EDU_DEGREE_ABBRProfessional
#  -6.4474767                  -16.3870056

vif(model_4)
#                               GVIF   Df   GVIF^(1/(2*Df))
# EDU_DEGREE_ABBR            1.344986  8        1.018697
# poly(STANDARDIZED_AGE, 2)  2.155847  2        1.211726
# poly(STANDARDIZED_TNFI, 2) 1.742166  2        1.148874
# URBAN_RURAL_FACTOR         1.157911  2        1.037334
# MARITAL_STATUS_FACTOR      1.804483  5        1.060804
# SAMPLE_SEX_FACTOR          1.054421  1        1.026850
# HOURS_WORKED_PER_WEEK_     1.104163  1        1.050792
# SAMPLE_RACE_FACTOR         1.222539  2        1.051515
# FAMSIZE                    1.223297  1        1.106028

bp_test <- bptest(model_4)
print(bp_test)
# studentized Breusch-Pagan test
# data:  model_4
# BP = 7896.9, df = 24, p-value < 2.2e-16

# Cook's distance
plot(model_4, which = 4)
abline(h = 4/length(fitted(model_4)), col = "red")

plot(fitted(model_4), residuals(model_4),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(residuals(model_4))
qqline(residuals(model_4), col = "red", lwd = 2)
