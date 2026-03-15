#load the data
install.packages("writexl")
rm(list=ls(all=TRUE))
library(data.table)
load("nlsy97.rdata")
attach(nlsy97)
install.packages("dplyr")
library(dplyr)

#data cleaning,create a new variable BMI based on height and weight
df1 <- subset(nlsy97, wage > 0)
df1 <- subset(df1, weight2017 > 0)
df1 <- subset(df1, whours > 1.5)
df1 <- subset(df1, tenure2017 > 0)
df1 <- subset(df1, yeduc > 0)
df1 <- subset(df1, health2017 > 0)
df1 <- subset(df1, marstatus > -0.1)
df1$BMI <- df1$weight2017 / ((df1$height2002 / 100)^2)
df1$tenure_2 <- df1$tenure2017 ^2
df1 <- subset(df1, BMI > 10 & BMI < 50)
df1$log_wage <- log(df1$wage)
summary(df1)

z_scores <- scale(df1)  # compute z-scores
df1 <- df1[
  apply(abs(z_scores) < 3, 1, all), ]

#create 4 BMI categories
df1$underweight <- ifelse(df1$BMI < 20, 1, 0)
df1$healthy     <- ifelse(df1$BMI >= 20 & df1$BMI < 25, 1, 0)
df1$overweight  <- ifelse(df1$BMI >= 25 & df1$BMI < 30, 1, 0)
df1$obese       <- ifelse(df1$BMI >= 30, 1, 0)
summary(df1)
#model 1:variables of interest:BMI(underweight,healthy,overweight,obese),gender(male,female)
model1 <- lm(log_wage ~ underweight + overweight +obese+ male , data = df1)
summary(model1)
#model 2:add the variables according to the human capital model
model2 <- lm(log_wage ~ underweight + overweight +obese+ male + yeduc + whours + tenure2017+tenure_2, data = df1)
summary(model2)
#model 3:exclude underweight because the amount of people who is underweight is relatively small,also add health condition
# three categories of health conditions
df1 <- df1 %>%
  mutate(
    high_health = as.numeric(health2017 %in% 1:2),  # excellent or very good health
    medium_health = as.numeric(health2017 ==3),   #good health
    low_health = as.numeric(health2017  %in% 4:5)   #fair or poor health
  )
model3 <- lm(log_wage ~  overweight +obese+ male ++high_health+low_health+ yeduc + whours + tenure2017+tenure_2, data = df1)
summary(model3)

#model 4: add the more "social" control variables:race and ethnicity,marital status,industry
#create3 categories for marital status:1.single  2.married/cohabiting   3.separated
df1 <- df1 %>%
  mutate(
    single = as.numeric(marstatus == 0),  # never married,not cohabiting
    married_cohabiting = as.numeric(marstatus %in% 1:2),#never married,cohabiting/married
    separated = as.numeric(marstatus %in% 3:5)   #legally separated/divorced/widowed
  )
model4 <- lm(log_wage ~  overweight +obese+ male + 
               yeduc + whours + tenure2017+tenure_2+
               black+hispanic+ married_cohabiting+separated+high_health+low_health, data = df1)
summary(model4)
#model 5:interaction terms:BMI group*gender
df1$underweight_female <- df1$underweight * df1$female
df1$healthy_female     <- df1$healthy * df1$female
df1$overweight_female  <- df1$overweight * df1$female
df1$obese_female       <- df1$obese * df1$female
df1$underweight_male        <- df1$underweight * df1$male  
df1$healthy_male       <- df1$healthy * df1$male
df1$overweight_male    <- df1$overweight * df1$male
df1$obese_male         <- df1$obese * df1$male

model5 <- lm(log_wage ~  female +overweight+ obese + overweight_female+obese_female
             +yeduc + whours + tenure2017+tenure_2+ black+hispanic+ married_cohabiting+separated
             +high_health+low_health, data = df1)
summary(model5)
#model 6:obesity and years of schooling
summary(lm(yeduc ~ obese, data = df1))
summary(lm(tenure2017 ~ obese, data = df1))
model6<- lm(log_wage ~  obese+ male + yeduc + whours + tenure2017+tenure_2+
              black+hispanic+ married_cohabiting+separated+high_health+low_health, data = df1)
model6_1 <- lm(log_wage ~ obese * yeduc+obese+ male + yeduc + whours + tenure2017+tenure_2+
                 black+hispanic+ married_cohabiting+separated+high_health+low_health, data = df1)
model6_2 <- lm(log_wage ~ obese * tenure2017 +obese+ male + yeduc + whours + tenure2017+tenure_2+
                 black+hispanic+ married_cohabiting+separated+high_health+low_health, data = df1)
summary(model6)
summary(model6_1)
summary(model6_2)
#model 7:analyze the interaction effect (obese*industry)(with the social control variables)
df2 <- df1[!is.na(df1$industrycode) & df1$industrycode > 0, ]
df2$industrycode <- as.integer(df2$industrycode)
df2$service_industry <- with(df2, ifelse(
  (industrycode >= 4670 & industrycode <= 5790) |   # Retail trade
    (industrycode >= 6470 & industrycode <= 6780) |   # Information and communication
    (industrycode >= 6870 & industrycode <= 7190) |   # Finance, insurance, real estate
    (industrycode >= 7270 & industrycode <= 7790) |   # Professional services
    (industrycode >= 7860 & industrycode <= 8470) |   # Educational/health/social
    (industrycode >= 8560 & industrycode <= 8690) |   # Entertainment/food services
    (industrycode == 5890) |                         # Arts/recreation
    (industrycode >= 8770 & industrycode <= 9290) |   # Other services
    (industrycode >= 9370 & industrycode <= 9590),    # Public administration
  1, 0
))

df2$obese_service <- df2$obese * df2$service_industry
model7 <- lm(log_wage ~  female + obese +service_industry +obese_service
             +yeduc + whours + tenure2017+tenure_2+
               black+hispanic+ married_cohabiting+separated+high_health+low_health, data = df2)
summary(model7)
#a data set that only include variables that we analyze
df_clean = df1[, c("log_wage", "underweight", "overweight", "obese", "male", "yeduc", "whours", 
                   "tenure2017", "tenure_2", "overweight_female", "obese_female", "black", 
                   "hispanic", "married_cohabiting", "separated", "high_health", "low_health")]
result <- data.frame(
  Column = names(df_clean),
  Mean = sapply(df_clean, mean),
  SD = sapply(df_clean, sd),
  Correlation = sapply(df_clean, function(x) cor(x, df_clean[["log_wage"]], use = "complete.obs")),
  Min_Max = sapply(df_clean, function(x) {
    rng <- range(x, na.rm = TRUE)
    sprintf("(%.2f, %.2f)", rng[1], rng[2])
  })
)

# Set correlation of the target column with itself to NA
result$Correlation[result$Column == "log_wage"] <- NA
print(result)

# Load the package
library(writexl)
write_xlsx(result, "result.xlsx")


models <- list(model1, model2, model3, model4, model5) 

coef_matrix <- lapply(models, function(mod) coef(summary(mod)))

all_vars <- unique(unlist(lapply(coef_matrix, rownames)))

result <- matrix("", nrow = length(all_vars) * 2, ncol = length(models))
rownames(result) <- as.vector(rbind(all_vars, paste0(all_vars, "_SE")))
colnames(result) <- paste0("Model_", seq_along(models))

for (i in seq_along(models)) {
  model_summary <- coef_matrix[[i]]
  for (var in rownames(model_summary)) {
    row_coef <- which(rownames(result) == var)
    row_se <- which(rownames(result) == paste0(var, "_SE"))
    
    coef_val <- model_summary[var, "Estimate"]
    se_val <- model_summary[var, "Std. Error"]
    p_val <- model_summary[var, "Pr(>|t|)"]
    
    stars <- if (p_val < 0.001) {
      "***"
    } else if (p_val < 0.05) {
      "**"
    } else if (p_val < 0.1) {
      "*"
    } else {
      ""
    }
    
    # Format values
    result[row_coef, i] <- paste0(format(round(coef_val, 3), nsmall = 3), stars)
    result[row_se, i] <- paste0("(", format(round(se_val, 3), nsmall = 3), ")")
  }
}

result_df <- as.data.frame(result)

result_df <- cbind(Variable = rownames(result), result_df)
rownames(result_df) <- NULL

print(result_df)
write_xlsx(result_df, "reg_result.xlsx")
# Identify all dummy columns (i.e., 0/1 variables)
dummy_cols <- names(df1)[sapply(df1, function(x) all(na.omit(unique(x)) %in% c(0, 1)))]

# For each dummy group, calculate wage stats where dummy == 1
group_stats <- lapply(dummy_cols, function(col) {
  group_data <- df1[df1[[col]] == 1, "wage"]
  mean_wage <- mean(group_data, na.rm = TRUE)
  sd_wage <- sd(group_data, na.rm = TRUE)
  data.frame(
    Group = col,
    Mean_Wage = round(mean_wage, 2),
    SD_Wage = round(sd_wage, 2)
  )
})

# Combine into one data frame
group_stats_df <- do.call(rbind, group_stats)

# Print the result
print(group_stats_df)

# Save to Excel
write_xlsx(group_stats_df, "wage_stats_by_dummy_groups.xlsx")
