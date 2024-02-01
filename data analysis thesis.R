# Prerequisites & Data Loading --------------------------------------------
# install.packages("semPlot", dependencies = TRUE)
# install.packages("aod", dependencies = TRUE)
#install.packages("wesanderson")


library(lavaan)
library(semPlot)
library(tidyverse)
library(aod)
library(dplyr)
library(wesanderson)

####REMOVE ME!
data_thesis <- read.csv("JADS/Thesis/data/thesis_data_v2.csv")
data_thesis$prolific_id <- NULL
####
write.csv(data_thesis, "JADS/Thesis/data/elise_long_thesis_data_demographics.csv")

data_thesis <- read.csv("thesis_data.csv")




# Preliminary Visualizations ----------------------------------------------

# Plot the mitigations, differences using average scores
long_data <- data_thesis %>%
  pivot_longer(
    cols = c(Diversity_avg, Quality_avg, Utility_avg),
    names_to = "variable",
    values_to = "value"
  )

# Create the plot
ggplot(long_data, aes(
  y = variable,
  x = value,
  fill = factor(
    mitigation,
    levels = c(3, 2, 1),
    labels = c("Mitigation 2", "Mitigation 1", "Baseline")
  )
)) +
  #geom_violin() +
  geom_boxplot(alpha = 0.5) +
  theme_minimal() +
  scale_fill_grey(
    breaks = c("Baseline", "Mitigation 1", "Mitigation 2"),
    start = 0.4,
    end = 0.7
  ) +
  labs(title = "Comparison of Variables under Different Conditions",
       x = "Variable",
       y = "Value",
       fill = "Condition")


# Plot the occupations, differences using average scores
long_data <- data_thesis %>%
  pivot_longer(
    cols = c(Diversity_avg, Quality_avg, Utility_avg),
    names_to = "variable",
    values_to = "value"
  )

# Create the plot
ggplot(long_data, aes(
  y = variable,
  x = value,
  fill = factor(
    occupation,
    levels = c("co", "pr", "ca", "na", "el", "dh"),
    labels = c(
      "Cook (Both/PC)",
      "Producer (Both/W)",
      "Security Guard (M/PC)",
      "Nursing Assistant (F/PC)",
      "Electrician (M/W)",
      "Dental hygienist (F/W)"
    )
  )
)) +
  #geom_violin() +
  geom_boxplot(alpha = 0.5) +
  theme_minimal() +
  scale_fill_grey(
    breaks = c(
      "Dental hygienist (F/W)",
      "Electrician (M/W)",
      "Nursing Assistant (F/PC)",
      "Security Guard (M/PC)",
      "Producer (Both/W)",
      "Cook (Both/PC)"
    ),
    start = 0.2,
    end = 0.9
  ) +
  labs(title = "Comparison of Variables for different occupations",
       x = "Variable",
       y = "Value",
       fill = "Condition")

# Create the same occupations plot, but for different mitigation conditions
# Baseline (1)
long_data_1 <- long_data %>%
  filter(mitigation == 1)

# Create the plot
ggplot(long_data_1, aes(
  y = variable,
  x = value,
  fill = factor(
    occupation,
    levels = c("co", "pr", "ca", "na", "el", "dh"),
    labels = c(
      "Cook (Both/PC)",
      "Producer (Both/W)",
      "Security Guard (M/PC)",
      "Nursing Assistant (F/PC)",
      "Electrician (M/W)",
      "Dental hygienist (F/W)"
    )
  )
)) +
  #geom_violin() +
  geom_boxplot(alpha = 0.5) +
  theme_minimal() +
  scale_fill_grey(
    breaks = c(
      "Dental hygienist (F/W)",
      "Electrician (M/W)",
      "Nursing Assistant (F/PC)",
      "Security Guard (M/PC)",
      "Producer (Both/W)",
      "Cook (Both/PC)"
    ),
    start = 0.2,
    end = 0.9
  ) +
  labs(title = "Comparison of Variables for different occupations for the baseline situation",
       x = "Variable",
       y = "Value",
       fill = "Condition")

# Equal mitigation (2)
long_data_2 <- long_data %>%
  filter(mitigation == 2)

# Create the plot
ggplot(long_data_2, aes(
  y = variable,
  x = value,
  fill = factor(
    occupation,
    levels = c("co", "pr", "ca", "na", "el", "dh"),
    labels = c(
      "Cook (Both/PC)",
      "Producer (Both/W)",
      "Security Guard (M/PC)",
      "Nursing Assistant (F/PC)",
      "Electrician (M/W)",
      "Dental hygienist (F/W)"
    )
  )
)) +
  #geom_violin() +
  geom_boxplot(alpha = 0.5) +
  theme_minimal() +
  scale_fill_grey(
    breaks = c(
      "Dental hygienist (F/W)",
      "Electrician (M/W)",
      "Nursing Assistant (F/PC)",
      "Security Guard (M/PC)",
      "Producer (Both/W)",
      "Cook (Both/PC)"
    ),
    start = 0.2,
    end = 0.9
  ) +
  labs(title = "Comparison of Variables for different occupations for the Mitigation 1 situation",
       x = "Variable",
       y = "Value",
       fill = "Condition")

# inverse mitigation (3)
long_data_3 <- long_data %>%
  filter(mitigation == 3)

# Create the plot
ggplot(long_data_3, aes(
  y = variable,
  x = value,
  fill = factor(
    occupation,
    levels = c("co", "pr", "ca", "na", "el", "dh"),
    labels = c(
      "Cook (Both/PC)",
      "Producer (Both/W)",
      "Security Guard (M/PC)",
      "Nursing Assistant (F/PC)",
      "Electrician (M/W)",
      "Dental hygienist (F/W)"
    )
  )
)) +
  #geom_violin() +
  geom_boxplot(alpha = 0.5) +
  theme_minimal() +
  scale_fill_grey(
    breaks = c(
      "Dental hygienist (F/W)",
      "Electrician (M/W)",
      "Nursing Assistant (F/PC)",
      "Security Guard (M/PC)",
      "Producer (Both/W)",
      "Cook (Both/PC)"
    ),
    start = 0.2,
    end = 0.9
  ) +
  labs(title = "Comparison of Variables for different occupations for the Mitigation 2 situation",
       x = "Variable",
       y = "Value",
       fill = "Condition")

# Plot some more average scores 
long_data <- data_thesis %>%
  pivot_longer(
    cols = c(Diversity_avg, Quality_avg, Utility_avg),
    names_to = "variable",
    values_to = "value"
  )

# Plot average scores per gender bias
ggplot(long_data, aes(
  y = variable,
  x = value,
  fill = factor(gender_stereotype, levels = c("both", "female", "male"))
)) +
  
  geom_boxplot(alpha = 0.5) +
  #geom_violin(alpha = 0.1, ) +
  theme_minimal() +
  scale_fill_grey(
    breaks = c("male", "female", "both"),
    start = 0.2,
    end = 0.9
  ) +
  labs(title = "Comparison of Variables for different Gender Stereotypes",
       x = "Variable",
       y = "Value",
       fill = "Condition")

# Plot per racial bias
ggplot(long_data, aes(
  y = variable,
  x = value,
  fill = factor(skin_stereotype, levels = c("POC", "white"))
)) +
  # geom_violin() +
  geom_boxplot(alpha = 0.5) +
  theme_minimal() +
  scale_fill_grey(breaks = c("white", "POC"),
                  start = 0.2,
                  end = 0.5) +
  labs(title = "Comparison of Variables for different Skin colour Stereotypes",
       x = "Variable",
       y = "Value",
       fill = "Condition")



# Optional dataset creation for exploration between mitigations/trials -------------
# # Create three new datasets per mitigation
# data_thesis <- data_thesis[data_thesis$mitigation == '1', ]
# data_thesis <- data_thesis[data_thesis$mitigation == '2', ]
# data_thesis <- data_thesis[data_thesis$mitigation == '3', ]
# 
# # Create 3 new datasets per time order
# data_thesis <- data_thesis[!is.na(data_thesis$time), ]
# data_thesis <- data_thesis[data_thesis$time == '1', ]
# data_thesis <- data_thesis[data_thesis$time == '2', ]
# data_thesis <- data_thesis[data_thesis$time == '3', ]



# Removal of too short completion times -----------------------------------
unique_data = data_thesis[!duplicated(data_thesis$participant),]

mean(as.numeric(unique_data$Time.taken))/60
median(as.numeric(unique_data$Time.taken))/60
sd(as.numeric(unique_data$Time.taken))/60
min((as.numeric(unique_data$Time.taken)))/60
max((as.numeric(unique_data$Time.taken)))/60

# Arbitrary cutoff point of 2.5th percentile, four bottom times
hist(as.numeric(log(unique_data$Time.taken)),breaks = 20 )

times = as.numeric(log(unique_data$Time.taken))
threshold <- quantile(times, 0.025)
length(times[times <= threshold]) # 4 participants

sd(log(as.numeric(unique_data$Time.taken)))
mean(log(as.numeric(unique_data$Time.taken)))
(mean(log(as.numeric(unique_data$Time.taken))) - min((log(as.numeric(unique_data$Time.taken)))))/sd(log(as.numeric(unique_data$Time.taken)))
max((log(as.numeric(unique_data$Time.taken))))

qqnorm(times)
qqline(times, col = "red")

# The following times can be dropped
sorted_data <- unique_data[order(unique_data$Time.taken), ]
head(sorted_data$Time.taken, 10)/60
bottom_participants <- head(sorted_data$participant, 4)

data_thesis <- data_thesis[!data_thesis$participant %in% bottom_participants, ]

summary(data_thesis)


# Descriptives participants  -------------------------------------
unique_data = data_thesis[!duplicated(data_thesis$participant),]

mean(na.omit(as.numeric(unique_data$Age)), na.rm = TRUE)
sd(na.omit(as.numeric(unique_data$Age)), na.rm = TRUE)
min(na.omit(as.numeric(unique_data$Age)), na.rm = TRUE)
max(na.omit(as.numeric(unique_data$Age)), na.rm = TRUE)

table(unique_data$Sex)
table(unique_data$Gender)
table(unique_data$Ethnicity.simplified)

# group_by(unique_data$Age)
count(unique_data, Sex, Gender)
count(unique_data, Ethnicity.simplified, Ethnicity)
count(unique_data, Ethnicity.simplified, Sex)

table(unique_data$Randomgroup)
count(data_thesis, Randomgroup, occupation)
count(data_thesis, Randomgroup, Randomsubgroup)
count(data_thesis, mitigation, occupation)

mean(data_thesis$Utility_avg)
mean(data_thesis$Quality_avg)
mean(data_thesis$Surprise_avg)
mean(data_thesis$Diversity_avg)



# Additional variables creation -------------------------------------------
# Note: Here decisions were made to select baselines for mitigation, demographics, stereotypes, and occupations

# Creating participant dummies: POC, Female
data_thesis$poc_participant_dummy <- as.numeric(data_thesis$Ethnicity.simplified == "White")
data_thesis$female_participant_dummy <- as.numeric(data_thesis$Sex == "Female")
data_thesis$order_dummy <- as.numeric(data_thesis$time == "1")
data_thesis$non_binary <- as.numeric(data_thesis$Gender == "Non-binary (would like to give more detail)")

# Creating condition dummies: Mitigation, POC, gender, professions
data_thesis$mitigation <- factor(data_thesis$mitigation)
data_thesis$time <- factor(data_thesis$time)

data_thesis$poc_dummy <- as.numeric(data_thesis$skin_stereotype == "POC")

mitigation_dummies <- model.matrix(~ mitigation - 1, data = data_thesis)
gender_dummies <- model.matrix(~ gender_stereotype - 1, data = data_thesis)
occupation_dummies <- model.matrix(~ occupation -1, data = data_thesis)

data_thesis <- cbind(data_thesis, subset(mitigation_dummies, select =-mitigation2))
data_thesis <- cbind(data_thesis, subset(gender_dummies, select =-gender_stereotypeboth))
data_thesis <- cbind(data_thesis, subset(occupation_dummies, select =-occupationco))


# Interaction term creation -----------------------------------------------
# Interaction between occupation and mitigation
data_thesis$int_ca_mit1 <- data_thesis$occupationca * data_thesis$mitigation1
data_thesis$int_dh_mit1 <- data_thesis$occupationdh * data_thesis$mitigation1
data_thesis$int_el_mit1 <- data_thesis$occupationel * data_thesis$mitigation1
data_thesis$int_na_mit1 <- data_thesis$occupationna * data_thesis$mitigation1
data_thesis$int_pr_mit1 <- data_thesis$occupationpr * data_thesis$mitigation1
data_thesis$int_ca_mit3 <- data_thesis$occupationca * data_thesis$mitigation3
data_thesis$int_dh_mit3 <- data_thesis$occupationdh * data_thesis$mitigation3
data_thesis$int_el_mit3 <- data_thesis$occupationel * data_thesis$mitigation3
data_thesis$int_na_mit3 <- data_thesis$occupationna * data_thesis$mitigation3
data_thesis$int_pr_mit3 <- data_thesis$occupationpr * data_thesis$mitigation3

# int between gender stereotype and mitigation
data_thesis$int_female_mit1 <- data_thesis$gender_stereotypefemale * data_thesis$mitigation1
data_thesis$int_male_mit1 <- data_thesis$gender_stereotypemale * data_thesis$mitigation1
data_thesis$int_female_mit3 <- data_thesis$gender_stereotypefemale * data_thesis$mitigation3
data_thesis$int_male_mit3 <- data_thesis$gender_stereotypemale * data_thesis$mitigation3

# int between racial stereotype and mitigation
data_thesis$int_POC_mit1 <- data_thesis$poc_dummy * data_thesis$mitigation1
data_thesis$int_POC_mit3 <- data_thesis$poc_dummy * data_thesis$mitigation3

# int between participant gender and mitigation
data_thesis$int_fpart_mit1 <- data_thesis$female_participant_dummy * data_thesis$mitigation1
data_thesis$int_fpart_mit3 <- data_thesis$female_participant_dummy * data_thesis$mitigation3

# int between particpant race and mitigation
data_thesis$int_Ppart_mit1 <- data_thesis$poc_participant_dummy * data_thesis$mitigation1
data_thesis$int_Ppart_mit3 <- data_thesis$poc_participant_dummy * data_thesis$mitigation3

# int between participant gender and stereotype
data_thesis$int_female_fpart <- data_thesis$female_participant_dummy * data_thesis$gender_stereotypefemale
data_thesis$int_male_fpart <- data_thesis$female_participant_dummy * data_thesis$gender_stereotypemale

# int between participant race and stereotype
data_thesis$int_POC_Ppart <- data_thesis$poc_dummy * data_thesis$poc_participant_dummy


data_thesis$poc_dummyc = data_thesis$poc_dummy - mean(data_thesis$poc_dummy)
data_thesis$poc_participant_dummyc = data_thesis$poc_participant_dummy - mean(data_thesis$poc_participant_dummy)

# int centreren
data_thesis$int_POC_Ppartc <- data_thesis$poc_dummyc * data_thesis$poc_participant_dummyc



# CFA modeling ------------------------------------------------------------
# Final selected CFA model
DT.model <- ' diversity  =~ NA*D01 + D02 + D03 + D04
              quality   =~ NA*Q01 +  Q03 + Q04
              utility   =~ NA*U02 + U03 + U04
              diversity ~~ 1*diversity
              quality ~~ 1*quality
              utility ~~ 1*utility'


fit_thesis <-
  cfa(
    DT.model,
    data = data_thesis,
    ordered = c(
      "D01",
      "D02",
      "D03",
      "D04",
      "Q01",
      "Q03",
      "Q04",
      "U02",
      "U03",
      "U04"
    )
  )

options("max.print" = 1500 )
summary(fit_thesis, modindices = TRUE, rsquare = TRUE)
library(semTools) 
reliability(fit_thesis) 


# Original, full CFA
DT.model <- ' diversity  =~ NA*D01 + D02 + D03 + D04
              quality   =~ NA*Q01 + Q02 + Q03 + Q04
              utility   =~ NA*U01 + U02 + U03 + U04
              surprise =~ NA*S01 + S02 + S03 + S04
              diversity ~~ 1*diversity
              quality ~~ 1*quality
              utility ~~ 1*utility
              surprise ~~ 1*surprise'

fit_thesis <-
  cfa(
    DT.model,
    data = data_thesis,
    ordered = c(
      "D01",
      "D02",
      "D03",
      "D04",
      "S01",
      "S02",
      "S03",
      "S04",
      "Q01",
      "Q02",
      "Q03",
      "Q04",
      "U01",
      "U02",
      "U03",
      "U04"
    )
  )

summary(fit_thesis, modindices = TRUE, rsquare=TRUE)



# CFA factor scores --------------------------------------------------
factor_scores <- as.data.frame(lavPredict(fit_thesis,type ="lv"))
summary(factor_scores)

# Factor score plots for Male stereotype, for different mitigations 
test1m = data_thesis[data_thesis$mitigation == '1'& data_thesis$gender_stereotype == 'male',]

# Silly workaround of an error of lavaan, if a certain factor score does not exist (e.g. one of 0,1,2,3,4 is missing), the prediction does not work. To check, I used the following manouvre: 
table(test1m$D01)
table(test1m$D02)
table(test1m$D03)
table(test1m$D04)
table(test1m$Q01)
table(test1m$Q03)
table(test1m$Q04)
table(test1m$U02)
table(test1m$U03) # Here we are missing a 4
table(test1m$U04)

duplicatedRow <- test1m[1, ] # Duplicate a row
duplicatedRow$U03 <- 4 # Insert a 4
test1m <- rbind(test1m, duplicatedRow) # Add this to the dataframe

# The following does not have missing values
test2m = data_thesis[data_thesis$mitigation == '2'& data_thesis$gender_stereotype == 'male',]

# This one does, similar analysis was performed, and D02 == 4, and D03 == 4 were missing
test3m = data_thesis[data_thesis$mitigation == '3'& data_thesis$gender_stereotype == 'male',]

duplicatedRow <- test3m[1, ]
duplicatedRow$D02 <- 4
test3m <- rbind(test3m, duplicatedRow)
duplicatedRow$D03 <- 4
test3m <- rbind(test3m, duplicatedRow)

# Get predictions for the new datasets, remove duplicate data
mit_1_fsm <- as.data.frame(lavPredict(fit_thesis, newdata = test1m, type="lv"))
mit_1_fsm <- mit_1_fsm[-(nrow(mit_1_fsm)): -nrow(mit_1_fsm), ]
mit_2_fsm <- as.data.frame(lavPredict(fit_thesis, newdata= test2m, type="lv"))
mit_3_fsm <- as.data.frame(lavPredict(fit_thesis, newdata= test3m, type="lv"))
mit_3_fsm <- mit_3_fsm[-(nrow(mit_3_fsm)-1): -nrow(mit_3_fsm), ]

# Factor score plots for Female stereotype, for different mitigations 
test1f = data_thesis[data_thesis$mitigation == '1'& data_thesis$gender_stereotype == 'female',]
test2f = data_thesis[data_thesis$mitigation == '2'& data_thesis$gender_stereotype == 'female',]
test3f = data_thesis[data_thesis$mitigation == '3'& data_thesis$gender_stereotype == 'female',]

# Get predictions for the new datasets
mit_1_fsf <- as.data.frame(lavPredict(fit_thesis, newdata = test1f, type="lv"))
mit_2_fsf <- as.data.frame(lavPredict(fit_thesis, newdata= test2f, type="lv"))
mit_3_fsf <- as.data.frame(lavPredict(fit_thesis, newdata= test3f, type="lv"))


# Factor score plots for Both stereotype, for different mitigations 
test1b = data_thesis[data_thesis$mitigation == '1'& data_thesis$gender_stereotype == 'both',]

test2b = data_thesis[data_thesis$mitigation == '2'& data_thesis$gender_stereotype == 'both',]


duplicatedRow <- test2b[1, ]
duplicatedRow$D01 <- 0
test2b <- rbind(test2b, duplicatedRow)
duplicatedRow$D03 <- 0
test2b <- rbind(test2b, duplicatedRow)
duplicatedRow$D04 <- 0
test2b <- rbind(test2b, duplicatedRow)
duplicatedRow$U02 <- 0
test2b <- rbind(test2b, duplicatedRow)
duplicatedRow$U04 <- 0
test2b <- rbind(test2b, duplicatedRow)

test3b = data_thesis[data_thesis$mitigation == '3'& data_thesis$gender_stereotype == 'both',]

# Get predictions for the new datasets, remove duplicate data
mit_1_fsb <- as.data.frame(lavPredict(fit_thesis, newdata = test1b, type="lv"))
mit_2_fsb <- as.data.frame(lavPredict(fit_thesis, newdata= test2b, type="lv"))
mit_2_fsb <- mit_2_fsb[-(nrow(mit_2_fsb)-4): -nrow(mit_2_fsb), ]
mit_3_fsb <- as.data.frame(lavPredict(fit_thesis, newdata= test3b, type="lv"))

# Make a summary of the factor scores for each mitigation, including the mean, se and ic
# Baseline (mit1)
factor_mit1_sum_dfm <- data.frame(
  column = names(mit_1_fsm),
  mean = sapply(mit_1_fsm, mean, na.rm = TRUE),
  se = sapply(mit_1_fsm, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_1_fsm, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

factor_mit1_sum_dff <- data.frame(
  column = names(mit_1_fsf),
  mean = sapply(mit_1_fsf, mean, na.rm = TRUE),
  se = sapply(mit_1_fsf, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_1_fsf, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

factor_mit1_sum_dfb <- data.frame(
  column = names(mit_1_fsb),
  mean = sapply(mit_1_fsb, mean, na.rm = TRUE),
  se = sapply(mit_1_fsb, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_1_fsb, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

# Equal mitigation (mit2)
factor_mit2_sum_dfm <- data.frame(
  column = names(mit_2_fsm),
  mean = sapply(mit_2_fsm, mean, na.rm = TRUE),
  se = sapply(mit_2_fsm, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_2_fsm, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

factor_mit2_sum_dff <- data.frame(
  column = names(mit_2_fsf),
  mean = sapply(mit_2_fsf, mean, na.rm = TRUE),
  se = sapply(mit_2_fsf, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_2_fsf, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

factor_mit2_sum_dfb <- data.frame(
  column = names(mit_2_fsb),
  mean = sapply(mit_2_fsb, mean, na.rm = TRUE),
  se = sapply(mit_2_fsb, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_2_fsb, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

# Inverse mitigation (mit3)
factor_mit3_sum_dfm <- data.frame(
  column = names(mit_3_fsm),
  mean = sapply(mit_3_fsm, mean, na.rm = TRUE),
  se = sapply(mit_3_fsm, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_3_fsm, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

factor_mit3_sum_dff <- data.frame(
  column = names(mit_3_fsf),
  mean = sapply(mit_3_fsf, mean, na.rm = TRUE),
  se = sapply(mit_3_fsf, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_3_fsf, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

factor_mit3_sum_dfb <- data.frame(
  column = names(mit_3_fsb),
  mean = sapply(mit_3_fsb, mean, na.rm = TRUE),
  se = sapply(mit_3_fsb, function(x)
    sd(x) / sqrt(length(x))),
  ic = sapply(mit_3_fsb, function(x)
    sd(x) / sqrt(length(x))) * 1.96
)

# Make one dataframe
factor_mit1_sum_dfm$mitigation <- "mit1"
factor_mit2_sum_dfm$mitigation <- "mit2"
factor_mit3_sum_dfm$mitigation <- "mit3"
factor_mit1_sum_dfm$gender_stereotype <- "male"
factor_mit2_sum_dfm$gender_stereotype <- "male"
factor_mit3_sum_dfm$gender_stereotype <- "male"
factor_mit1_sum_dff$mitigation <- "mit1"
factor_mit2_sum_dff$mitigation <- "mit2"
factor_mit3_sum_dff$mitigation <- "mit3"
factor_mit1_sum_dff$gender_stereotype <- "female"
factor_mit2_sum_dff$gender_stereotype <- "female"
factor_mit3_sum_dff$gender_stereotype <- "female"
factor_mit1_sum_dfb$mitigation <- "mit1"
factor_mit2_sum_dfb$mitigation <- "mit2"
factor_mit3_sum_dfb$mitigation <- "mit3"
factor_mit1_sum_dfb$gender_stereotype <- "both"
factor_mit2_sum_dfb$gender_stereotype <- "both"
factor_mit3_sum_dfb$gender_stereotype <- "both"

combined_df <- rbind(factor_mit1_sum_dfm, factor_mit2_sum_dfm, factor_mit3_sum_dfm, factor_mit1_sum_dff, factor_mit2_sum_dff, factor_mit3_sum_dff, factor_mit1_sum_dfb, factor_mit2_sum_dfb, factor_mit3_sum_dfb)



# Factor score plots ------------------------------------------------------
# Plots for every perceived system aspect 

ggplot(combined_df[combined_df$column == 'diversity', ],
       aes(x = mean, y = gender_stereotype, color = mitigation)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = mean - se, xmax = mean + se),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  theme_minimal() +
  coord_flip() +
  xlim(-1.1, 1.1) +
  labs(x = "Factor score", y = "Gender bias",  title = "Perceived diversity") +
  theme(legend.position = "bottom", text = element_text(size = 12)) +
  scale_color_manual(name = "Mitigation condition", 
                     labels = c("Baseline", "Equal Mitigation", "Inverse Mitigation"), 
                     values = wes_palette(n=3, name=9))+
  scale_y_discrete(labels = c("both" = "Both", "female" = "Female", "male" = "Male")) +
  theme(axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15), plot.title = element_text(hjust = 0.5, size = 18 ))

  
ggplot(combined_df[combined_df$column == 'quality', ],
       aes(x = mean, y = gender_stereotype, color = mitigation)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = mean - se, xmax = mean + se),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  theme_minimal() +
  coord_flip() +
  xlim(-1.1, 1.1) +
  labs(x = "Factor score", y = "Gender bias",  title = "Perceived quality") +
  theme(legend.position = "bottom", text = element_text(size = 12)) +
  scale_color_manual(name = "Mitigation condition", 
                     labels = c("Baseline", "Equal Mitigation", "Inverse Mitigation"), 
                     values = wes_palette(n=3, name=9))+
  scale_y_discrete(labels = c("both" = "Both", "female" = "Female", "male" = "Male")) +
  theme(axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15), plot.title = element_text(hjust = 0.5, size = 18 ))

ggplot(combined_df[combined_df$column == 'utility', ],
       aes(x = mean, y = gender_stereotype, color = mitigation)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = mean - se, xmax = mean + se),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  theme_minimal() +
  coord_flip() +
  xlim(-1.1, 1.1) +
  labs(x = "Factor score", y = "Gender bias",  title = "Perceived utility") +
  theme(legend.position = "bottom", text = element_text(size = 12)) +
  scale_color_manual(name = "Mitigation condition", 
                     labels = c("Baseline", "Equal Mitigation", "Inverse Mitigation"), 
                     values = wes_palette(n=3, name=9))+
  scale_y_discrete(labels = c("both" = "Both", "female" = "Female", "male" = "Male")) +
  theme(axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15), plot.title = element_text(hjust = 0.5, size = 18 ))


# Plots for every mitigation across mitigations gender stereotype
factor_scores <- as.data.frame(lavPredict(fit_thesis,type ="lv"))

# Attempt at getting different mitigations
mit_1_fs <- as.data.frame(lavPredict(fit_thesis, newdata= data_thesis[data_thesis$mitigation == '1',], type="lv"))
mit_2_fs <- as.data.frame(lavPredict(fit_thesis, newdata= data_thesis[data_thesis$mitigation == '2',], type="lv"))
mit_3_fs <- as.data.frame(lavPredict(fit_thesis, newdata= data_thesis[data_thesis$mitigation == '3',], type="lv"))

# mit1
factor_mit1_sum_df <- data.frame(
  column = names(mit_1_fs),
  mean = sapply(mit_1_fs, mean, na.rm = TRUE),
  #sd = sapply(mit_3_fs, sd, na.rm = TRUE),
  se = sapply(mit_1_fs, function(x)sd(x)/sqrt(length(x))),
  ic = sapply(mit_1_fs, function(x)sd(x)/sqrt(length(x))) * 1.96
  # ic = sapply(mit_1_fs, function(x)sd(x)/sqrt(length(x))) * qt((1-0.05)/2 + .5, length(mit_1_fs)-1)
)

# mit2
factor_mit2_sum_df <- data.frame(
  column = names(mit_2_fs),
  mean = sapply(mit_2_fs, mean, na.rm = TRUE),
  se = sapply(mit_2_fs, function(x)sd(x)/sqrt(length(x))),
  ic = sapply(mit_2_fs, function(x)sd(x)/sqrt(length(x))) * 1.96
)

# mit3
factor_mit3_sum_df <- data.frame(
  column = names(mit_3_fs),
  mean = sapply(mit_3_fs, mean, na.rm = TRUE),
  #sd = sapply(mit_3_fs, sd, na.rm = TRUE)
  se = sapply(mit_3_fs, function(x)sd(x)/sqrt(length(x))),
  ic = sapply(mit_3_fs, function(x)sd(x)/sqrt(length(x))) * 1.96
)

# Make one df
factor_mit1_sum_df$mitigation <- "mit1"
factor_mit2_sum_df$mitigation <- "mit2"
factor_mit3_sum_df$mitigation <- "mit3"

combined_df <- rbind(factor_mit1_sum_df, factor_mit2_sum_df, factor_mit3_sum_df)

ggplot(combined_df, aes(x = mean, y = column, color = mitigation)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = mean - se, xmax = mean + se), 
                width = 0.3, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  coord_flip() +
  xlim(-0.5, 0.5) +
  labs(x = "Factor scores", y = "System aspect", title = "Mean factor scores") +
  theme(legend.position = "bottom") + 
  scale_color_manual(name = "Mitigation condition", 
                   labels = c("Baseline", "Equal Mitigation", "Inverse Mitigation"), 
                   values = wes_palette(n=3, name=9))+
  scale_y_discrete(labels = c("diversity" = "Perceived diversity", "quality" = "Perceived quality", "utility" = "Perceived utility")) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 18 ))



# SEM modeling ------------------------------------------------------------
# Complete model, with all posible factors and variables included 
model <- 'diversity  =~ NA*D01 + D02 + D03 + D04
          quality   =~ NA*Q01 + Q02 +  Q03 + Q04
          surprise =~  NA*S01 + S02 + S03 + S04 
          utility   =~ NA*U01 + U02 + U03 + U04 
          diversity ~~ 1*diversity
          quality ~~ 1*quality
          utility ~~ 1*utility
          surprise ~~ 1*surprise

# Note again that here "Both" and "White" are baselines
          utility ~ diversity + quality + surprise + mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1 + int_female_mit3 + int_male_mit1 + int_male_mit3
+ int_POC_mit1 + int_POC_mit3 + int_POC_Ppart + int_male_fpart + int_female_fpart + int_Ppart_mit3 + int_Ppart_mit1 + int_fpart_mit3 + int_fpart_mit1

          quality ~ diversity + surprise +  mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale  + int_female_mit1 + int_female_mit3 + int_male_mit1 + int_male_mit3 + int_POC_mit1 + int_POC_mit3 + int_POC_Ppart + int_male_fpart + int_female_fpart + int_Ppart_mit3 + int_Ppart_mit1 + int_fpart_mit3 + int_fpart_mit1
          
          diversity ~ surprise + mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1 + int_female_mit3 + int_male_mit1 + int_male_mit3 + int_POC_Ppart + int_male_fpart + int_female_fpart + int_Ppart_mit3 + int_Ppart_mit1 + int_fpart_mit3 + int_fpart_mit1

          surprise ~ mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1 + int_female_mit3 + int_male_mit1 + int_male_mit3
+ int_POC_mit1 + int_POC_mit3 + int_POC_Ppart + int_male_fpart + int_female_fpart + int_Ppart_mit3 + int_Ppart_mit1 + int_fpart_mit3 + int_fpart_mit1';

fit <- sem(model, data = data_thesis, ordered = c("D01", "D02",
                                                  "D03","D04",
                                                   "S01", "S02",
                                                  "S03", "S04",
                                                  "Q01", "Q02", 
                                                  "Q03", "Q04",
                                                  "U01", "U02", 
                                                  "U03", "U04"))


summary(fit, fit.measures=TRUE,modindices=TRUE, rsquare=TRUE);

# Final SEM model, no S04, no participant demographics
model <- 'diversity  =~ NA*D01 + D02 + D03 + D04
          quality   =~ NA*Q01 +  Q03 + Q04
          utility   =~ NA*U02 + U03 + U04 
          diversity ~~ 1*diversity
          quality ~~ 1*quality
          utility ~~ 1*utility

          utility ~ diversity + quality

          diversity ~   mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1+ int_male_mit1 + int_female_mit3 + int_male_mit3
          
          quality ~ diversity + mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1+ int_male_mit1 + int_female_mit3 + int_male_mit3'

fit <- sem(model, data = data_thesis, ordered = c("D01", "D02",
                                                  "D03","D04",
                                                  "Q01", 
                                                  "Q03", "Q04",
                                                  "U02",
                                                  "U03", "U04")) 

summary(fit, fit.measures=TRUE,modindices=TRUE, rsquare=TRUE);

# Final SEM model, with demographic factors, without S04
model <- 'diversity  =~ NA*D01 + D02 + D03 + D04
          quality   =~ NA*Q01 +  Q03 + Q04
          utility   =~ NA*U02 + U03 + U04 
          diversity ~~ 1*diversity
          quality ~~ 1*quality
          utility ~~ 1*utility

          utility ~ diversity + quality  + poc_participant_dummy + poc_dummy #int_POC_Ppart

          diversity ~   mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1  + int_male_mit1 + int_male_mit3 + int_female_mit3 
          
          quality ~ diversity +  mitigation1 + mitigation3 + gender_stereotypefemale + gender_stereotypemale + poc_dummy + int_female_mit1  + int_male_mit1 + int_female_mit3  + int_male_mit3 + Age #+ non_binary;'


# Final SEM model, without demographic factors, with S04
model <- 'diversity  =~ NA*D01 + D02 + D03 + D04
          quality   =~ NA*Q01 +  Q03 + Q04
          utility   =~ NA*U02 + U03 + U04 
          diversity ~~ 1*diversity
          quality ~~ 1*quality
          utility ~~ 1*utility

          utility ~ diversity + quality + S04

          diversity ~   mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1  + int_male_mit1 + int_male_mit3 + int_female_mit3 
          
          S04 ~ diversity + mitigation1 + mitigation3 + poc_dummy + gender_stereotypemale + gender_stereotypefemale + int_female_mit1  + int_male_mit1 + int_male_mit3 + int_female_mit3 
          
          quality ~ diversity + S04  + mitigation1 + mitigation3 + gender_stereotypefemale + gender_stereotypemale + poc_dummy + int_female_mit1  + int_male_mit1 + int_female_mit3  + int_male_mit3;'


fit <- sem(model, data = data_thesis, ordered = c("D01", "D02",
                                                  "D03","D04",
                                                  "Q01", 
                                                  "Q03", "Q04",
                                                  "U02",
                                                  "U03", "U04")) 

# Final SEM model, with demographic factors and S04
#data_thesis$poc_dummyc = data_thesis$poc_dummy - mean(data_thesis$poc_dummy)

model <- 'diversity  =~ NA*D01 + D02 + D03 + D04
          quality   =~ NA*Q01 +  Q03 + Q04
          utility   =~ NA*U02 + U03 + U04 
          diversity ~~ 1*diversity
          quality ~~ 1*quality
          utility ~~ 1*utility

          utility ~ diversity + quality + S04 + poc_participant_dummyc #+ int_POC_Ppartc + poc_dummyc#+ poc_participant_dummy #+ int_POC_Ppart #+ poc_participant_dummy + int_POC_Ppart
          # 5) Age + non-binary 6) + int_male_fpart + int_female_fpart 7) + female_participant_dummy 8) + int_POC_Ppart

          diversity ~   mitigation1 + mitigation3 + poc_dummyc + gender_stereotypemale + gender_stereotypefemale + int_female_mit1  + int_male_mit1 + int_male_mit3 + int_female_mit3 
          # 2) + poc_participant_dummy + int_POC_Ppart 3) Age + non-binary 4) + female_participant_dummy + int_male_fpart + int_female_fpart 
          
          S04 ~ diversity + mitigation1 + mitigation3  + gender_stereotypemale + gender_stereotypefemale + int_female_mit1  + int_male_mit1 + int_male_mit3 + int_female_mit3   + poc_dummyc + poc_participant_dummyc + int_POC_Ppartc
          # 3) Age + non-binary 4)   + female_participant_dummy + int_POC_Ppart + int_male_fpart + int_female_fpart  
          
          quality ~ diversity + S04  + mitigation1 + mitigation3 + gender_stereotypefemale + gender_stereotypemale + poc_dummyc + int_female_mit1  + int_male_mit1 + int_female_mit3  + int_male_mit3  + non_binary + Age  ;' # 1) poc_participant_dummy + int_POC_Ppart 4) + female_participant_dummy + int_male_fpart + int_female_fpart


# + poc_participant_dummy + non_binary + Age + female_participant_dummy + int_POC_Ppart + int_male_fpart + int_female_fpart


fit <- sem(model, data = data_thesis, ordered = c("D01", "D02",
                                                  "D03","D04",
                                                  "Q01", 
                                                  "Q03", "Q04",
                                                  "U02",
                                                  "U03", "U04")) 



#semPaths(fit, title = FALSE, curvePivot = TRUE)

summary(fit, fit.measures=TRUE,modindices=TRUE, rsquare=TRUE);




