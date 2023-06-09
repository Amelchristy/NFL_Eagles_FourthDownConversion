#Load all libraries needed
library(tidyverse)
library(ggimage)
library(gt)
library(caret)
library(ggplot2)
library(lattice)
library(nflfastR)
     
pbp <- load_pbp(1999:2022)
nrow(pbp)
pbp
nrow(pbp)
#filtering eagles data so it's only Philly eagles and the downs are 4
eagles <- pbp |>
  filter((posteam == "PHI" | defteam == "PHI") & down == "4" & (play_type == "pass" | play_type == "run"))
eagles
nrow(eagles)
colnames(eagles)
# Select the relevant columns
eagles_select<- eagles[, c("qtr", "ydstogo", "yardline_100",
                                   "game_seconds_remaining", "posteam_score",
                                   "defteam_score",
                                   "play_type","fourth_down_converted")]


#remove all the nas rows 
eagles_clean<- na.omit(eagles_select)

eagles_select
eagles_clean
nrow(eagles_clean)


#partition the data 
n<-nrow(eagles_clean)
n1 <- floor(0.6*n)

set.seed(12345)
train_index<- sample(1:n,n1) 

eagles_train <-eagles_clean[train_index,]
eagles_valid <-eagles_clean[-train_index,]

#training the train dataaaaa

eagles.lm <- glm(eagles_train$fourth_down_converted ~., data=eagles_train, family='binomial')
summary(eagles.lm)

#forward
forward_jawn <- step(glm(fourth_down_converted ~ 1, data = eagles_train, family = "binomial"),
                                             scope = list(upper = eagles.lm),
                                             direction = "forward", trace = TRUE)

summary(forward_jawn)

#both
stepwise_jawn <- step(glm(fourth_down_converted ~ 1, data = eagles_train, family = "binomial"),
                              scope = list(upper = eagles.lm),
                              direction = "both", trace = TRUE)

summary(stepwise_jawn)

#back
backward_jawn <- step(glm(fourth_down_converted ~ 1, data = eagles_train, family = "binomial"),
                            direction = "backward", trace = TRUE)

summary(backward_jawn)


#tryna predict - glm predict,,,,,, right? Whats a non glm predict and whats one. oh is this caret? yup im right
prob_eagles <- predict(eagles.lm, newdata = eagles_valid, type = "response")

#pclass 0.4 based on eagles.lm
pclass_eagles <- ifelse(prob_eagles>=0.4,1,0)

df_class_prob_eagles <- data.frame(eagles_valid$fourth_down_converted, prob_eagles, pclass_eagles)
head(df_class_prob_eagles)


#confusion matrix for eagles.lm accuracy
library(caret)
confusionMatrix(factor(pclass_eagles),factor(eagles_valid$fourth_down_converted),positive = "1")

#pclass 0.5 cut off

pclass_eagles_0.5 <- ifelse(prob_eagles>=0.5,1,0)

confusionMatrix(factor(pclass_eagles_0.5),factor(eagles_valid$fourth_down_converted),positive = "1")


###Extra analysis, it shows the avg yds to go for a successful fourth_down
##Calculate avg yds_to_go for a successful fourth_down
successful_fourth_down <- eagles_clean |> filter(fourth_down_converted == 1)
avg_yds_to_go <- mean(successful_fourth_down$ydstogo)
avg_yds_to_go

max_yds_to_go <- max(successful_fourth_down$ydstogo)
max_yds_to_go

##most often yds_to_go for a successful fourthdown conversion
successful_fourth_down <- eagles_clean |> filter(fourth_down_converted == 1)
ydstogo_freq_table <- table(successful_fourth_down$ydstogo)
ydstogo_mode <- as.numeric(names(ydstogo_freq_table)[which.max(ydstogo_freq_table)])
ydstogo_mode


##Success rate based on playtype
play_type_success_rate <- eagles_clean |>
  group_by(play_type) |>
  summarize(successes = sum(fourth_down_converted), 
            attempts = n(), 
            success_rate = successes / attempts * 100)
play_type_success_rate



##bar chart 
ggplot(play_type_success_rate , aes(x =play_type,y = success_rate)) +
  geom_bar(stat = "identity", fill = "#004C54") +
  labs(x = "Play Type", y = "Success Rate (%)", title = "Success Rate vs. Play Type") +
  theme_minimal()

##successrate based on yards to go 
ydstogo_success_rate <- eagles_clean |> 
  group_by(ydstogo) |>
  summarize(successes = sum(fourth_down_converted), 
            attempts = n(), 
            success_rate = successes / attempts * 100)
ydstogo_success_rate

#barchart in kelly green color 

ggplot(ydstogo_success_rate , aes(x = ydstogo, y = success_rate)) +
  geom_bar(stat = "identity", fill = "#4CBB17") +
  labs(x = "Yards to Go", y = "Success Rate (%)", title = "Success Rate vs. Yards to Go") +scale_x_continuous(breaks = seq(min(ydstogo_success_rate$ydstogo), max(ydstogo_success_rate$ydstogo), by = 1))+
  theme_minimal()

##Quarter analysis
quarter_success_rate <- eagles_clean |> 
  group_by(qtr) |>
  summarize(successes = sum(fourth_down_converted), 
            attempts = n(), 
            success_rate = successes / attempts * 100)
quarter_success_rate


#quarter vs success rate chart
ggplot(quarter_success_rate, aes(x = factor(qtr), y = success_rate)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = round(success_rate, 1)), vjust = -0.5, size = 4) +
  labs(x = "Quarter", y = "Success Rate (%)", title = "Fourth-Down Conversion Success Rate by Quarter") +
  theme_minimal()

write.csv(quarter_success_rate, "quarter_success_rate.csv", row.names = FALSE)



##4th down conversion rate 
# Filter data for 2022

pbp_2022 <- load_pbp(2022)
nrow(pbp_2022)

# Filter for Philadelphia Eagles plays and fourth down attempts
library(dplyr)

# Filter for Philadelphia Eagles plays, fourth down attempts, and pass or run plays
library(dplyr)

##Success rate 2022
eagles_plays <- pbp_2022 |>
  filter(posteam == "PHI", down == 4) |>
  filter(play_type == "pass" | play_type == "run")


successful_conversions <- eagles_plays |>
  filter(fourth_down_converted == 1)

# Calculate success rate
success_rate <- nrow(successful_conversions) / nrow(eagles_plays)
su
# Display success rate as a percentage
success_rate_percent <- success_rate * 100
success_rate_percent

##success rate from 1999-2022

eagles_plays1 <- pbp |>
  filter(posteam == "PHI", down == 4) |>
  filter(play_type == "pass" | play_type == "run")

successful_conversions1 <- eagles_plays1 |>
  filter(fourth_down_converted == 1)


# Calculate success rate
success_rate1 <- nrow(successful_conversions1) / nrow(eagles_plays1)

# Display success rate as a percentage
success_rate_percent1 <- success_rate1 * 100
success_rate_percent1



# Split the data into two groups based on the 50-yard line
eagles_plays1_closer <- eagles_plays1 |>
  filter(yardline_100 <= 50)

eagles_plays1_farther <- eagles_plays1 |>
  filter(yardline_100 > 50)

# Calculate success rates for the two groups
successful_conversions1_closer <- eagles_plays1_closer |>
  filter(fourth_down_converted == 1)

successful_conversions1_farther <- eagles_plays1_farther |>
  filter(fourth_down_converted == 1)

success_rate1_closer <- nrow(successful_conversions1_closer) / nrow(eagles_plays1_closer)
success_rate1_farther <- nrow(successful_conversions1_farther) / nrow(eagles_plays1_farther)

# Display success rates as percentages
success_rate_percent1_closer <- success_rate1_closer * 100
success_rate_percent1_farther <- success_rate1_farther * 100


success_rate_percent1_closer
success_rate_percent1_farther

avg_yardline_100 <- mean(eagles_plays1$yardline_100)
avg_yardline_100


pbp_2022_23 <-load_pbp(2023)

#Trying to experiment adding weather and find the columns that contains "time"
colnames(eagles)
time_columns <- grep("time", colnames(eagles), value = TRUE)
time_columns
eagles(endclocktime)
eagles[end_time]
