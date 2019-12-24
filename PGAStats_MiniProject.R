###### mini-project-solution DARNIS CS555 ######
#### from Kaggle Dataset found: https://www.kaggle.com/bradklassen/pga-tour-20102018-data
data <- read.csv("/Users/LaurenDarnis/Desktop/pga-tour-20102018-data/2019_data.csv")

# learn more about the data
head(data)
tail(data)
names(data)
nrow(data)
ncol(data)

unique(data$Player.Name)
unique(data$Date)
unique(data$Statistic)
unique(data$Variable)
unique(data$Value)

#### HERE IS WHERE WE CONVERT THAT COLUMN INTO NUMERIC AND IGNORE PERCENT VALUES BY
# USING THE DATA WE KNOW ISNT A PERCENT BY FILTERING BY THE VARIABLE COLUMN

library(stringr)

data$Value <- (str_replace_all(data$Value, "[,$]", "")) # no period
data$Value <- as.numeric(data$Value)
data$Value # good to go

##### VARIABLES IM USING ############'

# Official World Golf Ranking - (AVG POINTS)
# All-Around Ranking - (TOTAL)
# Greens in Regulation Percentage - (%)
# Hit Fairway Percentage - (%)
# Putting Average - (AVG) 

### Now we need to order the data to work with it

data_value_n <- data[with(data, order(-data$Value)), ] # order by positive value / descending value
head(data_value_n)

data_value_p <- data[with(data, order(data$Value)), ] # order by negative value / ascending value
head(data_value_p)

### Here we take the subset of the values to compare the players against the statistics and see if they match the pattern of the Top 30

avg_points <- subset(data_value_n, data_value_n$Variable == "Official World Golf Ranking - (AVG POINTS)" & data_value_n$Date == "2019-08-04")
avg_points <- head(avg_points, n = 30)
avg_points

par(mfrow = c(1, 2))

hist(avg_points$Value, main = "Official World Golf Ranking", xlab = "Average Points", col = hcl(0))
boxplot(avg_points$Value, main = "Official World Golf Ranking", xlab = "Average Points", col = hcl(0))

## has outlier above fivenum range

f1 <- fivenum(avg_points$Value)
f1

cutoff1 <- c(f1[2] - (1.5 * (f1[4] - f1[2])), f1[4] + (1.5 * (f1[4] - f1[2])))
cutoff1

putting_avg <- subset(data_value_p, data_value_p$Variable == "Putting Average - (AVG)" & data_value_p$Date == "2019-08-04")
putting_avg <- head(putting_avg, n = 30)
putting_avg

hist(putting_avg$Value, main = "Putting Average", xlab = "Average", col = hcl(0))
boxplot(putting_avg$Value, main = "Putting Average", xlab = "Average", col = hcl(0))

# no outliers

f2 <- fivenum(putting_avg$Value)
f2

cutoff2 <- c(f2[2] - (1.5 * (f2[4] - f2[2])), f2[4] + (1.5 * (f2[4] - f2[2])))
cutoff2

all_around_rank <- subset(data_value_p, data_value_p$Variable == "All-Around Ranking - (TOTAL)" & data_value_p$Date == "2019-08-04")
all_around_rank <- head(all_around_rank, n = 30)
all_around_rank

hist(all_around_rank$Value, main = "All-Around Ranking", xlab = "Total Ranking", col = hcl(0))
boxplot(all_around_rank$Value, main = "All-Around Ranking", xlab = "Total Ranking", col = hcl(0))

# no outliers

f3 <- fivenum(all_around_rank$Value)
f3

cutoff3 <- c(f3[2] - (1.5 * (f3[4] - f3[2])), f3[4] + (1.5 * (f3[4] - f3[2])))
cutoff3

HIT_fairway_perc <- subset(data_value_n, data_value_n$Variable == "Hit Fairway Percentage - (%)" & data_value_n$Date == "2019-08-04")
HIT_fairway_perc <- head(HIT_fairway_perc, n = 30)
HIT_fairway_perc

hist(HIT_fairway_perc$Value, main = "Hit Fairway Percentage", xlab = "Percentage of Hits on the Fairway", col = hcl(0))
boxplot(HIT_fairway_perc$Value, main = "Hit Fairway Percentage", xlab = "Percentage of Hits on the Fairway", col = hcl(0))

## has outliers greater than fivenum range

f4 <- fivenum(HIT_fairway_perc$Value)
f4

cutoff4 <- c(f4[2] - (1.5 * (f4[4] - f4[2])), f4[4] + (1.5 * (f4[4] - f4[2])))
cutoff4

HIT_fairway_perc$Value[HIT_fairway_perc$Value > cutoff4[2]] # identify outliers 
HIT_fairway_perc$Value[HIT_fairway_perc$Value < cutoff4[1]] # none below

gir <- subset(data_value_n, data_value_n$Variable == "Greens in Regulation Percentage - (%)" & data_value_n$Date == "2019-08-04")
gir <- head(gir, n = 30)
gir

hist(gir$Value, main = "Greens in Regulation", xlab = "Percentage GIR", col = hcl(0))
boxplot(gir$Value, main = "Greens in Regulation", xlab = "Percentage GIR", col = hcl(0))

## has outliers greater than fivenum range

f5 <- fivenum(gir$Value)
f5

cutoff5 <- c(f5[2] - (1.5 * (f5[4] - f5[2])), f5[4] + (1.5 * (f5[4] - f5[2])))
cutoff5

gir$Value[gir$Value > cutoff5[2]] # one (has to be first row)
gir$Value[gir$Value < cutoff5[1]] # none

#### correlation tests
cor(putting_avg$Value, HIT_fairway_perc$Value) # -0.9586312
cor(putting_avg$Value, gir$Value) # -0.9667493
cor(HIT_fairway_perc$Value, gir$Value) # 0.9394349

# these all make sense because you want the lowest putting score and highest percentage
# in the other categories to be the best player

########## simple linear regression to determine relationship of putting average and official ranking
slr <- lm(avg_points$Value ~ putting_avg$Value)
anova(slr)
summary(slr) ## significant 

########## simple linear regression to determine relationship of Hit fairway percentage and official ranking
slr2 <- lm(avg_points$Value ~ HIT_fairway_perc$Value)
anova(slr2)
summary(slr2) ## significant 

########## simple linear regression to determine relationship of greens in regulation and official ranking
slr3 <- lm(avg_points$Value ~ gir$Value)
anova(slr3)
summary(slr3) ## significant 

########## simple linear regression to determine relationship of putting average and all around ranking
slr4 <- lm(all_around_rank$Value ~ putting_avg$Value)
anova(slr4)
summary(slr4) ## significant 

########## simple linear regression to determine relationship of Hit fairway percentage and all around ranking
slr5 <- lm(all_around_rank$Value ~ HIT_fairway_perc$Value)
anova(slr5)
summary(slr5) ## significant

########## simple linear regression to determine relationship of gir and all around ranking
slr6 <- lm(all_around_rank$Value ~ gir$Value)
anova(slr6)
summary(slr6)  ## significant 

###### all in mlr
mlr <- lm(avg_points$Value ~ putting_avg$Value + gir$Value + HIT_fairway_perc$Value)
summary(mlr)

### all are significant

######### multiple linear regression to determine best combo for official ranking
mlr1 <- lm(avg_points$Value ~ putting_avg$Value + gir$Value + HIT_fairway_perc$Value)
summary(mlr1) # all significant

# now we adjust for hit fairway percentage
mlr2 <- lm(avg_points$Value ~ putting_avg$Value + gir$Value) # here it says that putting average isn't significant
summary(mlr2) # gir is significant

# now we adjust for putting
mlr3 <- lm(avg_points$Value ~ gir$Value + HIT_fairway_perc$Value) # both are significant
summary(mlr3)

# now we adjust for gir
mlr4 <- lm(avg_points$Value ~ putting_avg$Value + HIT_fairway_perc$Value) # here putting avg isn't significant
summary(mlr4) # HIT is significant

# so now we see both of these lets find out which is more significant for official ranking, hit fairway or gir

######### multiple linear regression to determine best combo for all around ranking
mlr5 <- lm(all_around_rank$Value ~ putting_avg$Value + gir$Value + HIT_fairway_perc$Value)
summary(mlr5) # here only putting is significant

# controlling for hit fairway percentage
mlr6 <- lm(all_around_rank$Value ~ putting_avg$Value + gir$Value)
summary(mlr6) # same putting is still significant not gir

# controlling for putting
mlr7 <- lm(all_around_rank$Value ~ gir$Value + HIT_fairway_perc$Value)
summary(mlr7) # both significant, gir is more

# controlling for gir
mlr8 <- lm(all_around_rank$Value ~ putting_avg$Value + HIT_fairway_perc$Value)
summary(mlr8) # only putting is significant





## ORIGINAL REMOVED OUTLIERS, however since they are data from different datasets, we
## have to have the same amount of rows, below is the code i used originally to removed the outliers:

### Here we take the subset of the values to compare the players against the statistics and see if they match the pattern of the Top 30

avg_points <- subset(data_value_n, data_value_n$Variable == "Official World Golf Ranking - (AVG POINTS)" & data_value_n$Date == "2019-08-04")
avg_points <- head(avg_points, n = 30)
avg_points

par(mfrow = c(1, 2))

hist(avg_points$Value, main = "Official World Golf Ranking", xlab = "Average Points", col = hcl(0))
boxplot(avg_points$Value, main = "Official World Golf Ranking", xlab = "Average Points", col = hcl(0))

## has outlier above fivenum range

f1 <- fivenum(avg_points$Value)
f1

cutoff1 <- c(f1[2] - (1.5 * (f1[4] - f1[2])), f1[4] + (1.5 * (f1[4] - f1[2])))
cutoff1

avg_points$Value[avg_points$Value > cutoff1[2]] # identify outlier
avg_points <- avg_points[-c(1), ] # removed outlier from data set
avg_points$Value

putting_avg <- subset(data_value_p, data_value_p$Variable == "Putting Average - (AVG)" & data_value_p$Date == "2019-08-04")
putting_avg <- head(putting_avg, n = 30)
putting_avg

hist(putting_avg$Value, main = "Putting Average", xlab = "Average", col = hcl(0))
boxplot(putting_avg$Value, main = "Putting Average", xlab = "Average", col = hcl(0))

# no outliers

f2 <- fivenum(putting_avg$Value)
f2

cutoff2 <- c(f2[2] - (1.5 * (f2[4] - f2[2])), f2[4] + (1.5 * (f2[4] - f2[2])))
cutoff2

all_around_rank <- subset(data_value_p, data_value_p$Variable == "All-Around Ranking - (TOTAL)" & data_value_p$Date == "2019-08-04")
all_around_rank <- head(all_around_rank, n = 30)
all_around_rank

hist(all_around_rank$Value, main = "All-Around Ranking", xlab = "Total Ranking", col = hcl(0))
boxplot(all_around_rank$Value, main = "All-Around Ranking", xlab = "Total Ranking", col = hcl(0))

# no outliers

f3 <- fivenum(all_around_rank$Value)
f3

cutoff3 <- c(f3[2] - (1.5 * (f3[4] - f3[2])), f3[4] + (1.5 * (f3[4] - f3[2])))
cutoff3

HIT_fairway_perc <- subset(data_value_n, data_value_n$Variable == "Hit Fairway Percentage - (%)" & data_value_n$Date == "2019-08-04")
HIT_fairway_perc <- head(HIT_fairway_perc, n = 30)
HIT_fairway_perc

hist(HIT_fairway_perc$Value, main = "Hit Fairway Percentage", xlab = "Percentage of Hits on the Fairway", col = hcl(0))
boxplot(HIT_fairway_perc$Value, main = "Hit Fairway Percentage", xlab = "Percentage of Hits on the Fairway", col = hcl(0))

## has outliers greater than fivenum range

f4 <- fivenum(HIT_fairway_perc$Value)
f4

cutoff4 <- c(f4[2] - (1.5 * (f4[4] - f4[2])), f4[4] + (1.5 * (f4[4] - f4[2])))
cutoff4

HIT_fairway_perc$Value[HIT_fairway_perc$Value > cutoff4[2]] # identify outliers 
HIT_fairway_perc$Value[HIT_fairway_perc$Value < cutoff4[1]] # none below
HIT_fairway_perc[c(1:length(HIT_fairway_perc)), c(1:5)]
HIT_fairway_perc[c(1:2),]
HIT_fairway_perc <- HIT_fairway_perc[-c(1:2), ] # removed outlier from data set
HIT_fairway_perc$Value 

gir <- subset(data_value_n, data_value_n$Variable == "Greens in Regulation Percentage - (%)" & data_value_n$Date == "2019-08-04")
gir <- head(gir, n = 30)
gir

hist(gir$Value, main = "Greens in Regulation", xlab = "Percentage GIR", col = hcl(0))
boxplot(gir$Value, main = "Greens in Regulation", xlab = "Percentage GIR", col = hcl(0))

## has outliers greater than fivenum range

f5 <- fivenum(gir$Value)
f5

cutoff5 <- c(f5[2] - (1.5 * (f5[4] - f5[2])), f5[4] + (1.5 * (f5[4] - f5[2])))
cutoff5

gir$Value[gir$Value > cutoff5[2]] # one (has to be first row)
gir$Value[gir$Value < cutoff5[1]] # none
HIT_fairway_perc <- HIT_fairway_perc[-c(1),]
HIT_fairway_perc





