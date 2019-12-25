# FINAL PROJECT CODE

###### PREPARING THE DATA ######################
# determine the path and then input into the below: with double slashes to recognize they aren't tabs
# https://www.kaggle.com/bradklassen/pga-tour-20102018-data#2019_data.csv
data <- read.csv("D:\\My Data\\Desktop\\2019_data.csv") 
View(data)

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

x <- unique(data$Variable)
x[1000:1486] # to see the rest of the data

# difference between Statistic and Variable
diff_S_V <- data.frame(data$Statistic, data$Variable)
head(diff_S_V)

#################### 1st categorical variable in the data univariate: data$Statistic
# to understand how much there is to analyze with golf, let's take the statistics variable

table(data$Statistic)
nrow(table(data$Statistic))
barplot(table(data$Statistic), xlim = c(0, 500), xlab = "Statistics", ylab = "Frequency", 
        main = "Frequency of Each Statistic in PGA Dataset", col = rainbow(12), las = 0, xaxt = "n")
which.max(table(data$Statistic))
max(table(data$Statistic))

################## 2nd categorical univariate: data$Variable

### helped me to understand the Variable row values more

library(stringr)

a <- data$Variable[str_detect(data$Variable, "Official World Golf Ranking.+")]
table(a)
unique(a)

unique(data$Variable[str_detect(data$Variable, "PGA Championship.+")])

#### 2nd categorical analysis continued ################

nrow(table(data$Variable))
barplot(table(data$Variable), xlim = c(0, 2000), xlab = "Variables", ylab = "Frequency",
        main = "Frequency of Each Variable in PGA Dataset", col = rainbow(10), las = 0, xaxt = "n")

which.max(table(data$Variable))
max(table(data$Variable))

# Within the above parameters, specify a section within the Variable

p <- data$Variable[str_detect(data$Variable, "Official World Golf Ranking.+")]
t <- table(data$Variable[str_detect(data$Variable, "Official World Golf Ranking.+")])
which.max(table(p))
which.max(p)
max(table(p))
plot(table(p), xaxt = "n", main = "Frequency of Unique Value of Variable Official World Golf Ranking",
     ylab = "Frequency of Unique Value", xlab = "Variables") 

########################### Numeric Variable Analysis Univariate ##################### 

### without tibble:

table(data$Value)
length(table(data$Value))
range(table(data$Value))

### with tibble and in narrowing down like-values

########################## TIDYVERSE AND DPLYR

install.packages("tidyverse")
library(tidyverse)
data_t <- as.tibble(data)
glimpse(data_t)

head(as.tibble(data))

## tests on the data to see if it worked correctly - include in Word

filter(data_t, Player.Name == "Dustin Johnson", Date == "June 13 - June 16 (2019)")
filter(data_t, Variable == "Official World Golf Ranking - (TOTAL POINTS)", Date == "June 13 - June 16 (2019)", desc(Value)) # you can see here the Value's skewed

new <- data %>% # doesn't help solve the value issue - show in word 
  select(Player.Name, Date, Variable, Value)%>% 
  arrange(desc(as.numeric(Value))) %>%
  filter(Variable == "Official World Golf Ranking - (AVG POINTS)", Date == "June 13 - June 16 (2019)")

new

# this is how i have to do it

n_new <- new[0:100, ]
n_new
n_new$Value 
table(n_new$Value)
s <- table(n_new$Value)
n_new_f <- s[s != 0]
n_new_f
sum(n_new_f)
length(n_new_f)
which.max(s)
max(s)

# based on the above, for the top 100 players

mean(n_new_f)
sd(n_new_f)
median(n_new_f)
which.max(n_new_f)
max(n_new_f)

barplot(n_new_f, ylab = "Frequency", xlab = "Average Values", main = "Frequency for the Top 100 Players' Average Points",
        col = "cyan")


# making dataframe to test

name <- c("Brooks Koepka", "Dustin Johnson", "Justin Rose", "Rory McIlroy", "Tiger Woods", "Francesco Molinari", "Justin Thomas", "Patrick Cantlay",
          "Xander Schauffele", "Bryson DeChambeau", "Jon Rahm", "Gary Woodland", "Matt Kuchar", "Rickie Fowler", "Paul Casey", "Tony Finau", "Adam Scott",
          "Jason Day", "Webb Simpson", "Tommy Fleetwood", "Louis Oosthuizen", "Bubba Watson", "Marc Leishman", "Matt Wallace", "Patrick Reed",
          "Phil Mickelson", "Kevin Kisner", "Hideki Matsuyama", "Jordan Spieth", "Sergio Garcia")
date_range <- rep("June 13 - June 16 (2019)", 30)
stat <- rep("Official World Golf Ranking", 30)
variable <- rep("Average Points", 30)
value <- c(12.08, 10.46, 8.70, 8.63, 7.47, 6.90, 6.66, 6.44, 6.41, 6.40, 5.74, 5.60, 5.47, 5.43, 5.13, 5.08, 5.02, 4.71, 4.46, 4.43, 4.11, 4.03, 3.91, 
           3.72, 3.68, 3.64, 3.49, 3.37, 3.35, 3.25)


Top_30 <- data.frame(Name = name,
                     Date = date_range,
                     Stat = stat,
                     Variable = variable,
                     Value = value)

Top_30

length(value)
length(name) # to check

sum(value)/30
mean(value)
median(value)
table(value) # all occur only once
range(value)
diff(range(value))
var(value)
sd(value)

fivenum(value)
f <- fivenum(value)
summary(value)
quantile(value, c(0, 0.25, 0.5, 0.75, 1))
IQR(value)
scale(value)

# outliers:

outliers <- c(f[2] - 1.5 * (f[4] - f[2]), f[4] + 1.5 * (f[4] - f[2]))
outliers

boxplot(value, horizontal = TRUE, xaxt = "n", col = hcl(0), xlab = "Values of Top 30 Players")
axis(side = 1, at = fivenum(value), labels = TRUE)

par(mfrow = c(1, 3))

# plot without table values
barplot(value, xlab = "Average Points Per Player", ylab = "Total Points", col = "cyan", las = 1,
        main = "Average Points of Top 30 Players")

dotchart(value, xlab = "Average Points Per Player", ylab = "Total Points", labels = 1:30,
         main = "Dotchart of the Values of the Average Points of the Top 30 Players")


hist(value, col = hcl(1), xlab = "Average Points per Player")

# plot with table values
table(value)
barplot(table(value)) # uniform

### good variables to work with test

d1 <- data_t %>% 
  select(Player.Name, Date, Variable, Value)%>% 
  arrange(desc(Value)) %>%
  filter(Variable == "Official World Golf Ranking - (AVG POINTS)", Date == "June 13 - June 16 (2019)")

d1[0:100, ]

# find those values that are out of place in the top 100, below:

brooks <- data_t %>%
  select(Player.Name, Date, Variable, Value)%>% 
  arrange(desc(as.numeric(Value))) %>%
  filter(Variable == "Official World Golf Ranking - (AVG POINTS)", Date == "June 13 - June 16 (2019)", Player.Name == "Brooks Koepka") 

brooks

dustin <- data %>%
  select(Player.Name, Date, Variable, Value)%>% 
  arrange(desc(as.numeric(Value))) %>%
  filter(Variable == "Official World Golf Ranking - (AVG POINTS)", Date == "June 13 - June 16 (2019)", Player.Name == "Dustin Johnson") 

dustin

#### HERE IS WHERE WE CONVERT THAT COLUMN INTO NUMERIC AND IGNORE PERCENT VALUES BY
# USING THE DATA WE KNOW ISNT A PERCENT BY FILTERING BY THE VARIABLE COLUMN

data$Value <- (str_replace_all(data$Value, "[,$]", "")) # no period
data$Value <- as.numeric(data$Value)
data$Value # good to go

# need to fix the values column again for data_t

data_t$Value <- str_replace_all(data_t$Value, "[,$]", "") # no period
data_t$Value <- as.numeric(data_t$Value)
head(data_t$Value, na.rm = TRUE) # good to go
# data_t$Value <- round(data_t$Value) # should i round? maybe data is rounded data_t is not


##### VARIABLES IM USING ############'

# Official World Golf Ranking - (AVG POINTS)
# All-Around Ranking - (TOTAL)
# Greens in Regulation Percentage - (%)
# Hit Fairway Percentage - (%)
# Putting Average - (AVG) 

### NOw we need to order the data to work with it differently than the tibble:

data_value_n <- data[with(data, order(-data$Value)), ] # order by positive value
head(data_value_n)

data_value_p <- data[with(data, order(data$Value)), ] # order by negative value
head(data_value_p)

### Here we take the subset of the values to compare the players against the statistics and see if they match the pattern of the Top 30

a <- subset(data_value_n, data_value_n$Variable == "Official World Golf Ranking - (AVG POINTS)" & data_value_n$Date == "June 13 - June 16 (2019)")
a <- head(a, n = 30)
a

b <- subset(data_value_p, data_value_p$Variable == "Putting Average - (AVG)" & data_value_p$Date == "June 13 - June 16 (2019)")
b <- head(b, n = 30)
b

c <- subset(data_value_p, data_value_p$Variable == "All-Around Ranking - (TOTAL)" & data_value_p$Date == "June 13 - June 16 (2019)")
c <- head(c, n = 30)
c

d <- subset(data_value_n, data_value_n$Variable == "Hit Fairway Percentage - (%)" & data_value_n$Date == "June 13 - June 16 (2019)")
d <- head(d, n = 30)
d

e <- subset(data_value_n, data_value_n$Variable == "Greens in Regulation Percentage - (%)" & data_value_n$Date == "June 13 - June 16 (2019)")
e <- head(e, n = 30)
e

# compare two: player, for Putting Average (b) and Hit Fairway Percent (d)
table(b$Value, d$Value) # unsummarized - interpret numeric values
b1 <- table(b$Player.Name, b$Value)[table(b$Player.Name) != 0] 
d1 <- table(d$Player.Name, b$Value)[table(d$Player.Name) != 0] 
b1[b1 != 0]
d1[d1 != 0]

# to compare the three charts we make a new dataframe
abc <- data.frame("Hit Fairway Percentage" = d$Value,
                  "Greens in Regulation Percentage" = e$Value,
                  "Putting Average" = b$Value,
                  "All-Around Ranking" = c$Value,
                  "Official Ranking" = a$Value)

head(abc)

players <- data.frame("Hit Fairway Percentage" = d$Player.Name,
                      "Greens in Regulation Percentage" = e$Player.Name,
                      "Putting Average" = b$Player.Name,
                      "All-Around Ranking" = c$Player.Name,
                      "Official Ranking" = a$Player.Name)

head(players, n = 10) # looks like putting averag (b) and greens in regulation (e) are most important in doing well

par(mfrow = c(1,3))

plot(b$Value, d$Value, main = "Top 30 Players", xlab = "Putting Average", ylab = "Hit Fairway Percentage")
plot(b$Value, e$Value, main = "Top 30 Players", xlab = "Putting Average", ylab = "Greens in Regulation")
plot(d$Value, e$Value, main = "Top 30 Players", xlab = "Hit Fairway Percentage", ylab = "Greens in Regulation")

# compare official ranking to the three variables:
plot(a$Value, b$Value, main = "Top 30 Players", xlab = "Official Ranking", ylab = "Putting Average") # inverse relationship between official ranking and putting average
plot(a$Value, d$Value, main = "Top 30 Players", xlab = "Official Ranking", ylab = "Hit Fairway Percentage") # positive relationship between official ranking and Hit Fairway Percentage
plot(a$Value, e$Value, main = "Top 30 Players", xlab = "Official Ranking", ylab = "Greens in Regulation") # positive relationship between official ranking and Greens in Regulation Percentage

# the below all reflect the plots

x <- table(abc$Hit.Fairway.Percentage, abc$Official.Ranking)
y <- table(abc$Putting.Average, abc$Official.Ranking)

addmargins(x)
prop.table(x, 1)
prop.table(x, 2)
prop.table(x)
prop.table(y)

hist(b$Value, main = "Histogram of Putting Average", xlab = "Putting Avg")
hist(d$Value, main = "Histogram of Hit Fairway %", xlab = "Hit Fairway %")
hist(e$Value, main = "Histogram of Greens in Regulation", xlab = "GIR")

par(mfrow = c(1,3))

fivenum(b$Value)
mean(b$Value)
median(b$Value)
sd(b$Value)
boxplot(b$Value, horizontal = TRUE, main = "Boxplot for Putting Average", col = "yellow")
axis(side = 1, at = fivenum(b$Value), labels = TRUE)


fivenum(d$Value) # top players are within the range no outliers
mean(d$Value)
median(d$Value)
sd(d$Value)
boxplot(d$Value, horizontal = TRUE, main = "Boxplot for Hit Fairway Percentage", col = "pink")
axis(side = 1, at = fivenum(d$Value), labels = TRUE)


fivenum(e$Value)
mean(e$Value)
median(e$Value)
sd(e$Value)
boxplot(e$Value, horizontal = TRUE, main = "Boxplot for GIR", col = "blue")
axis(side = 1, at = fivenum(e$Value), labels = TRUE)

### OTHER WAYS TO FILTER THE DATA

data %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "PGA Championship Points - (POINTS)", Date == "June 13 - June 16 (2019)")

data %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(Value) %>%
  filter(Variable == "All-Around Ranking - (TOTAL)", Date == "June 13 - June 16 (2019)") 

# "Hit Fairway Percentage - (%)"
data %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "Hit Fairway Percentage - (%)", Date == "June 13 - June 16 (2019)") 

#"Greens in Regulation Percentage - (%)"
data %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "Greens in Regulation Percentage - (%)", Date == "June 13 - June 16 (2019)") 

#"Greens in Regulation Percentage - (# HOLES)"
data %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "Greens in Regulation Percentage - (# HOLES)", Date == "June 13 - June 16 (2019)") 

#"Putting Average - (AVG)"
data %>% # we don't want to round too much, but maybe now i just round to the nearest two digits?
  select(Player.Name, Date, Variable, Value)%>%
  arrange(Value) %>%
  filter(Variable == "Putting Average - (AVG)", Date == "June 13 - June 16 (2019)") 

# NOW DO ALL OF THESE FOR THE TIBBLE

data_t %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "PGA Championship Points - (POINTS)", Date == "June 13 - June 16 (2019)")

data_t %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(Value) %>%
  filter(Variable == "All-Around Ranking - (TOTAL)", Date == "June 13 - June 16 (2019)") 

# "Hit Fairway Percentage - (%)"
data_t %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "Hit Fairway Percentage - (%)", Date == "June 13 - June 16 (2019)") 

#"Greens in Regulation Percentage - (%)"
data_t %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "Greens in Regulation Percentage - (%)", Date == "June 13 - June 16 (2019)") 

#"Greens in Regulation Percentage - (# HOLES)"
data_t %>%
  select(Player.Name, Date, Variable, Value)%>%
  arrange(desc(Value)) %>%
  filter(Variable == "Greens in Regulation Percentage - (# HOLES)", Date == "June 13 - June 16 (2019)") 

#"Putting Average - (AVG)"
data_t %>% # we don't want to round too much, but maybe now i just round to the nearest two digits?
  select(Player.Name, Date, Variable, Value)%>%
  arrange(Value) %>%
  filter(Variable == "Putting Average - (AVG)", Date == "June 13 - June 16 (2019)") 

#### FOR VARIABLE WITH NUMERICAL DATA LETS DO HOLES IN ONE ##################
# e1. Eagles (Holes per) - (# OF EAGLES) 

e1 <- data %>%
  select(Player.Name, Date, Variable, Value) %>%
  arrange(desc(Value)) %>%
  filter(Variable == "Eagles (Holes per) - (# OF EAGLES)")

### data_t

e1t <- data_t %>%
  select(Player.Name, Date, Variable, Value) %>%
  arrange(desc(Value)) %>%
  filter(Variable == "Eagles (Holes per) - (# OF EAGLES)")

### DIFFERENCES BETWEEN TIBBLE AND REGULAR DATAFRAME
head(e1)
head(e1t)

# THEY ARE THE SAME because of how I sorted and ordered them

##### sampling variables - random variable example--eagles per number of holes

options(digits = 2)

table(e1$Value) # same as table(e1d$Value)

e1d <- data %>% # narrowing down for date - cumulative
  select(Player.Name, Date, Variable, Value) %>%
  arrange(desc(Value)) %>%
  filter(Variable == "Eagles (Holes per) - (# OF EAGLES)", Date == "June 13 - June 16 (2019)")
e1d

x <- e1d$Value
x

mean(x)

# frequencies of eagles data
count <- table(e1d$Value)
count

# above contingency table converted to dataframe:
dframe <- as.data.frame(count)
dframe

# probability distribution
f <- dframe$Freq / sum(dframe$Freq)
f
sum(f)

# Plotting the distribution 
par(mfrow = c(1,2))

boxplot(f, horizontal = TRUE, main = "Distribution of Eagles", col = "cyan")
axis(side = 1, labels = TRUE)
mean(f)
median(f)
sd(f)
fivenum(f)
hist(f, main = "Histogram of Eagles", col = "cyan", xlab = "Number of Eagles")

par(mfrow = c(1,3))

plot(x, main = "Plot of Frequencies of Eagles", xlab = "Number of Eagles", ylab = "Frequencies")
barplot(x, main = "Barplot of Frequencies of Eagles", xlab = "Number of Eagles", ylab = "Frequencies") 
hist(x, main = "Histogram of Frequencies of Eagles", xlab = "Number of Eagles", ylab = "Frequencies")
mean(x)
median(x)
sd(x)
fivenum(x)

library(prob)

# looks like normal distribution but won't take negative values but real world implication doesn't make sense so doesn't exist
set.seed(150)

par(mfrow = c(1,1))

mean(x)
sd(x)
hist(x, prob = TRUE)

samples <- combn(x,2)

xbar <- apply(samples, 2, FUN = mean) 

hist(xbar, prob = TRUE, xlim = c(0, 15))
mean(xbar)
sd(xbar)

samples.2 <- combn(x,3)

xbar.2 <- apply(samples.2, 2, FUN = mean) 

hist(xbar.2, prob = TRUE, xlim = c(0, 15))
mean(xbar.2)
sd(xbar.2)

length(x)

# this sample is so big and slow to run
samples.3 <- combn(x, 4)

xbar.3 <- apply(samples.3, 2, FUN = mean)
xbar.3

hist(xbar.3, prob = TRUE, xlim = c(0, 15))
mean(xbar.3)
sd(xbar.3)

mean(x)
mean(xbar)
mean(xbar.2)
mean(xbar.3)

sd(x)
sd(xbar)
sd(xbar.2)
sd(xbar.3)

par(mfrow = c(2,2))

hist(x, prob = TRUE)
hist(xbar, prob = TRUE, xlim = c(0, 15))
hist(xbar.2, prob = TRUE, xlim = c(0, 15))
hist(xbar.3, prob = TRUE, xlim = c(0, 15))

##################### Show how various sampling methods can be used on your data ########################
#### THIS IS DONE WITH THE HOLES IN 1 ALSO

set.seed(150)

library(sampling)

names(e1d)
head(e1d)
nrow(e1d)
table(e1d$Value)
table(e1d$Player.Name)[table(e1d$Player.Name) != 0]
sum(table(e1d$Player.Name)[table(e1d$Player.Name) != 0]) # 207 players got at least 1 hole in one!

sample_table <- table(e1d$Value)

#### SIMPLE RANDOM SAMPLING WITH AND WITHOUT REPLACEMENT
set.seed(150)

#### WITH

s <- srswr(50, nrow(e1d))
s[s!=0]

rows <- (1:nrow(e1d))[s!=0]
rows <- rep(rows, s[s != 0])
rows

sample.1 <- e1d[rows, ]
head(sample.1)
table(sample.1$Player.Name)[table(sample.1$Player.Name) != 0]

sample.1_table <- table(sample.1$Value)

#### WITHOUT

s1 <- srswor(50, nrow(e1d))
sample.2 <- e1d[s != 0, ]
head(sample.2)
table(sample.2$Player.Name)[table(sample.2$Player.Name) != 0]

sample.2_table <- table(sample.2$Value)

#### Systematic Sampling
set.seed(150)
N <- nrow(e1d)
n <- 30
k <- ceiling(N/n)
k

r <- sample(k, 1)
r

s <- seq(r, by = k, length = n)

sample.3 <- e1d[s, ]
head(sample.3)
table(sample.3$Player.Name)[table(sample.3$Player.Name) != 0]

sample.3_table <- table(sample.3$Value)

#### UNEQUAL PROBABILITIES 
set.seed(150)
pik <- inclusionprobabilities(e1d$Value, 50)
length(pik)
sum(pik)

s <- UPsystematic(pik)
sample.4 <- e1d[s != 0, ]
head(sample.4)
table(sample.4$Value)
table(sample.4$Player.Name)[table(sample.4$Player.Name) != 0]

sample.4_table <- table(sample.4$Value)

######### STRATIFIED SAMPLE
set.seed(150)

freq <- table(e1d$Value)
freq

st.sizes <- 16 * freq/sum(freq) 
st.sizes

st.1 <- strata(e1d, stratanames = c("Value"), size = st.sizes, method = "srswr", description = TRUE)
st.1

# wouldn't work without replacement:
st.2 <- strata(e1d, stratanames = c("Value"), size = st.sizes, method = "srswor", description = TRUE)
st.2

# won't recognize the stratified sampling - issue with dataframe
st.sample1 <- getdata(e1d$Value, st.1)
st.sample2 <- getdata(e1d$Value, st.2)

############## SUMMARY OF SAMPLES #################33

sample_table
reg <- c(18, 21, 33, 30, 26, 22, 21, 16, 9, 4, 2, 3, 2)

sample.1_table
s1.reg <- c(3, 4, 9, 7, 3, 8, 7, 4, 3, 1, 1)
percent1 <- s1.reg/reg * 100
percent1

sample.2_table
s2.reg <- c(3, 3, 9, 6, 3, 7, 6, 3, 3, 1, 1)
percent2 <- s2.reg/reg * 100
percent2

sample.3_table
s3.reg <- c(3, 3, 4, 5, 3, 3, 3, 3, 1, 1, 1)
percent3 <- s3.reg/reg * 100
percent3

sample.4_table
s4.reg <- c(1, 2, 5, 6, 6, 7, 7, 6, 4, 2, 1, 2, 1)
percent4 <- s4.reg/reg * 100
percent4

sample_table
sample.1_table
sample.2_table
sample.3_table
sample.4_table
percent1
percent2
percent3
percent4 


