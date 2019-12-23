###### TERM PROJECT DRAFT 
# install.packages("Lahman")

library(Lahman)
library(tidyr)
library(dplyr)
library(googleVis)
library(tibble)

# PEOPLE, BATTING, PITCHING, FIELDING, TEAMS, TEAMS FRANCHISES

dimnames(People)[[2]] # playerID, nameFirst, nameLast
dimnames(Batting)[[2]] # playerID, yearID, teamID, lgID, R: runs, H: hits, HR, SO
dimnames(Pitching)[[2]] # playerID, yearID, teamID, lgID, W: wins, L: losses, H: hits, HR: homeruns, SO: strickouts, ERA: earned run average
dimnames(Teams)[[2]] # yearID, lgID, teamID, franchID, divID, Rank
dimnames(TeamsFranchises)[[2]] # franchID, franchName
dimnames(SeriesPost)[[2]] # yearID, round, teamIDwinner, lgIDwinner, teamIDloser, lgIDloser, wins, losses, ties 
dimnames(Salaries)[[2]] # dyearID, teamID, lgID, playerID, salary 

nrow(SeriesPost) # 334
nrow(People) # 19617
nrow(Batting) # 105861
nrow(Pitching) # 46699
nrow(Teams) # 2895
nrow(TeamsFranchises) # 120
nrow(Salaries) # 26428 

teams <- full_join(Teams, TeamsFranchises)
head(teams)
tail(teams)

stats <- full_join(Batting, Pitching)
head(stats)
tail(stats)

stats2 <- full_join(stats, People)
head(stats2)
tail(stats2)

all_info <- full_join(stats2, SeriesPost)
head(all_info)
tail(all_info)

data <- full_join(all_info, teams)

data <- data[ ,c("playerID", "yearID", "teamID", "lgID", "R", "RBI", "H", "AB", "BB", "HBP", 
                 "SF", "SH", "HR", "SO", "W", "L", "IPouts",
                 "ERA", "nameFirst", "nameLast","franchID", "divID", "Rank", "DivWin", "G",
                 "WCWin", "LgWin", "WSWin", "name", "franchName", "round", "teamIDwinner",
                 "lgIDwinner", "teamIDloser", "lgIDloser", "wins", "losses", "ties")]

# create batting average variable
data$BattingAverage <- round((data$H/data$AB), digits = 3)

# create innings pitched from IPouts which is 3 innings per out (doesn't exist already)
data$IP <- round((data$IPouts/3), digits = 2)

# create the Plate appearances for later stats for only qualified players
data$PA <- data$AB + data$BB + data$HBP + data$SF + data$SH 

### ABBREVIATIONS
# R: runs - pitching/batting
# HR: homeruns 
# H: hits - pitching/batting
# SO: strikouts - pitching/batting
# ERA: earned run average
# RBI: runs batted in
# PA: plate appearances
# AB: at bats
# BB: Bases on Balls/Walks
# HBP: Hit By Pitch
# SF: Sacrifice Flies
# SH: Sacrifice Hits
# W: Wins - different than the post series data
# L: Losses - different than the post series data
# IPouts: Innings Pitched Per Out (3)
# IP: Innings Pitched
# BattingAverage: batting ave is number of hits / at bats
# WCWin: WildCard Win
# WSWin: World Series Win
# LgWin: league Win

# filter out the data for 2004
data <- data %>%
  filter(yearID == 2004)

dim(data) # 14212 41

data <- data %>%
  mutate_if(is.numeric, funs(replace_na(., 0)))

# review and learn from some of the data
data$teamID
head(data)
tail(data)
unique(data$round)

########################## PART B ################################

data_bos <- data %>%
  filter(teamID == "BOS")

head(data_bos)
tail(data_bos)
nrow(data_bos) # 533
ncol(data_bos) # 31

# Make sure the NA's are still gone for Boston's data
data_bos <- data_bos %>%
  mutate_if(is.numeric, funs(replace_na(., 0)))

########################## PART C ################################

### 1. PLAYER WITH THE HIGHEST BATTING AVERAGE
data_bos_batting <- data_bos[order(data_bos$BattingAverage, decreasing = TRUE), ]
best_batting_bos <- data_bos_batting[ , c("playerID", "nameFirst", "nameLast", "BattingAverage", "PA")]
best_batting_bos <-
  best_batting_bos %>%
  filter(PA >= 250) %>%
  distinct()

best_batting_bos_10 <- best_batting_bos[1:10, ]
best_batting_bos_10

best_batter_boston <- best_batting_bos[1, ]
best_batter_boston

### 2. PLAYER WITH THE MOST HOMERUNS: HR
data_bos_hr <- data_bos[order(data_bos$HR, decreasing = TRUE), ]
best_hr_bos <- data_bos_hr[ , c("playerID", "nameFirst", "nameLast", "HR")]
head(best_hr_bos, n = 10)

best_hr_bos <- best_hr_bos %>%
  filter(HR < 50) %>%
  distinct()

top_10_hr <- best_hr_bos[1:10, ]
top_10_hr

best_hr_boston <- best_hr_bos[1, ]
best_hr_boston 

### 3. PLAYER WITH BEST EARNED RUN AVERAGE: ERA
data_bos_ERA <- data_bos[order(data_bos$ERA), ]
best_era_bos <- data_bos_ERA[ ,c("playerID", "nameFirst", "nameLast", "ERA", "G", "IP", "IPouts")] 
head(best_era_bos, n = 40)

best_era_bos_play <- best_era_bos %>% # explain how i got here
  filter(G >= 28) %>%
  filter(ERA > 0) %>%
  distinct()

best_era_bos_play
best_era_player <- best_era_bos_play[1, ]
best_era_player

### 4. PLAYER WITH HIGHEST RUNS BATTED IN: RBI's
rbi_top <- data_bos[order(data_bos$RBI, decreasing = TRUE), ]
rbi_top <- rbi_top[ ,c("playerID", "nameFirst", "nameLast", "RBI")] 

rbi_top <- rbi_top %>%
  distinct()

rbi_top
rbi_top_ten <- rbi_top[1:10, ]
rbi_top_ten

rbi_best <- rbi_top[1, ]
rbi_best

############################ PART D ##############################

# show 5 teams for season you selected that have most wins or ranking points in descending order
# highest rank

data_rank <- data[order(data$Rank), ]
data_rank <- data_rank %>%
  filter(Rank >= 1) 

data_rank <- data_rank[ ,c("yearID", "teamID", "lgID", "franchID", "divID", "Rank", "DivWin", "WCWin",
                           "LgWin", "WSWin","franchName")]

data_rank
nrow(data_rank) # 30

data_rank_top_5 <- head(data_rank, n = 5)
data_rank_top_5

# Compare to Post-Series data

post_series <- SeriesPost %>%
  filter(yearID == 2004) %>%
  arrange(desc(wins)) 

post_series_5 <- head(post_series, n = 5)
post_series_5

post_series <- SeriesPost %>%
  filter(yearID == 2004) %>%
  arrange(desc(wins)) %>%
  distinct()

# arrange the wins for all teams
Pitching %>%
  filter(yearID == 2004) %>%
  filter(teamID == "BOS") %>%
  arrange(desc(W))

# filter and sum up the wins for all teams
Pitching %>%
  filter(yearID == 2004) %>%
  group_by(teamID) %>%
  summarise(W = sum(W))

# filter and sum up the losses for all teams
Pitching %>%
  filter(yearID == 2004) %>%
  group_by(teamID) %>%
  summarise(L = sum(L))

# calculate the total wins
sum_wins <- aggregate(data$W, by = list(teamID = data$teamID), FUN = sum)
sum_wins

# name the columns appropriately
colnames(sum_wins) <- c("Team", "Wins")

# calculate the total losses
sum_losses <- aggregate(data$L, by = list(teamID = data$teamID), FUN = sum)
sum_losses

# name the columns appropriately
colnames(sum_losses) <- c("Team", "Losses")

# merge the statistics for team, win and loss
merged_stats <- merge(sum_wins, sum_losses)
merged_stats

# create a new variable, Win Percentage 
merged_stats$WinLPct <- round((merged_stats$Wins / (merged_stats$Wins + merged_stats$Losses)), digits = 3)
merged_stats

# arrange by highest win percentage
WinLPct_merged <- merged_stats %>%
  arrange(desc(WinLPct))

WinLPct_merged

top_5_win_pct <- head(WinLPct_merged, n = 5)
top_5_win_pct

## look individually at the separate wins

data_rank %>%
  filter(DivWin == "Y")

data_rank %>%
  filter(divID == "W") %>%
  filter(DivWin == "Y")

data_rank %>%
  filter(divID == "E") %>%
  filter(DivWin == "Y")

data_rank %>%
  filter(divID == "C") %>%
  filter(DivWin == "Y")

data_rank %>%
  filter(lgID == "AL") %>%
  filter(LgWin == "Y")

data_rank %>%
  filter(lgID == "NL") %>%
  filter(LgWin == "Y")

data_rank %>%
  filter(WCWin == "Y") # wildcard winner

data_rank %>%
  filter(WSWin == "Y")

#################################### PART E ###############################

### Create at least FIVE different data visualizations, each using the principles of
### Module 6, that highlight the strengths and/or weaknesses of the teams and/or the players
### on the teams. You must use at least THREE different chart styles (bar, line, box, scatter, etc.)


# Compare Salary Statistics to those in Parts A and B
player_sal <- full_join(People, Salaries)
top_salaries <- player_sal[order(player_sal$salary, decreasing = TRUE), ]
top_salaries <- top_salaries[ ,c("playerID", "nameFirst", "nameLast", "yearID", "teamID", "lgID", "salary")]
top_salaries_bos <- top_salaries %>%
  filter(yearID == 2004) %>%
  filter(teamID == "BOS")

top_salaries_bos
top_salaries_bos_ten <- top_salaries_bos[1:10, ]

# salaries with which to compare
best_era_bos_play
rbi_top_ten
top_10_hr
best_batting_bos_10
top_salaries_bos_ten

### CHART 1: HIGHEST SALARIES
player_sal <- full_join(People, Salaries)

# I looked at worst salaries too but decided that wasn't as helpful as looking at all of the data
worst_salaries <- player_sal[order(player_sal$salary), ]
worst_sals_bos <- worst_salaries %>%
  filter(yearID == 2004) %>%
  filter(teamID == "BOS")

top_salaries <- player_sal[order(player_sal$salary, decreasing = TRUE), ]
top_salaries <- top_salaries[ ,c("playerID", "nameFirst", "nameLast", "yearID", 
                                 "teamID", "lgID", "salary")]
top_salaries_bos <- top_salaries %>%
  filter(yearID == 2004) %>%
  filter(teamID == "BOS")

top_salaries_bos

top_salaries_bos_20 <- top_salaries_bos[1:20, ]
top_salaries_bos_20 <- top_salaries_bos_20[ ,c("nameLast", "teamID", "salary")]

worst_salaries_bos_20 <- worst_sals_bos[1:20, ]
worst_salaries_bos_20 <- worst_salaries_bos_20[ ,c(
  "playerID", "nameFirst", "nameLast", "yearID", "teamID", "lgID", "salary")] 

top_salaries_bos_final <- top_salaries_bos[ ,c("nameLast", "teamID", "salary")]

chart1 <- 
  gvisBarChart(
    top_salaries_bos_final,
    options = list(gvis.editor="Edit me!",
                   title = "BOS Players with Highest Salaries",
                   width = 850,
                   height = 850,
                   legend = "top"))
plot(chart1)

### CHART 2: RUNS BATTED IN VS HOME RUNS: RBI vs. HR

# Get the statistics for this chart by combining the top ten from part a/b
chart2stats <- full_join(rbi_top_ten, top_10_hr)

rbi_top_ten
top_10_hr

chart2stats <- chart2stats %>%
  mutate_if(is.numeric, funs(replace_na(., 0)))

# Choose top 6 bc NA's in data look confusing
chart2stats <- chart2stats[1:6, ]

chart2 <-
  gvisAreaChart(chart2stats,
                xvar = "nameLast",
                yvar = c("RBI", "HR"),
                options = list(
                  gvis.editor = "Edit me!",
                  title = "BOS: RBI vs. HR",
                  legend = "top",
                  width = 600,
                  height = 400
                ))

plot(chart2)

### CHART 3: HOME RUNS VS BATTING AVERAGE
top_10_hr
best_batting_bos_10

chart3stats <- merge(top_10_hr, best_batting_bos_10)
chart3stats$BattingAverage <- chart3stats$BattingAverage*100

chart3 <-
  gvisLineChart(chart3stats,
                xvar = "nameLast",
                yvar = c("HR", "BattingAverage"),
                options = list(
                  gvis.editor = "Edit me!",
                  title = "BOS: HR vs. Batting Average",
                  legend = "top",
                  width = 600,
                  height = 400
                ))

plot(chart3)

# take first 9 bc last has NA's

### CHART 4: RANK VS ERA VS HR VS W

data_w <- data[ ,c("Rank", "ERA", "HR", "teamID", "W")]
data_w$Rank <- data_w$Rank * 50
data_w$ERA <- data_w$ERA * 50
data_w <- data_w %>%
  filter(Rank > 0) %>%
  arrange(desc(W))

data_w

data_w_best10 <- data_w[1:10, ]
data_w_best10

chart4 <-
  gvisComboChart(data_w_best10,
                 xvar = "teamID",
                 yvar = c("Rank", "ERA", "HR", "W"),
                 options = list(
                   seriesType = "line",
                   series = '{0: {type: "bars"}}',
                   gvis.editor = "Edit me!",
                   title = "Teams: Rank vs. ERA vs. HR vs. W",
                   legend = "top",
                   width = 900,
                   height = 600
                 ))


plot(chart4)

### CHART 5: SALARY VS HR VS RBI VS BATTING AVERAGE

# Create appropriate subsets for new data combination
top_salaries_bos <- top_salaries_bos[ ,c("playerID", "nameFirst", "nameLast", "salary")]
data_bos_batting <- data_bos_batting[ ,c("playerID", "nameFirst", "nameLast", "BattingAverage", 
                                         "RBI", "HR")]


rbi_top
best_hr_bos
data_bos_batting
top_salaries_bos 

merge1 <- full_join(rbi_top, best_hr_bos)
merge2 <- full_join(data_bos_batting, top_salaries_bos)
chart5data <- full_join(merge1, merge2)

chart5data <- chart5data %>%
  mutate_if(is.numeric, funs(replace_na(., 0)))

# multiply or divide statistics to make them visually comparable to the other data
chart5data$BattingAverage <- chart5data$BattingAverage * 100
chart5data$salary <- chart5data$salary/100000

chart5data <- chart5data %>%
  filter(RBI > 0) %>%
  distinct() %>%
  filter(HR > 0) %>%
  filter(salary > 0) %>%
  arrange(desc(RBI))

chart5data

chart5 <-
  gvisColumnChart(chart5data,
                  xvar = "nameLast",
                  options = list(
                    gvis.editor = "Edit me!",
                    title = "Salary vs. Batting Average vs. HR vs. RBI for Boston",
                    legend = "top",
                    width = 1200,
                    height = 600
                  ))

plot(chart5)

################################## PART F ###############################
### Use a mapping function (perhaps gvisGeoChart()) to display the location on a
### map the home locations of the last 10 champions of the league you chose in part a.

all_teams_data <- full_join(SeriesPost, teams)

last_ten_WS <- SeriesPost %>%
  filter(yearID <= 2018) %>%
  filter(yearID >= 2009) %>%
  filter(round == "WS")

last_ten_WS  

last_ten_WS <- last_ten_WS[ ,c("yearID", "teamIDwinner")]  
last_ten_WS  

last_ten_WS_year <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
last_ten_WS_team <- c("NYA", "SFN", "SLN", "SFN", "BOS", "SFN", "KCA", "CHN", "HOU", "BOS")
last_ten_WS_city <- c("New York", "San Francisco", "St. Louis", "San Francisco", "Boston",
                      "San Francisco", "Kansas City", "Chicago", "Houston", "Boston")
last_ten_WS_state <- c("New York", "California","Missouri", "California", "Massachusetts", 
                       "California", "Missouri", "Illinois", "Texas", "Massachusetts")
last_ten_WS_country <- c("US", "US", "US", "US","US", "US", "US", "US", "US", "US")

last_ten_WS_data <- data.frame(last_ten_WS_year, last_ten_WS_team, last_ten_WS_city, last_ten_WS_state,
                               last_ten_WS_country)

colnames(last_ten_WS_data) <- c("Year", "Team", "City", "State", "Country")
last_ten_WS_data

geo_chart <- 
  gvisGeoChart(last_ten_WS_data,
               locationvar = "State",
               colorvar = "Year",
               hovervar = "Team",
               options = list(
                 region = "US",
                 displayMode = "region",
                 resolution = "provinces",
                 width = 800,
                 height = 600,
                 gvis.editor = "Edit me!"
               ))
plot(geo_chart)   # the darker the color the more recent their championship was


######################## PART F ########################
# EXTRA CREDIT 

SeriesPost %>%
  filter(yearID == 2004)

###

col1 <- c("ALDS: BOS",
          "ALDS: ANA",
          "ALCS: BOS",
          "ALDS: MIN",
          "ALDS: NYA",
          "ALCS: BOS vs. NYA",
          "ALCS: NYA",
          "NLDS: SLN",
          "NLDS: HOU",
          "NLDS: ATL",
          "NLDS: LAN",
          "NLCS: SLN",
          "NLCS: HOU",
          "NLCS: SLN vs HOU",
          "WS: SLN vs BOS")

col2 <- c("ALCS: BOS",
          "ALCS: BOS",
          "ALCS: BOS vs. NYA",
          "ALCS: NYA",
          "ALCS: NYA",
          "WS: SLN vs BOS",
          "ALCS: BOS vs. NYA",
          "NLCS: SLN",
          "NLCS: HOU",
          "NLCS: HOU",
          "NLCS: SLN",
          "NLCS: SLN vs HOU",
          "NLCS: SLN vs HOU",
          "WS: SLN vs BOS",
          "WS: BOS")

col3 <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)


data.sank <- data.frame(col1, col2, col3)
colnames(data.sank) <- c("From", "To", "Weight")
data.sank

chart_sank <-
  gvisSankey(data.sank,
             from = "From",
             to = "To",
             weight = "Weight",
             options = list(
               sankey = "{link: {color: {fill: '#d799ae' } }, 
               node: { color: { fill: '#00ff00'},
               label: { color: '#871b47'}} }" ))

plot(chart_sank)

















