setwd('/home/john/NFL/NFLData_2000-2014/csv')
players <- read.csv("PLAYER.csv", header = TRUE, stringsAsFactors = FALSE)
passing <- read.csv("PASS.csv", header = TRUE, stringsAsFactors = FALSE)
comp <- read.csv("COMP.csv", header = TRUE, stringsAsFactors = FALSE)
core <- read.csv("CORE.csv", header = TRUE, stringsAsFactors = FALSE)

colnames(players) <- tolower(colnames(players))
colnames(passing) <- tolower(colnames(passing))
colnames(comp) <- tolower(colnames(comp))
comp$complete <- 1
colnames(core) <- tolower(colnames(core))
library(dplyr)

filter(players,lname=="Redmond")

## Super Bowl 36, Tom Brady completion to "J.R. Redmond" player "JR-0600"
brady_redmond <- filter(passing, trg == "JR-0600", psr == "TB-2300")

target_completion <- left_join(x  = brady_redmond,
                               y  = comp,
                               by = "pid")
target_completion$complete <- ifelse(is.na(target_completion$complete),
                                     0,
                                     1)
sum(target_completion$complete)
19/27

## Super Bowl 38, John Kasay kicks the ball out of bounds.
filter(players, lname == "Kasay")
## player is "JK-0200"
kickoff <- read.csv("KICKOFF.csv", header = TRUE, stringsAsFactors = FALSE)
penalty <- read.csv("PENALTY.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(kickoff) <- tolower(colnames(kickoff))
colnames(penalty) <- tolower(colnames(penalty))
filter(kickoff, kicker == "JK-0200")

## find JK kickoffs with OB penalty
inner_join(filter(kickoff, kicker == "JK-0200"), penalty, by = "pid")
table(penalty$desc)
filter(penalty, desc == "Kickoff Out of Bounds")

## Having trouble finding that play.
game <- read.csv("GAME.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(game) <- tolower(colnames(game))
str(game)

core <- read.csv("CORE.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(core) <- tolower(colnames(core))
core_sb38 <- filter(core, gid == 1052)

filter(kickoff, pid == 171512)
filter(penalty, pid == 171512)
filter(kickoff, knet == 30, ktb == 'N', kr == "XX-0000", kry == 0)
filter(kickoff, knet == 30, ktb == 'N', kry == 0)
filter(kickoff, knet == 30, ktb == 'N', kry == 0, kicker == "JK-0200")
filter(kickoff, kicker == "JK-0200")
9/399

## Probability of Tyree catch ##
filter(players, lname == "Tyree")
filter(passing, trg == "DT-2000")
filter(passing, trg == "DT-2000", psr == "EM-0200")

tyree_deep_middle <- filter(passing, trg == "DT-2000", loc == "DM")

manning_tyree <- filter(passing, trg == "DT-2000", psr == "EM-0200")

target_completion <- left_join(x  = manning_tyree,
                               y  = comp,
                               by = "pid")
target_completion$complete <- ifelse(is.na(target_completion$complete),
                                     0,
                                     1)
sum(target_completion$complete)
42/73
## Manning Tyree, deep passes
manning_tyree_deep <- filter(target_completion, loc %in% c("DM","DL","DR"))
4/9

manning_deep <- left_join(x  = filter(passing, psr == "EM-0200", loc %in% c("DM","DR","DL")),
                          y  = comp,
                          by = "pid")
manning_deep_middle <- left_join(x  = filter(passing, psr == "EM-0200", loc == "DM"),
                                 y  = comp,
                                 by = "pid")

manning_deep$complete <- ifelse(is.na(manning_deep$complete),
                                0,
                                1)
sum(manning_deep$complete)
410/1033

manning_deep_middle <- left_join(x  = filter(passing, psr == "EM-0200", loc == "DM"),
                                 y  = comp,
                                 by = "pid")

manning_deep_middle$complete <- ifelse(is.na(manning_deep_middle$complete),
                                0,
                                1)
sum(manning_deep_middle$complete)
133/300

## Probability of Manningham catch in Super Bowl 46 ##
filter(players, lname == "Manningham")

manning_manningham <- left_join(x  = filter(passing, trg == "MM-0700", psr == "EM-0200"),
                               y  = comp,
                               by = "pid")
manning_manningham$complete <- ifelse(is.na(manning_manningham$complete),
                                      0,
                                      1)
sum(manning_manningham$complete)
169/291

manning_manningham_deep <- filter(manning_manningham, loc %in% c("DM","DL","DR"))
sum(manning_manningham_deep$complete)
36/96

manning_manningham_deepleft <- filter(manning_manningham, loc == "DL")
sum(manning_manningham_deepleft$complete)
16/42

## Kearse catch ##
filter(players, lname == "Kearse") # JK-0250
filter(players, lname == "Wilson") # RW-3850

wilson_kearse <- left_join(x  = filter(passing, trg == "JK-0250", psr == "RW-3850"),
                           y  = comp,
                           by = "pid")
wilson_kearse$complete <- ifelse(is.na(wilson_kearse$complete),
                                      0,
                                      1)
sum(wilson_kearse$complete)
77/139

wilson_kearse_deep <- filter(wilson_kearse, loc %in% c("DM","DL","DR"))
sum(wilson_kearse_deep$complete)
25/59

wilson_kearse_deepright <- filter(wilson_kearse, loc == "DR")
sum(wilson_kearse_deepright$complete)
14/34

## How many passes from 1 yard line are intercepted ##
core_passing <- inner_join(x  = core,
                           y  = passing,
                           by = "pid")
core_passing_1yd <- filter(core_passing,
                           ytg == 1,
                           yfog == 99)
intercept <- read.csv("INTERCPT.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(intercept) <- tolower(colnames(intercept))
intercept$intercept <- 1

scoring <- read.csv("SCORING.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(scoring) <- tolower(colnames(scoring))
scoring$touchdown <- 1

int_1yd <- left_join(x  = core_passing_1yd,
                     y  = intercept,
                     by = "pid")
int_1yd$intercept <- ifelse(is.na(int_1yd$intercept),
                            0,
                            1)
one_yard_pass_plays <- left_join(x  = int_1yd,
                                 y  = scoring,
                                 by = "pid")
one_yard_pass_plays$touchdown <- ifelse(is.na(one_yard_pass_plays$touchdown),
                                        0,
                                        1)
sum(one_yard_pass_plays$intercept)
41/1318

sum(one_yard_pass_plays$touchdown)
683/1318
#incomplete
(1318-(41+683))/1318
