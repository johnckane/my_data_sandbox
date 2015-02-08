library(dplyr)
library(sqldf)
library(ggplot2)
setwd("/home/john/NFL/NFLData_2000-2013/")

fumbles     <- read.csv("FUMBLES.csv", header = TRUE, stringsAsFactors = FALSE)
core        <- read.csv("CORE.csv", header = TRUE, stringsAsFactors = FALSE)
games       <- read.csv("GAMES.csv", header = TRUE, stringsAsFactors = FALSE)
players     <- read.csv("PLAYERS.csv", header = TRUE, stringsAsFactors = FALSE)
sacks       <- read.csv("SACKS.csv", header = TRUE, stringsAsFactors = FALSE)
completions <- read.csv("COMPS.csv", header = TRUE, stringsAsFactors = FALSE)
pass        <- read.csv("PASS.csv", header = TRUE, stringsAsFactors = FALSE)
rush        <- read.csv("RUSH.csv", header = TRUE, stringsAsFactors = FALSE)

colnames(fumbles)     <- tolower(colnames(fumbles))
colnames(core)        <- tolower(colnames(core))
colnames(games)       <- tolower(colnames(games))
colnames(players)     <- tolower(colnames(players))
colnames(sacks)       <- tolower(colnames(sacks))
colnames(completions) <- tolower(colnames(completions))
colnames(pass)        <- tolower(colnames(pass))
colnames(rush)        <- tolower(colnames(rush))

#' filter out some unnecessary rows before we start joining tables
core <- core %>% filter(type=='PASS'|type=='RUSH')
games <- games %>% filter(week >= 1 & week <= 17)

# union complete passes and sacks, that way we can isolate plays when fumbles
# were possible

comp_sacks <- sqldf('
                    select
                      pid
                    from
                      completions
                    union all
                    select
                      pid
                    from
                      sacks')
## Now union all rushing plays with all pass plays that were either a 
## completion or a sack
plays <- sqldf('
               select
                gid,
                pid,
                off,
                type
               from
                core
               where
                type = "RUSH"
               union all
               select
                *
               from
                (select
                  a.gid,
                  a.pid,
                  a.off,
                  a.type
                from
                 core as a,
                 comp_sacks as b
                where
                 a.pid = b.pid
                and a.type = "PASS")
               ')

# first, for the sake of memory, join plays with fumbles

fumble_plays <- sqldf('
                      select
                        b.gid,
                        a.pid,
                        b.off,
                        b.type,
                        a.fum
                      from
                        fumbles as a,
                        plays as b
                      where
                        a.pid = b.pid
                      ')

## Now join with game data
fumble_plays_games <- sqldf('
                          select
                            a.gid,
                            b.seas as season,
                            b.week,
                            a.pid,
                            a.off,
                            a.type,
                            b.cond,
                            a.fum
                          from
                            fumble_plays as a,
                            games as b
                          where 
                              a.gid = b.gid
                          ')



## Now for each team and year we need plays from scrimmage fumbles or not,
## to determine fumble rates. First step is to match up play data with game
## data so that we can assign each play to a game, week and season

## add up all fumbles by team and year
fumbles_year_team <- fumble_plays_games %>%
  group_by(season,off) %>%
  summarise(fumbles = n())

all_plays <- sqldf('
                    select
                      b.seas,
                      b.week,
                      a.gid,
                      a.pid,
                      a.off,
                      a.type
                    from
                      plays as a
                    left join
                      games as b
                    on
                      a.gid = b.gid
                    ')
                    

team_season_fumble_eligible_plays <- 
  all_plays %>%
  group_by(seas,off) %>%
  summarise(fep = n()) # 'fep' stands for 'fumble eligible plays'


## now get the rates
fumble_summary <- sqldf('
                        select
                          a.season,
                          a.off,
                          a.fumbles,
                          b.fep
                        from
                          fumbles_year_team as a,
                          team_season_fumble_eligible_plays as b
                        where
                          a.off = b.off
                        and a.season = b.seas')

yearly_fumble_rates <- fumble_summary %>%
  mutate(fumble_rate_100 = 100*fumbles/fep)

write.csv(yearly_fumble_rates,"yearly_fumble_rates.csv")

non_ne <- filter(yearly_fumble_rates, off != 'NE' )
ne_only <- filter(yearly_fumble_rates,off == 'NE')

yearly_avg <- fumble_summary %>%
  group_by(season) %>%
  summarise(total_plays = sum(fep),
            total_fumbles = sum(fumbles),
            total_fumble_rate_100     = 100*total_fumbles/total_plays) %>%
  mutate(off = 'AVG')

plot1 <- ggplot(data=non_ne,
                aes(x=season,y=fumble_rate_100,group=off))
plot1 + 
  geom_line(color="Gray") +
  geom_line(data=ne_only,aes(x=season,y=fumble_rate_100),color='Red') +
  geom_line(data=yearly_avg,aes(x=season,y=total_fumble_rate_100),color = 'Black') +
  geom_vline(xintercept=2006,linetype = "dashed") +
  scale_x_continuous("Season",
                     breaks=seq(from=2000,to=2013,by=1)) +
  scale_y_continuous("Fumbles per 100 Plays") +
  ggtitle("All NFL Teams /n Fumbles per 100 Plays by Season") +
  annotate("text", x = 2003.3, y = 0.25, label = "Kansas City", color='Gray') +
  annotate("text", x = 2010, y = 0.4, label = "New England", color = 'Red') +
  annotate("text", x = 2011, y =  2.5, label = "League Average", color = 'Black')

## Get NE before and after data

yearly_fumble_rates %>%
  filter(off=="NE") %>%
  group_by(post) %>%
  summarise(total_plays = sum(fep),
            total_fumbles = sum(fumbles)) %>%
  mutate(ppf = total_plays/total_fumbles,
         fumble_rate_100 = 100*total_fumbles/total_plays)
