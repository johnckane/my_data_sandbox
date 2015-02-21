setwd("/home/john/NFL/NFLData_2000-2013/")
wet_conditions <- c("Flurries","Light Snow","Snow","Light Rain","Rain")

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
#' filter out some unnecessary rows
reg_season_games <- games %>% filter(week >= 1 & week <= 17 )
reg_season_games_wet <- reg_season_games %>% filter(cond %in% wet_conditions)
reg_season_games_dry <- reg_season_games %>% filter(cond  %!in% wet_conditions)

ne_games <- filter(reg_season_games,v=="NE"|h=="NE")
atl_games <- filter(reg_season_games,v=="ATL"|h=="ATL")

ne_wet_games <- filter(reg_season_games_wet,v=="NE"|h=="NE")
atl_wet_games <- filter(reg_season_games_wet,v=="ATL"|h=="ATL")

### Look at post 2006
reg_season_games_post06 <- reg_season_games %>% filter(seas >= 2007)
reg_season_games_post06$wet <-
  ifelse(reg_season_games_post06$cond %in% wet_conditions,
         1,
         0)

ne_games_post06 <- filter(reg_season_games_post06,v=="NE"|h=="NE")
atl_games_post06 <- filter(reg_season_games_post06,v=="ATL"|h=="ATL")

ne_wet_games_post06 <- filter(reg_season_games_post06,wet==1,v=="NE"|h=="NE")
atl_wet_games_post06 <- filter(reg_season_games_post06wet==1,v=="ATL"|h=="ATL")

### Compare NE fumble rates post 2006 in wet games versus non-wet games
#### match up fumble plays with game id, then that data with dry or wet conditions
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
                            b.wet,
                            a.fum
                            from
                            fumble_plays as a,
                            reg_season_games_post06 as b
                            where 
                            a.gid = b.gid
                            ')
## add up all fumbles by team and year
fumbles_team <- fumble_plays_games %>%
  group_by(off,wet) %>%
  summarise(fumbles = n())

all_plays <- sqldf('
                   select
                   a.seas,
                   a.week,
                   a.wet,
                   b.gid,
                   b.pid,
                   b.off,
                   b.type
                   from
                   reg_season_games_post06 as a
                   left join
                   plays as b
                   on
                   a.gid = b.gid                    
                   ')
                    

team_season_fumble_eligible_plays <- 
  all_plays %>%
  group_by(off,wet) %>%
  summarise(fep = n()) # 'fep' stands for 'fumble eligible plays'

fumble_summary <- sqldf('
                        select
                          b.off,
                          a.fumbles,
                          b.fep,
                          b.wet
                        from
                          team_season_fumble_eligible_plays as b
                        left join
                          fumbles_team as a
                        on
                          a.off = b.off
                        and a.wet = b.wet')
fumble_summary$fumbles <- ifelse(is.na(fumble_summary$fumbles),
                             0,
                             fumble_summary$fumbles)

grouped_fumble_rates <- fumble_summary %>%
  mutate(fumble_rate_100 = 100*fumbles/fep)

library(reshape2)
cast_rates <- dcast(grouped_fumble_rates,
                    off~wet,
                    value.var = "fumble_rate_100")

cast_fep <- dcast(grouped_fumble_rates,
                  off~wet,
                  value.var = "fep")
colnames(cast_rates) <- c("team","dry_rate","wet_rate")
colnames(cast_fep)   <- c("team","dry_fep","wet_fep")

cast_data <- sqldf('
                   select
                    a.team,
                    b.dry_fep,
                    a.dry_rate,
                    b.wet_fep,
                    a.wet_rate
                   from
                    cast_rates as a,
                    cast_fep as b
                   where
                    a.team = b.team')
#write.csv(cast_data,"fumble_rates_by_weather.csv")

cast_data$team2 <- factor(cast_data$team,
                           levels = cast_data[order(cast_data$wet_rate),
                                               "team"])

plot6 <- ggplot(data=cast_data,aes(x=wet_rate,y=team2))
plot6 +
  geom_point() +
  scale_x_continuous("Fumbles per 100 Plays in Wet Weather") +
  scale_y_discrete("Team") +
  ggtitle("Fumbles per 100 Plays in Wet Weather \n 2007-2013")
