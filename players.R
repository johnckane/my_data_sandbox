player_list = c("LeGarrette Blount",
                "Danny Amendola",
                "Brandon Lloyd",
                "Deion Branch",
                "Brandon Tate",
                "Danny Woodhead",
                "Fred Taylor",
                "BenJarvus Green-Ellis",
                "Sammy Morris",
                "Wes Welker",
                "Ben Watson",
                "Jabar Gaffney")

players2 <- players %>%
  select(player,fname,lname) %>%
  mutate(name = paste(fname," ",lname,sep="")) %>%
  filter(name %in% player_list)

player_plays <- sqldf('
               select
                a.gid,
                a.pid,
                a.off,
                a.type,
                b.bc as player
               from
                core as a,
                rush as b
               where
                a.type = "RUSH"
               and a.pid = b.pid 
               union all
               select
                a.gid,
                a.pid,
                a.off,
                a.type,
                c.trg as player
               from
                core as a,
                comp_sacks as b,
                pass as c
               where
                   a.pid = b.pid
               and a.type = "PASS"
               and a.pid = c.pid
               ')

player_plays2 <- sqldf('
                       select
                        a.*,
                        b.name
                       from
                        player_plays as a,
                        players2 as b
                       where
                        a.player = b.player')

## Now limit the plays to those occurring after 2007 and later
player_plays_post07 <- sqldf('
                             select
                              a.*,
                              b.seas as season
                             from
                              player_plays2 as a,
                              games as b
                             where
                              a.gid = b.gid
                             ')
player_plays_post07 <- sqldf('
                             select
                              *
                             from
                              player_plays_post07
                             where
                              season >= 2007')
## Now bring in the fumble data
players_final <- sqldf('
                       select
                        a.*,
                        b.fumble
                       from
                        player_plays_post07 as a
                       left join
                       (select
                         pid,
                         1 as fumble
                       from
                        fumbles) as b
                       on
                        a.pid = b.pid                      
                       ')
write.csv(players_final,"players_final.csv")
players_final$fumble <- ifelse(is.na(players_final$fumble),0,1)

players_final$NE <- ifelse(players_final$off=="NE",1,0)
players_final2 <- players_final %>%
  group_by(name,NE,fumble) %>%
  summarise(total_plays = n())
  
cast_player <- dcast(players_final2,name~NE+fumble,value.var="total_plays",fill=0)
colnames(cast_player) <- c("player",
                           "non_ne_non_fumble",
                           "non_ne_fumble",
                           "ne_non_fumble",
                           "ne_fumble")
cast_player <- cast_player %>%
  mutate(non_ne_fumble_rate_100 = 100*non_ne_fumble/(non_ne_non_fumble+non_ne_fumble),
         ne_fumble_rate_100 = 100*ne_fumble/(ne_non_fumble + ne_fumble))

melt_player <- melt(cast_player,
                    id.vars = "player",
                    measure.vars = c("non_ne_fumble_rate_100",
                                     "ne_fumble_rate_100"))
melt_player$var_id <- ifelse(melt_player$variable == "non_ne_fumble_rate_100",
                             "Other Teams",
                             "New England")


## now get samples sizes for each bar for each player
sample_sizes <- cast_player %>%
  mutate(ne_total = ne_non_fumble + ne_fumble,
         non_ne_total = non_ne_non_fumble + non_ne_fumble)
melt_sample_sizes <- melt(sample_sizes,
                          id.vars = "player",
                          measure.vars = c("ne_total","non_ne_total"))
melt_sample_sizes$var_id <- ifelse(melt_sample_sizes$variable == "ne_total",
                                   "New England",
                                   "Other Teams")
melt_sample_sizes$label_var <- paste("n = ",melt_sample_sizes$value)

plot7 <- ggplot(data=melt_player,
                aes(x=var_id,
                    y=value))
plot7 +
  geom_bar(stat="identity") +
  facet_wrap(~player,ncol = 3) +
  scale_y_continuous("Fumbles per 100 Plays",limits = c(0,5)) +
  geom_text(data=melt_sample_sizes,
            aes(x=var_id,
                y=4,
                label = label_var)) +
  scale_x_discrete("Team") +
  ggtitle("Players Fumble Rates, Playing for Patriots vs. Other Teams \n 2007-2013")
