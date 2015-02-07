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
  ggtitle("All NFL Teams \n Fumbles per 100 Plays by Season") +
  annotate("text", x = 2003.3, y = 0.25, label = "Kansas City", color='Gray') +
  annotate("text", x = 2010, y = 0.4, label = "New England", color = 'Red') +
  annotate("text", x = 2011, y =  2.5, label = "League Average", color = 'Black')

## Get NE before and after data
yearly_fumble_rates$post <- ifelse(yearly_fumble_rates$season<=2006,0,1)
yearly_fumble_rates %>%
  filter(off=="NE") %>%
  group_by(post) %>%
  summarise(total_plays = sum(fep),
            total_fumbles = sum(fumbles)) %>%
  mutate(ppf = total_plays/total_fumbles,
         fumble_rate_100 = 100*total_fumbles/total_plays)
#################################################################################
library(reshape2)
## all teams before and after 2006
yearly_fumble_rates$post <- ifelse(yearly_fumble_rates$season<=2006,0,1)
pre_post_2006 <- yearly_fumble_rates %>%
  group_by(off,post) %>%
  summarise(total_plays = sum(fep),
            total_fumbles = sum(fumbles)) %>%
  mutate(fumble_rate_100 = 100*total_fumbles/total_plays)

reshape_pre_post_2006 <- dcast(pre_post_2006,off~post,value.var="fumble_rate_100")
colnames(reshape_pre_post_2006) <- c("team","pre","post")
reshape_pre_post_2006 <- reshape_pre_post_2006 %>%
  mutate(delta = post - pre)

reshape_pre_post_2006$team2 <- factor(reshape_pre_post_2006$team,
                                      levels = reshape_pre_post_2006[order(reshape_pre_post_2006$delta),
                                                                     "team"])
plot2 <- ggplot(data=reshape_pre_post_2006,aes(x=delta,y=team2))
plot2 +
  geom_point() +
  scale_x_continuous("Change in Average Plays per Fumble") +
  scale_y_discrete("Team") +
  geom_vline(xintercept = 0, linetype="dashed")  +
  geom_vline(xintercept = mean(reshape_pre_post_2006$delta)) +
  ggtitle("Change in Average Fumbles per Play \n 2007-2013 vs. 2000-2006") +
  geom_segment(aes(x = -0.5, y = 28, xend = -0.35, yend = 28), 
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text",
           x=-0.7,
           y=28,
           label = "League Average")

## Degree of improvement over the average during that time for ATL, NE and
## Detroit
reshape_pre_post_2006$delta_centered <- reshape_pre_post_2006$delta - mean(reshape_pre_post_2006$delta)
filter(reshape_pre_post_2006,team2 %in% c('ATL', 'NE', 'DET'))

## As the plot indicates, Detroit worsened up by as many fumbles per 100 plays 
## than either Atlanta or NE improved
reshape_pre_post_2006$team3 <- factor(reshape_pre_post_2006$team,
                                      levels = reshape_pre_post_2006[order(reshape_pre_post_2006$post),
                                      "team"])
plot3 <- ggplot(data=reshape_pre_post_2006,aes(x=post,y=team3))
plot3 +
  geom_point() +
  scale_x_continuous("Fumbless per 100 Play") +
  scale_y_discrete("Team") +
  geom_vline(xintercept = mean(reshape_pre_post_2006$post)) +
  ggtitle("Fumbles per 100 Plays 2007-2013") +
  geom_segment(aes(x = 1.93, y = 28, xend = 2.13, yend = 28), 
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text",
           x=1.71,
           y=28,
           label = "League Average")
## look at NE vs ARI 
reshape_pre_post_2006$post_centered <- reshape_pre_post_2006$post - mean(reshape_pre_post_2006$post)
reshape_pre_post_2006$post_z <- reshape_pre_post_2006$post_centered/sd(reshape_pre_post_2006$post)
filter(reshape_pre_post_2006,team3 %in% c('NE', 'ATL','ARI'))
qqnorm(y=reshape_pre_post_2006$post)
## NE chances at being so good
1/pnorm(-2.370150,0,1)
## ATL
1/pnorm(-1.936586,0,1)
## ARI chances at being so bad
1/(1-pnorm(1.987171))
## So even if these were normal (questionable, seems to have a high peak). But 
## these aren't random variables. I think most readers will acknowledge that
## ball security is a skill, and not purely random. 

#################################################################################
## Outlier seasons happen. Just look at Kansas City in 2002. The raw data
## certainly speaks to NE lapping the competition when it comes to performance
## in this area. I discussed earlier the impact of outliers here. One way to 
## attempt to remove this possibility is simply rank each team each season by
## the plays per fumble metric. 
## visually, not as compelling but the relative difference is the same when 
## looking at x vs. 1/x. 
## I don't believe in throwing out data. 
season_ranks <- yearly_fumble_rates %>%
  group_by(season) %>%
  mutate(fumble_rank = rank(fumble_rate_100,ties.method="min"))



## Let's summarise average ranks, first by 2000-2006, then from 2007-2013
## The rule change went into effect for the 2006 season, though the marked
## difference was really for the 2007 season onward
fumble_rank_00_06 <- filter(season_ranks,season <= 2006) %>%
  group_by(off) %>%
  summarise(avg_rank1   = mean(fumble_rank),
            med_rank1   = median(fumble_rank))
fumble_rank_07_13 <- filter(season_ranks,season >= 2007) %>%
  group_by(off) %>%
  summarise(avg_rank2   = mean(fumble_rank),
            med_rank2   = median(fumble_rank))
fumble_ranks_compare <- sqldf('
                              select
                                a.*,
                                b.avg_rank2,
                                b.med_rank2,
                                b.avg_rank2 - a.avg_rank1 as avg_rank_change,
                                b.med_rank2 - a.med_rank1 as med_rank_change
                              from
                                fumble_rank_00_06 as a,
                                fumble_rank_07_13 as b
                              where
                                a.off = b.off
                              order by
                                avg_rank_change')
## First look at how NE did after 2006.
library(gridExtra)

fumble_rank_07_13$off2 <- factor(fumble_rank_07_13$off,
                                 levels = fumble_rank_07_13[order(fumble_rank_07_13$avg_rank2),1]$off)


## Average League Ranking
plot4 <- ggplot(data=fumble_rank_07_13,aes(x=avg_rank2,y=off2))
plot4 +
  geom_point() +
  scale_x_continuous("Average League Rank") +
  scale_y_discrete("Team") +
  ggtitle("Average League Rank in Plays per Fumble \n 2007-2013")

## Improvements in Ranking
fumble_ranks_compare$off2 <- factor(fumble_ranks_compare$off,
                                    levels = fumble_ranks_compare[order(fumble_ranks_compare$avg_rank_change),1])
plot5 <- ggplot(data=fumble_ranks_compare,aes(x=avg_rank_change,y=off2))
plot5 +
  geom_point() +
  scale_x_continuous("Average League Rank Change") +
  scale_y_discrete("Team") +
  ggtitle("Average League Rank Change \n 2007-2013 vs 2000-2006")


