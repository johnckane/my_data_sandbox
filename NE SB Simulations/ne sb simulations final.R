library(dplyr)
library(ggplot2)

set.seed(2015)
sb36 <- replicate(10000, sample(c(0,1), 1, c(.297, .703), replace = FALSE))
sb38 <- replicate(10000, sample(c(0,1), 1, c(.317, .683), replace = FALSE))
sb39 <- replicate(10000, sample(c(0,1), 1, c(.177, .823 ), replace = FALSE)) 
sb42 <- replicate(10000, sample(c(0,1), 1, c(.211, .789 ), replace = FALSE))
sb46 <- replicate(10000, sample(c(0,1), 1, c(.310, .690 ), replace = FALSE))
sb49 <- replicate(10000, sample(c(0,1), 1, c(.493, .507 ), replace = FALSE))

sb_simulations <- data.frame(sb36, sb38, sb39, sb42, sb46, sb49)

sb_simulations <- mutate(sb_simulations, 
                         sb_wins = sb36 + sb38 + sb39 + sb42 + sb46 + sb49)
table(sb_simulations$sb_wins)/10000*100

results <- data.frame(table(sb_simulations$sb_wins)/10000*100)
colnames(results) <- c("wins", "chance")

results$labels <- paste(results$chance, "%", sep="")

plot <- ggplot(data=results,aes(x = wins, y = chance, label = labels))
plot + 
    geom_bar(stat="identity") +
    scale_x_discrete("Super Bowl Wins") +
    scale_y_continuous("",lim=c(0,35)) +
    geom_text(vjust=-1) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
    ggtitle("Distriubtion of New England Super Bowls Won")

    
