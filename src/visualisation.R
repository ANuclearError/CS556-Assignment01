# This code handles the visualisation of the Steam data, presenting various
# parts of the data.

library(ggplot2)


steam <- read.csv("data/top100in2weeks.csv")

# Generate tags dataframe.
tags <- data.frame(v=numeric(201), w=numeric(201), x=numeric(201),
    y=numeric(201), z=numeric(201));
tags[,1] <- colnames(steam[,-(1:16)])
tags[,2] <- t(steam[101,-(1:16)])
tags[,3] <- t(steam[102,-(1:16)])
tags[,4] <- t(steam[103,-(1:16)])
tags[,5] <- t(steam[104,-(1:16)])
colnames(tags) = c("Tag", "Tagged", "RelTagged", "AvgScore", "RelAvgScore")

# Handles steam data frame
steam <- steam[1:100,]
steam$ScoreGrade <- cut(steam$ScoreRank, seq(0, 100, by=10))
steam$PriceBand <- cut(steam$Price, seq(0, 6000, by=500))


# ScoreRank visualisation
score_rank_hist <- ggplot(steam, aes(x=ScoreRank)) +
	geom_histogram(bins=10, col="red", fill="green", alpha=.25,
		breaks=seq(0, 100, by=10)) +
	scale_x_continuous(breaks=seq(0, 100, by=10), limits=c(0,100)) +
	scale_y_continuous(breaks=seq(0, 24, by=4), limits=c(0, 24)) +
	stat_bin(bins = 10, breaks=seq(0, 100, by=10), geom="text", vjust=-1,
		aes(y=..count..,label=..count..))


# Price visualisation
price_hist <- ggplot(steam, aes(x=Price)) +
    geom_histogram(bins=12, col="red", fill="green", alpha=.25,
        breaks=seq(0, 6000, by=500)) +
    scale_x_continuous(breaks=seq(0, 6000, by=500), limits=c(0,6000)) +
    scale_y_continuous(breaks=seq(0, 32, by=4), limits=c(0, 32)) +
    stat_bin(bins=12, breaks=seq(0, 6000, by=500), geom="text", vjust=-1,
        aes(y=..count..,label=..count..))


# Score/Price visualisation
score_price_rects <- data.frame(xstart=c(0,50,0,50), xend=c(50,100,50,100),
    ystart=c(0,0,3000,3000), yend=c(3000,3000,6000,6000), col=letters[1:4])

score_price <- ggplot() +
    geom_point(data=steam, aes(ScoreRank, Price)) +
    geom_smooth() +
    geom_rect(data=score_price_rects,
        aes(xmin=xstart, xmax=xend, ymin=ystart, ymax=yend, fill=col, alpha=.25)) + 
    scale_x_continuous(breaks=seq(0, 100, by=10), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 6000, by=500), limits=c(0,6000)) +
    theme(legend.position="none")


# Publisher visualisation
pub <- aggregate(steam$ScoreRank, by=list(steam$Publisher), length)
pub[3] <- aggregate(steam$ScoreRank, by=list(steam$Publisher), mean)[2]
colnames(pub) <- c("Publisher", "Count", "AvgScore")
pub_bar <- ggplot(pub, aes(x=Publisher, y=AvgScore)) +
    geom_bar(stat="identity", col="red", fill="green", alpha=.25) +
    geom_text(aes(label=round(AvgScore)), vjust=-1)


# Tag visualisation
tag_score <- ggplot(tags, aes(Tagged, AvgScore)) + geom_point() + geom_smooth()
tag_score_rel <- ggplot(tags, aes(RelTagged, RelAvgScore)) + geom_point() +
    geom_smooth()


# The playtime point graphs
avg_f <- ggplot(steam, aes(x=AverageForever, y=ScoreRank)) +
    geom_point() + geom_smooth()
avg_t <- ggplot(steam, aes(x=Average2Weeks, y=ScoreRank)) +
    geom_point() + geom_smooth()
med_f <- ggplot(steam, aes(x=MedianForever, y=ScoreRank)) +
    geom_point() + geom_smooth()
med_t <- ggplot(steam, aes(x=Median2Weeks, y=ScoreRank)) +
    geom_point() + geom_smooth()

# Owners
owners <- ggplot(steam, aes(x=Owners, y=ScoreRank)) +
    geom_point() + geom_smooth()
players_f <- ggplot(steam, aes(x=PlayersForever, y=ScoreRank)) +
    geom_point() + geom_smooth()
players_t <- ggplot(steam, aes(x=Players2Weeks, y=ScoreRank)) +
    geom_point() + geom_smooth()

save_all <- function() 
    ggsave("doc/report/img/score_rank.png", score_rank_hist) +
    ggsave("doc/report/img/price.png", price_hist) +
    ggsave("doc/report/img/score_price.png", score_price) +
    ggsave("doc/report/img/publishers.png", pub_bar) +
    ggsave("doc/report/img/tag_score.png", tag_score) +
    ggsave("doc/report/img/tag_score_rel.png", tag_score_rel) +
    ggsave("doc/report/img/avg_f.png", avg_f) +
    ggsave("doc/report/img/avg_t.png", avg_t) +
    ggsave("doc/report/img/med_f.png", med_f) +
    ggsave("doc/report/img/med_t.png", med_t) +
    ggsave("doc/report/img/owners.png", owners) +
    ggsave("doc/report/img/players_f.png", players_f) +
    ggsave("doc/report/img/players_t.png", players_t)
# save_all()