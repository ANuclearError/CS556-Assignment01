# This code handles the visualisation of the Steam data, presenting various
# parts of the data.

library(ggplot2)


steam <- read.csv("data/top100in2weeks.csv")

# ScoreRank visualisation
ggplot(steam, aes(x=ScoreRank)) +
	geom_histogram(bins=10, col="red", fill="green", alpha=.25,
		breaks=seq(0, 100, by=10)) +
	scale_x_continuous(breaks=seq(0, 100, by=10), limits=c(0,100)) +
	scale_y_continuous(breaks=seq(0, 24, by=4), limits=c(0, 24)) +
	stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.5) +
	stat_bin(bins = 10, breaks=seq(0, 100, by=10), geom="text", vjust=-1,
		aes(y=..count..,label=..count..))