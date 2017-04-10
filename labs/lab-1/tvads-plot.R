library(ggplot2)
library(gridExtra)
library(scales)

# creating a vector of time from 8am to 9am
d <- read.csv("fake-tvads-data.csv")
summary(d)
d$t <- strptime(as.character(d$t), "%Y-%m-%d %H:%M:%S")
summary(d)

da <- subset(d, type=="audience")
da <- droplevels(da)

d <- subset(d, type %in% c("tune_in", "tune_out"))
d <- droplevels((d))

events <- c(120, 240, 300, 420, 480, 564, 636)
num_events <- as.numeric(d$t[events])
pos_events <- as.POSIXct(num_events, origin = '1970-01-01')

# http://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet...
# http://docs.ggplot2.org/dev/vignettes/themes.html
# http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=4

theme2 <- theme(
  axis.text = element_text(size = 10),
	axis.text.y = element_blank(),
	axis.title = element_blank(),
	axis.ticks.y = element_blank(),
  legend.background = element_rect(fill = "grey90", color="grey"),
  legend.position = c(0.10, 0.70),
  legend.title = element_blank(),
  legend.key = element_rect(fill = "grey90", colour = "grey90"),
  legend.margin = margin(unit = "cm"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white", color = "grey40", size=0.5),
	plot.margin = unit(c(0,1,1,1),"cm")
)

pl <- ggplot(d, aes(x=t, y=value, color=type)) + geom_point(size=0.1)
pl <- pl + geom_line(size=0.6)
pl <- pl + scale_color_manual(values=c("#4daf4a", "#377eb8"), labels=c("# of STB Tuning In", "# of STB Tuning Out"))
pl <- pl + theme2
pl <- pl + geom_vline(xintercept = num_events, colour = "grey50", size = 0.6)

theme1 <- theme(axis.text.y=element_blank(), axis.text.x=element_blank(),
	axis.title = element_blank(),
	axis.ticks = element_blank(),
	legend.background = element_rect(fill = "grey90", color="grey"),
	legend.position = c(0.11, 0.90),
	legend.title = element_blank(),
	legend.key = element_rect(fill = "grey90", colour = "grey90"),
	legend.margin = margin(unit = "cm"),
	panel.grid.major = element_line(colour = "grey90", size=0.5, linetype="dashed"),
  panel.grid.minor = element_blank(),
	panel.background = element_rect(fill = "white", color = "grey40", size=0.5),
	plot.margin = unit(c(1,1,-0.1,1),"cm")
)

pl1 <- ggplot(da, aes(x=t, y=value, color=type))
pl1 <- pl1 + geom_line(mapping = aes(color="#B13C66"), size=0.6)
pl1 <- pl1 + geom_vline(xintercept = 1:100)
pl1 <- pl1 + theme1 + labs(x=NULL)
pl1 <- pl1 + geom_segment(data=da[events,], aes(x=t, xend=t, y=-Inf, yend=value), colour = "grey50", size = 0.6)
pl1 <- pl1 + geom_point(data=da[events,], mapping = aes(color="grey50"), size=3, shape=21, colour="grey50")
pl1 <- pl1 + scale_color_manual(values=c("#B13C66", "grey50"), labels=c("Percentage of Audience", "Beginning of Commercial Break"))
pl1 <- pl1 + annotate("text", x = max(da$t), y = c(61, 71, 81, 91, 101), label = c("60%", "70%", "80%", "90%", "100%"),
                      color='grey50')

grid.arrange(pl1, pl, ncol=1, nrow=2, heights = c(0.7, 0.3))



