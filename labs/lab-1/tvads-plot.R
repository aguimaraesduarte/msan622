library(ggplot2)
library(gridExtra)

# creating a vector of time from 8am to 9am
d <- read.csv("fake-tvads-data.csv")

da <- subset(d, variable=="audience")
d <- subset(d, variable %in% c("tune_in", "tune_out"))

# http://docs.ggplot2.org/dev/vignettes/themes.html
# http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=4

theme2 <- theme(
        axis.text = element_text(size = 10),
	axis.text.y=element_blank(),
	axis.title=element_blank(),
	axis.ticks.y=element_blank(),
        legend.background = element_rect(fill = "grey90", color="grey"),
        legend.position = c(0.10, 0.70),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "grey90", colour = "grey90"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "grey40", size=0.5),
	plot.margin=unit(c(0,1,1,1),"cm")
)

pl <- ggplot(d, aes(x=date, y=value, color=variable)) + geom_point(size=0.1)
pl <- pl + geom_line(size=0.5)
pl <- pl + scale_color_manual(values=c("#4daf4a", "#377eb8"))
pl <- pl + theme2

theme1 <- theme(axis.text.y=element_blank(), axis.text.x=element_blank(),
	axis.title=element_blank(),
	axis.ticks=element_blank(),
	panel.grid.major = element_line(colour = "grey90", size=0.5, linetype="dashed"),
        panel.grid.minor = element_blank(),
	panel.background = element_rect(fill = "white", color = "grey40", size=0.5),
	plot.margin=unit(c(1,1,-0.1,1),"cm"))

pl1 <- ggplot(da, aes(x=date, y=audience)) + geom_line(size=0.5) + theme1 + labs(x=NULL)

grid.arrange(pl1, pl, ncol=1, nrow =2, heights = c(0.7, 0.3))

# another posibility
pl <- pl + scale_color_brewer(type="qual", palette="Set1")


