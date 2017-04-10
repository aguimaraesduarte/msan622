library(ggplot2)

df <- read.csv("results-gradient-boosting.csv",
               header = T,
               stringsAsFactors = F)

theme_update(plot.title = element_text(hjust = 0.5, size = 24))
g <- ggplot(df, aes(y=y, x=pred))
g <- g + geom_segment(aes(x=0, y=0, xend=21, yend=21, lty="dashed"),
                      color="grey70", size=.5)
g <- g + geom_segment(aes(x=0, y=3, xend=17.5, yend=20.5, lty="solid"),
                      color="grey70", size=.5)
g <- g + geom_segment(aes(x=2.5, y=-0.5, xend=21, yend=18, lty="dotted"),
                      color="grey70", size=.5)
g <- g + geom_point(size=3, alpha = 0.5)
g <- g + xlab("Predicted") + ylab("Measured") + ggtitle("Measured vs Predicted Passing Rate")
g <- g + scale_linetype_manual(name="",
                               values=c("solid", "66", "66"),
                               labels=c("Measured = Predicted", "Measured = Predicted - 3%", "Measured = Predicted + 3%"),
                               guide=guide_legend(override.aes = list(linetype=c("solid", "66", "66"))))
g <- g + theme(axis.text = element_text(size=16),
               axis.title=element_text(size=20),
               legend.position = c(.8, .2),
               legend.background = element_rect(fill = "white", color="white"),
               legend.title = element_blank(),
               legend.text = element_text(size=14),
               legend.key.size = unit(2, "line"))
g
#override.aes = list(linetype=c("solid", "dashed", "dashed")))
