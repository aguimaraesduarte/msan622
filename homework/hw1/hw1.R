library(ggplot2)

df <- read.csv("cancer_survival_rate.csv",
               header = T,
               stringsAsFactors = F,
               col.names = c("CancerType", "5YR", "10YR", "15YR", "20YR"))

# Define new Y positions
df$X5YR_ <- c(99,97,95,89,86,84,82,80,71,69,64,62,60,58,56,54,43,32,30,24,16,14,8,4)
df$X10YR_ <- c(94,96,92,87,78,80,83,76,64,58,56,52,54,46,44,49,32,29,13,19,11,8,6,3)
df$X15YR_ <- c(87,94,91,84,72,74,81,70,63,46,52,50,54,40,38,48,30,28,8,19,6,10,4,2)
df$X20YR_ <- c(81,95,88,83,65,67,79,69,60,38,48,46,52,34,32,50,26,24,6,15,8,4,10,2)

theme_update(plot.title = element_text(hjust = 0.5, size = 24))

# Create plot
p <- ggplot(df)

# Create slopelines
p <- p + geom_segment(aes(x=0.4, xend=1.4, y=X5YR_, yend=X10YR_), size=1, color="grey70")
p <- p + geom_segment(aes(x=1.65, xend=2.65, y=X10YR_, yend=X15YR_), size=1, color="grey70")
p <- p + geom_segment(aes(x=2.9, xend=3.9, y=X15YR_, yend=X20YR_), size=1, color="grey70")

# Create text
## Cancers and rates
p <- p + geom_text(label=df$CancerType, y=df$X5YR_, x=rep.int(0, nrow(df)), size=4)
p <- p + geom_text(label=df$X5YR, y=df$X5YR_, x=rep.int(0.3, nrow(df)), size=4)
p <- p + geom_text(label=df$X10YR, y=df$X10YR_, x=rep.int(1.52, nrow(df)), size=4)
p <- p + geom_text(label=df$X15YR, y=df$X15YR_, x=rep.int(2.77, nrow(df)), size=4)
p <- p + geom_text(label=df$X20YR, y=df$X20YR_, x=rep.int(4, nrow(df)), size=4)
## Years
p <- p + geom_text(label="5 years", y=103, x=0.3, size=5)
p <- p + geom_text(label="10 years", y=103, x=1.52, size=5)
p <- p + geom_text(label="15 years", y=103, x=2.77, size=5)
p <- p + geom_text(label="20 years", y=103, x=4, size=5)

# Labels and title
p <- p + xlab("") + ylab("") + ggtitle("Cancer Survival Rates")
p <- p + xlim(0, 4)
p <- p + ylim(0, 105)

# Remove chart junk
p <- p + theme(panel.background = element_blank(),
               panel.grid=element_blank(),
               axis.ticks=element_blank(),
               axis.text=element_blank(),
               panel.border = element_rect(colour = "black", fill=NA, size=1))

# Plot
p
