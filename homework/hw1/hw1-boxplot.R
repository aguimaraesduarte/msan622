library(ggplot2)
library(reshape2)

glmnet <- read.csv("results-glmnet-cross-table.csv",
                   header = T,
                   stringsAsFactors = F)
glmnet$algo <- "glmnet"

pgboost <- read.csv("results-pgboost-cross-table.csv",
                    header = T,
                    stringsAsFactors = F)
pgboost$algo <- "boosting"

df <- rbind(glmnet, pgboost)
df <- melt(df, id.vars = c("iter", "algo"))
df$algo = factor(df$algo, levels=c('boosting','glmnet'), ordered = T)
df$variable = factor(df$variable, levels=c('mederr','rmse'), labels = c("MedErr", "RMSE"), ordered = T)

theme_update(plot.title = element_text(hjust = 0.5, size = 24))
g <- ggplot(df, aes(y=value, x=algo))
g <- g + geom_boxplot(size=1, outlier.size=4)
g <- g + facet_wrap(~variable, scales = "free")
g <- g + xlab("") + ylab("") + ggtitle("Comparing glmnet and gradient boosting")
g <- g + theme(strip.text.x = element_text(size = 16),
               axis.text = element_text(size=16))
g

