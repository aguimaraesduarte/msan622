library(reshape2)

set.seed(23)
# creating a vector of time from 8am to 9am
a <- 5
n <- 3600/a
t <- seq(ISOdate(2016,01,01, 8, 0,0), by = 4, length.out = n)
x  <- rnorm(n, 0, 0.4)
x2  <- rnorm(n, 0, 0.3)
tune_in <- rep(0, n)
tune_out <- rep(0, n)
tune_in[0] <- 0.1

for (i in 3:length(tune_in)) {
	tune_in[i] <- 0.7 + x[i] + x[i -1] + x[i-2]
	tune_out[i] <- 0.6 + x2[i] + x2[i-1] + x2[i-2]
}
tune_in <- ifelse(tune_in > 0, tune_in, 0 )
tune_out <- ifelse(tune_out > 0, tune_out, 0 )

adstart <- rep(0, n)
events <- (60/a) * c(10, 20, 25, 35, 40, 47, 53)
magnitude <- c(4.5, 4.3, 4.55, 4.7, 4.8, 5, 4.9)

for (i in 0:4) {
	adstart[events + i] <- magnitude - abs(rnorm(7, 1/(i+1), 0.5))
}
tune_out  <- tune_out  + adstart

adends_tune_in <- rep(0, n)
adends <- events + 120/a

for (i in 0:5) {
	adends_tune_in[adends + i] <- abs(rnorm(7, 2.5/(i+1), 0.4))
}
tune_in  <- tune_in  + adends_tune_in

audience <- rep(0, n)
audience[1] <- 100 + tune_in[1] - tune_out[1]
for (i in 2:n) {
        audience[i] <- audience[i-1] + tune_in[i] - tune_out[i] 
}

type <- rep("audience", n)
aud.max <- max(audience)
audience <- 100* audience / aud.max
d <- data.frame(date=t, audience, tune_in, tune_out)

write.csv(d, "fake-tvads-data.csv", quote=FALSE)

d2 <- melt(d, id.vars = "date")

write.csv(d2, "fake-tvads-data.csv", quote=FALSE)
