library(fitdistrplus)


dgumbel <- function(x,mu,s){ # PDF
  exp((mu - x)/s - exp((mu - x)/s))/s
}

pgumbel <- function(q,mu,s){ # CDF
  exp(-exp(-((q - mu)/s)))
}

qgumbel <- function(p, mu, s){ # quantile function
  mu-s*log(-log(p))
}

rev_dist_mers <- read.csv("name.csv")
rev_dist_mers2 = c(rev_dist_mers[,-1])
gumbel_mers.fit <- fitdist(rev_dist_mers2, "gumbel", start=list(mu=5, s=5), method="mle")
summary(gumbel_mers.fit)

gofstat(gumbel_mers.fit, discrete=FALSE) # goodness-of-fit statistics

# Plot the fit

par(cex=1.2, bg="white")

plot(gumbel_mers.fit, lwd=2, col="steelblue")

mtext("Inversion Distance Between A and B",                  
      side = 3,
      line =  -1,
      cex = 1.2,
      col = "blue",
      outer = TRUE)


svg("plot_A_B.svg")
par(cex=1.2, bg="white")

plot(gumbel_mers.fit, lwd=2, col="steelblue")

mtext("Inversion Distance Between A and B",                  
      side = 3,
      line =  -1,
      cex = 1.2,
      col = "blue",
      outer = TRUE)
dev.off()
