library(ggplot2)

## 1.2 Inverse CDF Method

U <- runif(10000, min=0, max=1)
X <- qexp(U, rate=1/2)
par(mfrow=c(1, 2))
hist(U)
hist(X)

## 1.3 Accept/Reject Algorithm

### 1.3.1 MCMC algorithm

df <- function(x) {
    return(0.7*dnorm(x, mean=2, sd=1) + 0.3*dnorm(x, mean=5, sd=1))
}

x <- seq(-3, 12, length=200)
density.data <- data.frame(x=x, y=df(x))
density <- ggplot(density.data, aes(x=x, y=y)) + geom_line() + labs(y='f(x)', x='x')
density

rproposal <- function(x.i) {
    out <- x.i + runif(1, -2, 2)
    return(out)
}

x <- 3
x.star <- 3
x.star[2] <- rproposal(x[1])
x.star[2]

if(df(x.star[2])/df(x[1]) > runif(1)) {
    x[2] <- x.star[2]
} else {
    x[2] <- x[1]
}

library(STA578)
plot_chain(density, x, x.star)

for(i in 2:10) {
    x.star[i+1] <- rproposal(x[i])
    if(df(x.star[i+1]) / df(x[i]) > runif(1)) {
        x[i+1] <- x.star[i+1]   # Accept.
    } else {
        x[i+1] <- x[i]          # Reject.
    }
}

plot_chain(density, x, x.star)

chain <- MCMC(df, 2, rproposal, N=1000)
trace_plot(chain)

chain2 <- MCMC(df, chain[1000], rproposal, N=10000)
long.chain <- c(chain, chain2)
hist(long.chain)

#### Variance too low
p.small <- function(x) {
    return(x + runif(1, -.1, +.1))
}
chain <- MCMC(df, 2, p.small, N=1000)
trace_plot(chain)

#### Variance too high
p.large <- function(x) {
    return(x + runif(1, -30, +30))
}
chain <- MCMC(df, 2, p.large, N=1000)
trace_plot(chain)

### Burn-in
chain <- MCMC(df, 40, rproposal, N=1000) # start at x = 40
trace_plot(chain)

chain1 <- MCMC(df, -30, rproposal, N=1000)
chain2 <- MCMC(df, 0, rproposal, N=1000)
chain3 <- MCMC(df, 30, rproposal, N=1000)
chains <- cbind(chain1, chain2, chain3)
trace_plot(chains)
