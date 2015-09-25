## exploration model
M1 <- lm(log(keyfentr) ~ e0 * factor(sex), data = e0_vs_lxequality_wide)
M1
op  <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(M1, add.smooth = F, which = 1)
E <- resid(M1)
hist(E, xlab = "Residuals", main = "")
plot(log(e0_vs_lxequality_wide$keyfentr), E, xlab = "Log (Entropy)", ylab = "Residuals")
plot(factor(e0_vs_lxequality_wide$sex), E, xlab = "Sex", ylab = "Residuals" )
par(op)


op <- par(mfrow = c(2,2))
plot(M1, add.smooth = F)
par(op)

## xy plot
library(lattice)
xyplot(keyfentr~e0|factor(cntry), type = "l",
       xlab = "Life Expectancy",
       ylab = "Keyfitz' Entropy",
       data = e0_vs_lxequality_wide)


gompSurv <- function(a, b , x) {
  return(exp((a/b) - ((a/b) * exp(b * x))))
}

gompMort <- function(a, b, x) {
  return(a * exp(b * x))
}

gompDeath <- function(lx, mux){
  return((lx) * mux)
}

age  <- seq(0, 100, 0.1)

lx1 <- gompSurv(a = 0.00001, b = 0.14, x = age)
mux1 <- gompMort(a = 0.00001, b = 0.14, x = age)
de1 <- gompDeath(lx = lx1, mux = mux1)

plot(age, mux1, type = "l")
plot(age, lx1, type = "l")
plot(age, de1, type = "l")

col <- rainbow(15)[c(1, 4, 6, 13, 15)]
a <- 0.00001
b <- seq(0.12, 0.16, 0.01)

for (i in 1:5) {
  if (i == 1){ plot(age, gompSurv(a, b[i], age), col = col[i], type = "l")
               legend("topright", col = col, legend = b, lty = 1)} else {
    lines(age, gompSurv(a, b[i], age), col = col[i])
  }
}

}

lines(age, gompSurv(a, 0.09, age), col = col[i])

#lx2 <- gompSurv(a = 0.009, b = 0.14, x = age)

lines(age, lx2, col = "darkblue")
lines(age, lx3, col = "red")


loglx <- log(lx1)

plot(age, loglx, type = "l")
