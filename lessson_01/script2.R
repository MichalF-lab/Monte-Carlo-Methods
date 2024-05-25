library(MASS)
library(ggplot2)
set.seed(131311)

generate_Y <- function(p, mu, sigma, n) {
    X <- rbinom(n, 1, p)
    Y <- ifelse(X == 0, rnorm(sum(X == 0), 0, 1),
                rnorm(sum(X == 1), mu, sigma))
    return(Y)
}

p <- 0.9
mu <- 20
sigma <- 3
n <- 1000

Y <- generate_Y(p, mu, sigma, n)
hist(Y, breaks = 30, probability = TRUE)


x <- seq(min(Y), max(Y), length.out = 1000)
mix <- (1 - p) * dnorm(x, 0, 1) + p * dnorm(x, mu, sigma)

lines(x, mix, col = "blue", lwd = 2)

tab <- matrix(nrow = 2, ncol = 0) 
while (length(tab) / 2 <= 1000) {
    x <- runif(1, 0, 1)
    y <- runif(1, 0, 1)

    if (x + y >= 1) {
        next
    }

    gestosc <- 20 * x * y ^ 2

    if (gestosc > runif(1, 0, 1)) {
        tab <- cbind(tab, c(x, y))
    }
}

plot(tab[1,], tab[2,], col = "blue", pch = 16, xlim = c(0, 1), ylim = c(0, 1))

sigmq <- matrix(data = c(2, 2, 0,2, 5, -3,0, -3, 9), nrow = 3, byrow = TRUE)
mi <- c(1,-1,5)
data <- mvrnorm(n = 1000, mu = mi, Sigma = sigmq)
head(data)

#print(all.equal(sigmq, t(sigmq)))
#print(all(eigen(sigmq)$values > 0))

#cov_matrix <- matrix(c(1, 0.7, 0.7, 1), nrow = 2)

#correlated_variables <- mvrnorm(n = 100, mu = c(0, 0), Sigma = cov_matrix)

#head(correlated_variables)
# Konwersja na ramkê danych
df <- as.data.frame(data)
names(df) <- c("X1", "X2", "X3")

# Wykres próbek
ggplot(df, aes(x = X1, y = X2, color = X3)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "red") +
    labs(title = "Próby z rozk³adu wielowymiarowego",
       x = "X1", y = "X2", color = "X3")

denisty_1 <- function(x) {
    return(exp(1) ^ (-(x ^ 2)))
}

approximation <- function(f, n) {
    #plot(seq(0, 1, length.out = n), f(seq(0, 1, length.out = n)), type = "l", ylim = c(0, 1))

    return(mean(f(seq(0, 1, length.out = n))))
}
plotvar <- c()
for (ij in seq(1, 100000, by = 10 * ij)) {
    plotvar <- c(plotvar, approximation(denisty_1, ij))
}
plot(seq(0, 1000, length.out = 5), plotvar, type = "l")
#print(approximation(denisty_1, 10))
#print(approximation(denisty_1, 100))
#print(approximation(denisty_1, 1000))
#print(approximation(denisty_1, 10000))