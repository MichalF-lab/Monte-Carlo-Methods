set.seed(131311)

#--------------------------------------
x2 <- seq(0, 50, by = 0.01)
los <- sort(runif(5001, 0, 1))
#-------------------------------------
gen_exp1 <- function(lambdaa) {
    plot(x2, -(log(los))/ lambdaa)
    lines(x2, dexp(x2, rate = lambdaa)* 800, col = "blue")
}
gen_exp1(1/10)

gen_wei <- function(k, lambda) {
    temp <- (lambda * (-log(1 - los)) ^ (1 / k))
    hist(temp, freq = FALSE)
    lines(x2, dweibull(x2, k, lambda), col = "blue")
}

gen_wei(2, 5)

gen_cau <- function(lambda) {
    temp <- lambda * tan(pi * (los - 0.5))
    hist(temp, breaks = 5000, xlim = c(-10, 10))
    x3 <- seq(-10, 10, by = 0.004)
    lines(x3, dcauchy(x3, scale = lambda)* 10000, col = "blue")
}
gen_cau(5)
#Wygeneruj dwie niezale¿ne zmienne losowe u i v z rozk³adu jednostajnego na przedziale(0, 1) .
#Oblicz x = arctg((v - 0.5) / u)
#Zwróæ wartoœæ przesuniecie + lambda * x.

#---------------------------------------------
gen_box_mul <- function() {
    los1 <- (runif(5001, 0, 1))
    los2 <- (runif(5001, 0, 1))
    x1 <- sqrt(-2*log(los1))*cos(2*pi*los2)
    x3 <- seq(-10, 10, by = 0.01)
    hist(x1, freq = FALSE, xlim = c(-5, 5), breaks = 30)
    lines(x3, dnorm(x3), col = "blue")
}
gen_box_mul()

gen_splot <- function() {
    los1 <- (runif(1000, 0, 1))
    los2 <- (runif(1000, 0, 1))
    x1 <- sqrt(-2 * log(los1)) * cos(2 * pi * los2)
    x2 <- sqrt(-2 * log(los1)) * sin(2 * pi * los2)
    plot(x1,x2)
    plot(x1,x1+x2)
    plot(x1 + x2, (x1 - x2) * sqrt(2))
    print(cov(x1, x2))
    print(cor.test(x1, x2))
    # p-value = 0.1666
    print(cov(x1, x1 + x2))
    print(cor.test(x1, x1 + x2))
    # p-value = 2.2e-16
    print(cov(x1 + x2, (x1 - x2) * sqrt(2)))
    print(cor.test(x1 + x2, (x1 - x2) * sqrt(2)))
    # p-value = 0.57
    #Przyjmuje sie ¿e dla p-value <= 0.05 mo¿emy stwierdziæ, ¿e istnieje 
    #statystycznie istotny zwi¹zek miêdzy zmiennymi
}
gen_splot()


gen_custom <- function() {
    x2 <- seq(0, 1, by = 0.001)
    los <- (runif(1001, 0, 1))
    los <- los ^ 3
    dys <- density(los)$y[1:1001]
    plot(x2, dys)
    # Przypomina rozk³ad beta
    # Wzor na gestoœc znajduje sie tu https://pl.wikipedia.org/wiki/Rozk%C5%82ad_beta
    # U^3 Nie jest funkcja kwantylow¹ bo nie jest monotonicznie rosn¹ca
}

gen_custom()

gen_custom2 <- function() {
    los1 <- (runif(5001, 0, 1))
    los2 <- (runif(5001, 0, 1))
    x1 <- sqrt(-2 * log(los1)) * cos(2 * pi * los2)
    x1 <- abs(x1)
    x3 <- seq(-10, 10, by = 0.01)
    hist(x1, freq = FALSE, xlim = c(0, 10), breaks = 30)
    lines(x3, dnorm(x3) * 2, col = "blue")
    # Jest to poprostu uciecie gêstoœci rozk³adu normalnego do liczb nieujemnych
    # -2(ln U) to jest zm los o rozk³adzie wyk³adniczym z parametrem 2
}

gen_custom2()