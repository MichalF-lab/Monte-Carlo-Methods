# zad 1
generator <- function(m, a, n = 50) {
    s <- 1 # Wartoœæ pocz¹tkowa

    # Generowanie liczb pseudolosowych
    liczby <- c() # Wektor

    for (i in 1:n) {
        # Generowanie kolejnej wartoœci
        s <- (a * s) %% m
        # Obliczenie liczby pseudolosowej
        liczby <- c(liczby, s / m)
    }
    return(liczby)
}

wynik1 <- generator(37,19)
wynik2 <- generator(2 ^ 31 - 1, 39373)

# zad 3
hist(wynik1)
hist(wynik2)

# zad 4
tab1 <- matrix(generator(37, 19, 100), 2, 500)
tab2 <- matrix(generator(2 ^ 31 - 1, 39373, 100), 2, 500)

plot(tab1[1,], tab1[2,], col = "blue", pch = 16, xlim = c(0, 1), ylim = c(0, 1))
plot(tab2[1,], tab2[2,], col = "blue", pch = 16, xlim = c(0, 1), ylim = c(0, 1))

# zad 2
okres <- function(m, a) {
    s <- 1 # Wartoœæ pocz¹tkowa

    # Generowanie liczb pseudolosowych
    liczby <- c() # Wektor

    while (TRUE) {
        # Generowanie kolejnej wartoœci
        s <- (a * s) %% m
        # Obliczenie liczby pseudolosowej
        liczby <- c(liczby, s)
        if (any(duplicated(liczby))) {
            indeksy <- which(liczby == liczby[length(liczby)])
            return(indeksy[2] - indeksy[1])
        }
    }
}

wynik3 <- okres(37, 19)
print(wynik3)

#---------------------------------------------------------------
# zad 2
set.seed(131311)

# Smieci
# c(runif(50), runif(50))
#tab <- matrix(runif(50),2,50)
#hist(tab)

# Tworzymy tablice
tab <- matrix(runif(500), 2, 500)

# Smieci
#plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1))
#points(tab[1,], tab[2,], col = "blue", pch = 20)

# zad 1
plot(tab[1,], tab[2,], col = "blue", pch = 16, xlim = c(0, 1), ylim = c(0, 1))

# zad 3
hist(tab)
abline(h = 100)

# Smieci
# lines(density(tab), col = "red")
# curve(dnorm)

#--------------------------------------------------------------

#?dbinom

# zad 1 
generator1 <- function(p) {
    temp <- runif(1, 0, 1)
    if (temp > p) {
        return(0)
    }
    else {
        return(1)
    }
}
#print(generator1(0.3))

# zad 2
generator2 <- function(p, n) {
    temp <- c()
    for (i in 1:n) {
        temp = c(temp, generator1(p))
    }
    temp
}
hist(generator2(0.3, 12))

# zad 3
generator3 <- function(lambda) {
    temp <- runif(1, 0, 1)
    k = 0
    p = exp(-lambda)
    F1 = p

    while(TRUE){
        if (temp < F1) {
            return(k)
        }
        p = p * lambda / (k + 1)
        F1 = F1 + p
        k = k + 1
    }
}
#print(generator3(3))


generator4 <- function(p, n) {
    temp <- c()
    for (i in 1:n) {
        temp = c(temp, generator3(p))
    }
    temp        
}
dane <- generator4(3, 12000)
hist(dane)
wyniki <- table(rpois(12000, 3))
lines(as.numeric(names(wyniki)), as.numeric(wyniki), type = "l", col = "blue")

# Przyk³adowe dane
x <- c(0.2, 0.5, 0.8)
y <- c(0.1, 0.7, 0.3)

# Rysowanie odcinka
plot(c(0, 1), type = "l", col = "gray")

# Dodawanie punktów
points(wynik1, col = "red")