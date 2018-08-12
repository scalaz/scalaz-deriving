library(plot3D)

ilist <- function(a, b) {
    a * b / 2.8
}
jhash <- function(a, b) {
    ifelse(a < 6, 1.0 / (0.57 - 0.166 * log2(a)), b / 2.8 + (a ** 1.05) / 1.8)
}

x <- y <- seq(1, 128, length=25)
z1 <- outer(x, y, ilist)
z2 <- outer(x, y, jhash)
#persp3D(x, y, z1, border="black",theta=-15, phi=25)
#persp3D(x, y, z2, add=TRUE, border="black",theta=-15, phi=25)

z3 <- outer(x, y, best)
persp3D(x, y, z3, border="black",theta=-15, phi=25)
