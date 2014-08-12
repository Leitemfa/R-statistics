R-statistics
============

R codes for study statistics

#Point Pattern analysis in R

#Adding tables

#Method 1

X.nom<-"filename.txt"

X.nom

X<-scanpp(X.nom, header=T, owin(c(-5.10, 47.60), c(-4.92, 49.15)) #Sempre alterar os valoes de "c" com coordenadas

Z<-as.ppp(X)

contour(density(Z, 10), axes = FALSE)  #cria gráfico de contornos

plot(Z)

#Method 2

mydata <- read.table("filename.txt", header = TRUE)

is.factor(M)

attach(mydata)  #Still not working yet

W <- ppp(mydata$X, mydata$Y, c(-5, 50), c(-5, 50), marks=mydata$M) #No automatic way to change the "c" coordenates according to a file?

#Processing data

G <- Gest(Z)
plot(G$r, G$km, type="l")     #will give you a basic plot of Gb(r) against r where Gb(r) is the Kaplan-Meier estimate of G(r) computed by Gest

plot(G, cbind(km, rs, theo) ~ r) #will plot the Kaplan-Meier estimate G$km, the border corrected (reduced sample) estimate G$rs, and the theoretical Poisson value, against r

#Ripley’s K function is often transformed to L(r) = qK(r)/. To estimate and plot the L function

K <- Kest(Z)
plot(K, sqrt(./pi) ~ r)

K12 <- Kest(Z)
g12 <- pcf(K12, method="d", spar=0.7)
lambda2 <- summary(Z)$M("1","2")
Oring <- eval.fv(lambda2 * g12)
E <- envelope(Z, pcf, nsim = 99, rank = 1)

plot(E)

plot(Oring, ylab="Oring(r)")

fit <- ppm(Z, ~1, Strauss(r=0.1))
fit
plot(fit)
pf <- predict(fit)
coef(fit)

#Adding script of envelopes

K12 <- Kest(Z)
g12 <- pcf(K12, method="d", spar=0.7)
Oring <- eval.fv(g12)
E <- envelope(Z, pcf, nsim = 99, rank = 1)

plot(E)
