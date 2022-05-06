# Goes with this post: http://stats.stackexchange.com/a/206345/67822

setwd("C:/Users/s182954/Documents/Nanocourses-2022/IntroductionToRForBeginners/R-Source");

hsb2 <- read.csv("hsb2.csv")
contr.poly(4)

hsb2$readcat<-cut(hsb2$read, 4, ordered = TRUE)
table(hsb2$readcat) 
mean(hsb2$write)
(means = tapply(hsb2$write, hsb2$readcat, mean))

# Manually producing contr.poly(4)
(scores = 1:4)
mean(scores)
(y = scores - mean(scores))
(seq_len(4) - 1)
X = outer(y, seq_len(4) - 1, "^")
(c_Q = qr(X)$qr)
(z = c_Q * (row(c_Q) == col(c_Q)))
# The actual Q matrix is:
Q <- qr.Q(qr(X))
(Q %*% z)
#This is the same as the call in the function:
(raw = qr.qy(qr(X), z))
(Z <- sweep(raw, 2L, apply(raw, 2L, function(x) sqrt(sum(x^2))), 
            "/", check.margin = FALSE))




plot(tapply(hsb2$write, hsb2$readcat, mean), pch=15, cex=2, xlab="", 
     ylab="Mean write", xaxt="n", main="Type of Polynomial Relation")
axis(1, at = seq(1,4, by = 1), las=1)




# We are running a simple ANOVA, performed as lm() on ordered explanatory
# variable and we get a coeff for every level of the factor variable.
# Later we see what fits best.

summary(lm(write ~ readcat, hsb2))
coeff = coefficients(lm(write ~ readcat, hsb2))


C = contr.poly(4)

# These are the actual means at each level:

(recovered = c(coeff %*% c(1, C[1,]),
               coeff %*% c(1, C[2,]),
               coeff %*% c(1, C[3,]),
               coeff %*% c(1, C[4,])))
# Perfectly corresponding to the actual values:

(actual = tapply(hsb2$write, hsb2$readcat, mean))

# And the best fitting model is linear:

LINEAR_CONTRAST = c(c(coeff[1] + coeff[2] * C[1,1], 
                      coeff[1] + coeff[2] * C[2,1],
                      coeff[1] + coeff[2] * C[3,1], 
                      coeff[1] + coeff[2] * C[4,1])) 
points(LINEAR_CONTRAST, pch=19, col=2, cex=1.5)
lo1 = lm(LINEAR_CONTRAST ~ c(1:4))
x = seq(1, 4, 0.0001)
abline(lm(LINEAR_CONTRAST ~ c(1:4)), col=2, lwd=2)
abline(h=mean(hsb2$write))

QUADRATIC_CONTRAST = c(coeff[1] + coeff[3] * C[1,2], 
                       coeff[1] + coeff[3] * C[2,2], 
                       coeff[1] + coeff[3] * C[3,2], 
                       coeff[1] + coeff[3] * C[4,2])
points(QUADRATIC_CONTRAST, pch=19, col="forestgreen", cex=2)
lines(spline(1:4, QUADRATIC_CONTRAST, n=100), 
      lwd=2, col="forestgreen")


CUBIC_CONTRAST = c(coeff[1] + coeff[4] * C[1,3], 
                   coeff[1] + coeff[4] * C[2,3], 
                   coeff[1] + coeff[4] * C[3,3], 
                   coeff[1] + coeff[4] * C[4,3])
points(CUBIC_CONTRAST, pch=19, col="darkorange", cex=2)
lines(spline(1:4, CUBIC_CONTRAST, n=1000), 
      col="darkorange", lwd=2)


legend(1,62,legend=c("Linear","Quadratic","Cubic"),
       col=c("firebrick","forestgreen","darkorange"),
       lty=1,
       border="", bty="n",cex=0.8)

# Compare to applying .L (linear) coefficient to all contrasts:


plot(tapply(hsb2$write, hsb2$readcat, mean), pch=15, cex=2, xlab="", 
     ylim= c(40,65),
     ylab="Mean write", xaxt="n", main="Type of Polynomial Relation")
axis(1, at = seq(1,4, by = 1), las=1)
LINEAR_CONTRAST = c(c(coeff[1] + coeff[2] * C[1,1], 
                      coeff[1] + coeff[2] * C[2,1],
                      coeff[1] + coeff[2] * C[3,1], 
                      coeff[1] + coeff[2] * C[4,1])) 
points(LINEAR_CONTRAST, pch=19, cex=1.5, col=2)
lo1 = lm(LINEAR_CONTRAST ~ c(1:4))
x = seq(1, 4, 0.0001)
abline(lm(LINEAR_CONTRAST ~ c(1:4)), col=2,lwd=2)
abline(h=mean(hsb2$write))

QUADRATIC_CONTRAST = c(coeff[1] + coeff[2] * C[1,2], 
                       coeff[1] + coeff[2] * C[2,2], 
                       coeff[1] + coeff[2] * C[3,2], 
                       coeff[1] + coeff[2] * C[4,2])
points(QUADRATIC_CONTRAST, pch=19, col="forestgreen",cex=1.5)
lines(spline(1:4, QUADRATIC_CONTRAST, n=1000), 
      col="forestgreen", lwd=2)


CUBIC_CONTRAST = c(coeff[1] + coeff[2] * C[1,3], 
                   coeff[1] + coeff[2] * C[2,3], 
                   coeff[1] + coeff[2] * C[3,3], 
                   coeff[1] + coeff[2] * C[4,3])
points(CUBIC_CONTRAST, pch=19, col="darkorange", cex=1.5)
lines(spline(1:4, CUBIC_CONTRAST, n=1000), 
      col="darkorange", lwd=2)

swiss$im.ord.3 <- cut(swiss$Infant.Mortality,3);
swiss$im.ord.4 <- cut(swiss$Infant.Mortality,4); 
swiss$im.ord.5 <- cut(swiss$Infant.Mortality,5);

swissord3.f <- formula(Fertility~ordered(im.ord.3));
swissord4.f <- formula(Fertility~ordered(im.ord.4)); 
swissord5.f <- formula(Fertility~ordered(im.ord.5)); 
swissfull.f <- formula(Fertility~Infant.Mortality); 

swissord3.m <- glm(swissord3.f, data=swiss);
swissord4.m <- glm(swissord4.f,data=swiss);
swissord5.m <- glm(swissord5.f,data=swiss); 
swissfull.m <- glm(swissfull.f,data=swiss); 

summary(swissord3.m)
summary(swissord4.m)
summary(swissord5.m)
summary(swissfull.m)
