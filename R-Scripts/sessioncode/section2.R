quantile(rnorm(10000,mean(iris$Petal.Length),sd(iris$Petal.Length)),(0:1000/1000)) -> theoretical.quants;
quantile(iris$Petal.Length, (0:1000/1000)) -> experimental.quants; 
plot(experimental.quants,theoretical.quants);
qqplot(rnorm(10000,mean(iris$Petal.Length),sd(iris$Petal.Length)),iris$Petal.Length);
qqnorm(iris$Petal.Length);


qqnorm(scale(iris$Sepal.Length),main="Normal Quantile Quantile Plot \n for Sepal Length in Iris Data");
abline(0,1);


trnscounts <- c(18,13,17,14,15,16,15,17,22,12);
qqplot(trnscounts,rpois(1000,15), main='Poisson(15) QQ Plot');
qqplot(trnscounts,rnbinom(1000,size=20,mu=15));
par(mfrow=(c(2,1)));
qqplot(trnscounts,rnbinom(1000,size=20,mu=15), main="Poisson(15) QQ Plot");
abline(0,1);
qqplot(trnscounts,rnbinom(1000,size=20,mu=15), main="Negative Binomial(n=20,mu=15) QQ Plot");
abline(0,1);
par(mfrow=(c(1,1))); 

plot(density(rnbinom(1000,size=20,mu=15)),ylim=c(0,0.1)); 
lines(density(rpois(1000,15)),col='blue');
legend('topright',legend=c('Poisson(15)','NB(n=20,mu=15)'),col=c('black','blue'),lty=1);


iris$LP <- (iris$Petal.Length > median(iris$Petal.Length))
logistic.form1 <- formula(LP ~ Petal.Width+Sepal.Length+Sepal.Width);
logistic.mod1 <- glm(logistic.form1,data=iris,family=binomial(link='logit'));
summary(logistic.mod1);

exp(coef(logistic.mod1))
