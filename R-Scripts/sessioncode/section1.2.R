iris.form.1 <- formula(Sepal.Length~Sepal.Width*Petal.Length*Petal.Width + as.factor(Species));
iris.mod.1 <- glm(iris.form.1,data=iris); 
summary(iris.mod.1)

iris$Species <- relevel(as.factor(iris$Species),ref='versicolor')
iris.mod.1.ref2 <- glm(iris.form.1,data=iris);

iris$PW.O <- cut(iris$Petal.Width,3)
iris.ordinal.form <- formula(Petal.Length ~ ordered(PW.O), 
                             levels = c('(0.0976,0.9]',
'(0.9,1.7]', 
'(1.7,2.5]'));
iris.ordinal.mod <- glm(iris.ordinal.form,data=iris);
summary(iris.ordinal.mod);

iris.form.1 <- formula(Sepal.Length~Sepal.Width*Petal.Length*Petal.Width+Species);
iris.mod.1 <- glm(iris.form.1,data=iris); 
plot(iris.mod.1);

f.p <- formula(Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width);
iris.pmod <- glm(f.p,data=iris); 
predict(iris.pmod,data.frame(Sepal.Width=1,
                             Petal.Length=2,
                             Petal.Width=3))


f.p <- formula(Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width);
iris.pmod <- glm(f.p,data=iris);

coef(iris.pmod)
c(AIC(iris.pmod),BIC(iris.pmod),deviance(iris.pmod),logLik(iris.pmod))

confint(iris.pmod)
vcov(iris.pmod)


f.p <- formula(Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width);
iris.pmod <- glm(f.p,data=iris); 
iris.pmod.estim <- coefficients(summary(iris.pmod))[,1];
iris.pmod.pvals <- coefficients(summary(iris.pmod))[,4];
iris.pmod.pvals.adj <- p.adjust(iris.pmod.pvals,method = 'bonferroni');
cbind(iris.pmod.estim,iris.pmod.pvals,iris.pmod.pvals.adj);


