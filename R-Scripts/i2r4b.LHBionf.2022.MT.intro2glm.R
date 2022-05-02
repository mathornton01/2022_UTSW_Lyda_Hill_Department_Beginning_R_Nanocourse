# Introduction to R for Beginners
#  May 10, 2022 - Introduction to Linear Models 
#    iris dataset 



# It is never a bad idea to plot the data first to examine
# whether a linear relationship makes sense. 

# The pairs function will make simultaneous scatterplots of 
#  our data, so we can see where there are potential linear
#  trends. 
pairs(iris)
summary(iris)

formula("Fertility~Catholic") -> form1;
formula(Fertility~Catholic) -> form2;

glm(form1,data=swiss)
glm(form2,data=swiss)

iris[,1:4] -> irisn;

formula(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width) -> irisn.form1;

glm(irisn.form1,data=irisn) -> irisn.mod1;
lm(irisn.form1,data=irisn) -> irisn.mod1.lm; 

summary(irisn.mod1);
summary(irisn.mod1.lm);

formula(Sepal.Length~0+Sepal.Width+Petal.Length+Petal.Width) -> irisn.form2;

glm(irisn.form2,data=irisn) -> irisn.mod2; 

summary(irisn.mod2)

formula(Sepal.Length~0+Petal.Length*Petal.Width*Sepal.Width) -> irisn.form3;

glm(irisn.form3,data=irisn) -> irisn.mod3;

summary(irisn.mod3);


# Forward stepwise selection for single/joint fixed effects. 
formula(Sepal.Length~0) -> irisn.ioform;
glm(irisn.ioform,data=irisn) -> irisn.iomod; 

forward <- step(irisn.iomod, direction='forward',scope=formula(irisn.mod3),trace=0)
forward$anova
forward$coefficients

# Backwards stepwise selection for single/joint fixed effects. 
backward <- step(irisn.mod3, direction='backward',scope=formula(irisn.mod3),trace=0);
backward$anova
backward$coefficients

# Bidirectional Stepwise Selection for single/joint fixed effects. 
both <- step(irisn.iomod,direction='both',scope=formula(irisn.mod3),trace=0);
both$anova
both$coefficients

library(glmnet);

X <- irisn[,2:4];
X <- cbind(1,X);
X[,5] <- X[,2]*X[,3];
X[,6] <- X[,2]*X[,4];
X[,7] <- X[,3]*X[,4];
X[,8] <- X[,2]*X[,3]*X[,4]; 

Y <- irisn[,1]; 

enet.mod1 <- cv.glmnet(X,Y,family="gaussian");

enet.mod1

plot(enet.mod1)

# Joint Effects Modeling. 

par(mfrow=c(1,2))
med.pl <- median(irisn$Petal.Length);
ind.pl.high <- irisn$Petal.Length > med.pl;

plot(irisn[ind.pl.high,'Sepal.Width'],irisn[ind.pl.high,'Sepal.Length'],
     xlab="Sepal Width",
     ylab="Sepal Length",
     ylim=c(4,10),
     main="Dependence of (Sepal Length on Sepal Width) Depends on Petal Length \n 
     Fisher's Iris Data Set (Sepal Width only Models) (N=150)")
points(irisn[!ind.pl.high,'Sepal.Width'],irisn[!ind.pl.high,'Sepal.Length'],col='red',pch=2)

abline(lm(Sepal.Length~1+Sepal.Width,data=irisn[ind.pl.high,]))
abline(lm(Sepal.Length~1+Sepal.Width,data=irisn[!ind.pl.high,]),col='red')
med.pl
legend('topleft',legend=c(paste("Petal Length >",med.pl),
                          paste("Petal Length <",med.pl)),col=c('black','red'),
       pch=1:2)

text(2.75,5,"Estimated Slope = -0.097 cm \n p-value: 0.351",col='red')
text(3.55,6.55,"Estimated Slope = 0.761 cm \n p-value: 0.000404")

formula(Sepal.Length~0+Sepal.Width*Petal.Length*Petal.Width) -> irisn.form3.a;
formula(Sepal.Length~0+Sepal.Width+Petal.Length+Petal.Width+
          Sepal.Width:Petal.Length + 
          Sepal.Width:Petal.Width + 									Petal.Length:Petal.Width + 			
          Sepal.Width:Petal.Length:Petal.Width) -> irisn.form3.b;
irisn.form3.a==irisn.form3.b
irisn.form3.a
irisn.form3.b

summary(glm(irisn.form3.a,data=irisn))
summary(glm(irisn.form3.b,data=irisn))


glm(irisn.form3.a,data=iris[iris$Species=='setosa',])-> irisn.setosa.full;
glm(irisn.form3.a,data=iris[iris$Species=='versicolor',])-> irisn.versicolor.full;
glm(irisn.form3.a,data=iris[iris$Species=='virginica',])-> irisn.virginica.full;

cbind(coefficients(irisn.setosa.full),
      coef(summary(irisn.setosa.full))[,4],
      coefficients(irisn.versicolor.full),
      coef(summary(irisn.versicolor.full))[,4],
      coefficients(irisn.virginica.full),
      coef(summary(irisn.virginica.full))[,4]) -> coefficients.df;

library(htmlTable)
colnames(coefficients.df) <- rep(c("Coefficient","P-Value"),3)
format(coefficients.df,digits=4,scientific=FALSE) -> coefficients.df;
htmlTable(coefficients.df,n.rgroup=c(3,3,1),
          rgroup=c("Singleton Effects", "Bivariate Effects", "Trivariate Effects"),
          n.cgroup=rep(2,3),cgroup=c("Setosa Data", "Versicolor Data", "Virginica Data"),
          scientific=F)
