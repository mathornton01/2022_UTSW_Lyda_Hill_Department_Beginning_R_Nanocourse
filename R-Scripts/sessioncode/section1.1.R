summary(iris); 
help(iris); 

irisn=iris[,1:4];

pairs(irisn);
plot(irisn$Sepal.Length,irisn$Sepal.Width);

formula(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width) -> irisn.form1;
glm(irisn.form1,data=irisn) -> irisn.mod1;

summary(irisn.mod1);
irisn.mod1;


summary(irisn.mod1);


formula(Sepal.Length~0+Sepal.Width+Petal.Length+Petal.Width) -> irisn.form2;
glm(irisn.form2,data=irisn) -> irisn.mod2;
summary(irisn.mod2)

formula(Sepal.Length~0+Sepal.Width*Petal.Length*Petal.Width) -> irisn.form3.a;
formula(Sepal.Length~0+Sepal.Width*Petal.Length - Petal.Length) -> irisn.form4;
formula(Sepal.Length~0+Sepal.Width+Petal.Length+Petal.Width+
          Sepal.Width:Petal.Length + 
          Sepal.Width:Petal.Width + 									
          Petal.Length:Petal.Width + 			
          Sepal.Width:Petal.Length:Petal.Width) -> irisn.form3.b;

irisn.mod3.a <- glm(irisn.form3.a,data=iris)
irisn.mod3.b <- glm(irisn.form3.b,data=iris)
irisn.mod4 <- glm(irisn.form4, data=iris)

summary(irisn.mod3.a)
summary(irisn.mod3.b)
summary(irisn.mod4)

formula(Sepal.Length~0+offset(1.3*Sepal.Width)+Petal.Length+Petal.Width+
          Sepal.Width:Petal.Length + 
          Sepal.Width:Petal.Width + 									Petal.Length:Petal.Width + 			
          Sepal.Width:Petal.Length:Petal.Width) -> irisn.form5;

irisn.mod5 <- glm(irisn.form5,data=iris)
summary(irisn.mod5)


glm(irisn.form3.a,data=iris[iris$Species=='setosa',])-> irisn.setosa.full;
glm(irisn.form3.a,data=iris[iris$Species=='versicolor',])-> irisn.versicolor.full;
glm(irisn.form3.a,data=iris[iris$Species=='virginica',])-> irisn.virginica.full;