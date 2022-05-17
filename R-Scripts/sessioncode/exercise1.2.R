swiss$IMC3 <- ordered(cut(swiss$Infant.Mortality,3));
swiss$IMC4 <- ordered(cut(swiss$Infant.Mortality,4));
swiss$IMC5 <- ordered(cut(swiss$Infant.Mortality,5));

form1 <- formula(Fertility~IMC3)
mod1 <- glm(form1,data=swiss)

form2 <- formula(Fertility~IMC4)
mod2 <- glm(form2,data=swiss)

form3 <- formula(Fertility~IMC5)
mod3 <- glm(form3,data=swiss)

form4 <- formula(Fertility~Infant.Mortality);
mod4 <- glm(form4,data=swiss);

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
