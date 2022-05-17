help(swiss);

swiss.form.1 <- formula(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture+Examination);
swiss.mod.1 <- glm(swiss.form.1,data=swiss);

summary(swiss.mod.1);

swiss.form.2 <- formula(Fertility~Education*Agriculture*Infant.Mortality);
swiss.mod.2 <- glm(swiss.form.2,data=swiss)

AIC(swiss.mod.2)

swiss.form.3 <- formula(Fertility ~ Education*Agriculture*Infant.Mortality - Agriculture:Infant.Mortality - Education);
swiss.mod.3 <- glm(swiss.form.3,data=swiss)
AIC(swiss.mod.3)

