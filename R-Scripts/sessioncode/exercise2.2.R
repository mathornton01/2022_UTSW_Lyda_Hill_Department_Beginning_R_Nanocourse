poisson.reg.wb.formula <- formula(breaks~wool+tension);
poisson.reg.wb.mod <- glm(poisson.reg.wb.formula,
                          data=warpbreaks,
                          family=poisson(link="identity"));
summary(poisson.reg.wb.mod);

