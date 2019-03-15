data(state)
statedata = cbind(data.frame(state.x77),state.abb,state.area,state.center, state.division, state.name, state.region)

str(statedata)

tapply(statedata$HS.Grad,statedata$state.region, mean)

boxplot(statedata$Murder ~ statedata$state.region)

statedata[statedata$state.region ==  "Northeast",]

lmod = lm(Life.Exp  ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(lmod)

plot(statedata$Income, statedata$Life.Exp)

lmod = lm(Life.Exp  ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(lmod)

lmod = lm(Life.Exp  ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(lmod)

lmod = lm(Life.Exp  ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(lmod)

predict(lmod)
sort(predict(lmod))

sort(abs((predict(lmod)) - statedata$Life.Exp))

sort(abs(sort(resid(lmod))))
