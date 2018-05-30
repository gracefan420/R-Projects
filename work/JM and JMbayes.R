library(nlme)
library(survival)
library(JM)
library(coda)
library(rjags)

library(JMbayes)

# linear mixed model fit (random intercepts + random slopes)
fitLME <- lme(log(serBilir) ~ drug * year, random = ~ year | id,correlation=corCompSymm(), data = pbc2)
summary(fitLME)

# survival regression fit
fitSURV <- coxph(Surv(years, status2) ~ drug, data = pbc2.id, x = TRUE)

# joint model fit, under the (default) Weibull model
fitJOINT_jm <- jointModelBayes(fitLME, fitSURV, timeVar = "year")
fitJOINT_jm
summary(fitJOINT_jm)

fitJOINT <- jointModel(fitLME, fitSURV, timeVar = "year")
fitJOINT
summary(fitJOINT)

Event Process
Value Std.Err  z-value p-value
(Intercept)   -4.4038  0.2734 -16.1065 <0.0001
drugD-penicil  0.0377  0.1799   0.2095  0.8340
Assoct         1.2396  0.0931  13.3196 <0.0001 *********
log(shape)     0.0177  0.0828   0.2139  0.8306

# Survival probabilities for a new patient i that has provided a set of bilirubin measurements 
# up to a time t
# e.g. patient 2 and 25 have provided us with 9 and 12 serum bilirubin measurements respectively
# 
# - dynamic prediction : survival probabilities are updated as additional information is recorded
# each time the patient come to the clinc and has new measurement(biomarkder), the prediction updated update.default

newd=pbc2[pbc2$id == "2", ]
newd=pbc2[pbc2$id == "25", ]

sfit_jm <- survfitJM(fitJOINT, newdata = pbc2[pbc2$id == "2", ])
for (i in 1:nrow(newd)) {
  newd1=newd[1:i,]
  sfit_jmbayes <- survfitJM(fitJOINT_jm, newdata = newd1)
  plot(sfit_jmbayes, estimator='mean',
       include.y=TRUE, conf.int=T,fill.area=T, col.area='lightgrey') #Y is the longitudinal data
}
  
library(shiny)
runApp(file.path(.Library,'JMbayes/demo'))


