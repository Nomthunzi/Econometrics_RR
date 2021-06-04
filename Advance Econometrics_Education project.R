
install.packages("AER")
library("AER")
data("CollegeDistance")

install.packages("dplyr")
library(dplyr)


install.packages("devtools")
install.packages("hrbrthemes")
library(hrbrthemes)

install.packages("pscl")
install.packages("ucminf")
install.packages("ordinal")
install.packages("reshape")
install.packages("generalhoslem")
install.packages("oglmx")
install.packages("brant")
install.packages("aod")
install.packages("LogisticDx")



library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")
library("LogisticDx")
library("ucminf")
library("ordinal")
library("reshape")
library("generalhoslem")
library("oglmx")
library("aod")
library("brant")
library("ggplot2")
library("devtools")
library("dplyr")


#dependent variable :education
table(CollegeDistance$education)
# level 18 has very few observations. To improve the analysis I will add it to level 17. Students with some graduate and those with graduate degree will be combined together.

CollegeDistance$education[CollegeDistance$education == 18] <- 17
table(CollegeDistance$education)

#convert Variable education to factor and rename levels  - 6 being the higher level of education
CollegeDistance$education <-as.factor(CollegeDistance$education)
levels(CollegeDistance$education) <- c("1","2","3","4","5","6")
table(CollegeDistance$education)

#creating nonlinear variables

# Adding square wage variable to model the effect a differing wages (There might be non linear relationship between education and wage/score)
#a positive effect of wage/score squared means that as people earn more the effect is stronger and vice-versa.
#As people earn more, they have enough for tuition and hence will continue studies.


CollegeDistance <- CollegeDistance %>% 
  mutate(wage2 = (CollegeDistance$wage ^ 2))


#an interaction variable
#interaction occurs when the effect of one variable depends on another variable.
#an interaction between gender and scores will check if there exists differences between scores of male and female
#first, let's change levels of variable gender to a dummy numeric :0 male and 1 female



levels(CollegeDistance$gender) <- c("0","1")
table(CollegeDistance$gender)

# Box plot of score per level in both gender genders
bp <- ggplot(CollegeDistance, aes(CollegeDistance$education, CollegeDistance$score)) + 
  geom_boxplot(aes(fill = CollegeDistance$gender)) +
  theme_minimal() +
  theme(legend.position = "top")
bp

#male female 
#2139   2600 

CollegeDistance <- CollegeDistance %>% 
  mutate(Scorgen = (as.numeric(CollegeDistance$score)*as.numeric(CollegeDistance$gender)))


colSums(is.na(CollegeDistance)) %>% #no missing variables in all columns
  sort()


#the probit and logit model

#education is our dependent variable.
#let's first create the levels



#logit model
elogit <- polr(education ~ ., 
               data = CollegeDistance,
               method = "logistic",
               Hess = TRUE)

summary(elogit)
coeftest(elogit)

#Goodness-of-fit tests 
#we will use Hosmer-Lemeshow Tests because we have continuous and categorical covariates


logitgof(CollegeDistance$education, fitted(elogit), g = 7, ord = TRUE)

#pvalue is  >0,05, the we can reject the null that we have good specification 

#let's run the brant test to see if we need to use an advanced model.

brant(elogit)

#the Omnibus is 0 which means that the proportional odds assumption is violated.
#then we have to use ordered probit instead of logit ordered.

eprobit <- polr(education ~ ., 
                data=CollegeDistance,
                method = "probit")

summary(eprobit)

coeftest(elogit)

#C
#general to specific variable selection 

#the general model

eprobit <- polr(education ~ ., 
                data=CollegeDistance,
                method = "probit")


summary(eprobit)

coeftest(eprobit)
# stage1 Let's check if all the insignificant variables are all jointly insignificant
#let's run the model without insignificant variables: urban,unemp,region,wages,wage2

eprobit1a <- polr(education ~ gender + ethnicity + score + fcollege + mcollege + home + distance
                + tuition + income + Scorgen, 
                data=CollegeDistance,
                method = "probit")

anova(eprobit, eprobit1a)

# all insignificant variables are jointly significant
# therefore we have to drop variables in the way one after another
# let's drop "the most insignificant" variable from eprobit model 
# that is wage2

# Step 2

eprobit2 <- polr(education ~ gender + ethnicity + score + fcollege + mcollege + home + urban
          + unemp + wage + distance + tuition + income + region + Scorgen, 
          data= CollegeDistance,
          method = "probit")

summary(eprobit2)
coeftest(eprobit2)

# there are still insignificant variable in eprobit2 model
# let's drop "the insignificant" variable from eprobit2
# that is urban

# let's estimate model eprobit2 without variable "urban"
# and test joint hypothesis: beta_trade=beta_urban=0
# in the general model that is model eprobit

eprobit2a <- polr(education ~ gender + ethnicity + score + fcollege + mcollege + home +
                   unemp + wage + distance + tuition + income + region + Scorgen, 
                 data= CollegeDistance,
                 method = "probit")


anova(eprobit, eprobit2a)

#the p-value is greater than 0,05 which means that yes the model is better without variable urban as well

#the final model 

eprobit3 <- polr(education ~ gender + ethnicity + score + fcollege + mcollege + home +
                    unemp + wage + distance + tuition + income + region + Scorgen, 
                  data= CollegeDistance,
                  method = "probit")

summary(eprobit3)
coeftest(eprobit3)

#All variables are statistically significant.




# D: QUALITY PUBLICATION TABLE

install.packages("stargazer")
library("stargazer")

stargazer(elogit, eprobit, eprobit1a, eprobit2, eprobit3, type ="text")

stargazer(elogit, eprobit, eprobit1a, eprobit2, eprobit3, type ="html")

# The stargazer is very good because we can see directly the estimates of the 
# independent variables in the 2 models and compare them. In our example, all 
# the variables, except tuition, distance, region west, Scorgen and wage2, are 
# greater in the ordered logit than all the other ordered probit models. This difference
# is not relevant for our purpose because we are interested in the sign.
# About the interpretation of parameters, they have no quantitative interpretation,
# but only qualitative, i.e. we can interpret only the sign. Moreover, it's possible
# to interpret only the very first and last alternative and in our case the probability
# of the 1st alternative has opposite sign than the estimate, while the probability of the
# last alternative has the same sign.

# In our case, we have decided to compare the estimates of all the models, including the intermediate ones,
# but for our purpose we will put attention on ordered logit and the final ordered probit (5).
# In the case of ordered logit, all the variables except distance, tuition, regionwest, wage2 and Scorgen
# have positive sign so if they increase then the probability of the very first alternative (12 years of education)
# decreases, while the probability of the last alternative (18 years of education) increases.
# On the other hand, distance, tuition, region west and Scorgen have negative sign: if they increase, then the probability
# of 12 years of education increases (same sign), while the probability of 18 years of education
# decreases (same sign). Also wage2 has negative sign but we cannot consider it so much them
# since it's a polynomial.
# For the final model, the signs are the same for all the variables, except for wage,
# that now has a negative sign.

# As written in the note at the end of the table, * means p<0.1, ** means p<0.05 and *** means p<0.01,
# so we consider significant all the estimates that have at least 2 stars. Therefore, in the final ordered probit
# all the variables are significant, since we applied the general-to-specific approach.


# E: marginal effects

install.packages("erer")
library("erer")

mea <- ocME(w = eprobit3); mea

mea$out


# From marginal effects of the final model we can see that the p-values of all the 
# independent variables are lower than 0.05, except for regionwest, that has a 
# p-value equal to 0.072 in ME.3 and 0.05 in ME.4. So they are all statistically significant, except
# regionwest in these 2 cases.
# In general, we can say that distance, wage, tuition and regionwest have positive
# effect on the first 2 levels of education, while starting from level 3 of education
# their marginal effects become negative and so they affect negatively the levels 
# of education. For the rest of variables, they have effects opposite to the previous one.
# From this argument, we have to exclude scorgen because its marginal effects are always zero or very low. 




#F: Calculation of R2

# In the model requirements the professor asks for
# R2 McKelvey-Zavoina, count R2, and adjusted count R2, but
# we cannot calculate them because it's not a model with
# binary dependent variables.


options(scipen=99)

pR2(elogit)
pR2(eprobit)
pR2(eprobit3)

# llh (The log-likelihood from the fitted model)
# llhNull (The log-likelihood from the intercept-only restricted model)
# G2 (Minus two times the difference in the log-likelihoods)
# McFadden (McFadden's pseudo r-squared)
# r2ML (Maximum likelihood pseudo r-squared)
# r2CU (Cragg and Uhler's pseudo r-squared)

# All the R2 that we obtain allow us to compare models, they don't tell other things. A pseudo R-squared only has
# meaning when compared to another pseudo R-squared of the same type, on the same data, predicting the same outcome. 
# In this situation, the higher pseudo R-squared indicates which model better predicts the outcome.
# Looking to the three pseudo-R2 that we obtained (McFadden, Maximum Likelihood and
# Cragg-Uhler) we can say that the logit model is better than the  probit, which at the same time
# is better than the final model (probit3), since we obtained slightly higher values of these pseudo R2.
# McFadden R2 have no clear interpretation.




#unpack libraries

library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")
library("LogisticDx")
library("ucminf")
library("ordinal")
library("reshape")
library("generalhoslem")
library("oglmx")
library("aod")
library("brant")


install.packages("VGAM")
library("VGAM")

#install packages
install.packages("generalhoslem")
library("generalhoslem")

#We have concluded above that the eprobit3 model is our final model. We will now test for goodness of fit



# Goodness-of-fit tests for eprobit3
#H0 The model fits the data well
#H1 The model does not fit the data well

# Joint significance
eprobit3.unrestricted = polr(education ~ gender + ethnicity + score + fcollege + mcollege + home +
                               unemp + wage + distance + tuition + income + region, data=CollegeDistance) 
eprobit3.restricted = polr(education~1, data=CollegeDistance)

#Lipsitz goodness of fit test 
lipsitz.test(eprobit3.unrestricted) 
#since p-value = 0.05491 we fail to reject H0 and conclude that the model fits data well


# Pulksteinis Robinson test
pulkrob.chisq(eprobit3.unrestricted, c("gender"))
#since p-value = 0.1405 we fail to reject H0 and conclude that the model is a good fit

#Robustness of the model

# multicolinearity test for numeric variables

mydata1 <- data.frame(CollegeDistance[,c(3,8,9,10,11)])
round(cor(mydata1),2)
#According to the above results, we do not have a problem of multicolinearity among numeric predictors

#Check multicollinearity among all variables in the dataset ( categorical and numerical together).


install.packages("sjPlot")
library("sjPlot")

mydata <- data.frame(CollegeDistance)
mydata[] <- lapply(mydata,as.integer)
sjp.corr(mydata,na.deletion = c("listwise", "pairwise"),
        corr.method = c("pearson"))

#According to the above results, high correlation among predictors was found twice : variable wage and wage2 (its exponential variable) as well as variable gender and Scorgen.
#This means that only one of the pair can be used in the model.
#In the original paper, variable wage was not significant and was already excluded from the final model which is correct however
# the variables gender and Scorgen were both included in the final model though the marginal effects revealed the error.
# Hence we will run a new model without this variable.

#But generally speaking there is no problem  of multicolinearity among predictors since no value in the table (apart from cases indicated above) is higher than 50 for positive correlation or -50 for negative correlation.



##New probit model without scoregen variable
eprobit3a <- polr(education ~ gender + ethnicity + score + fcollege + mcollege + home +
                   unemp + wage2 + distance + tuition + income + region, 
                 data= CollegeDistance,
                 method = "probit")

summary(eprobit3a)
coeftest(eprobit3a)


# heteroskedacity 
eprobit3b <- hetglm(as.factor(education) ~ gender + ethnicity + score + fcollege + mcollege + home +
               unemp+ wage + distance + tuition + income + region,
             data =CollegeDistance,
             family = binomial(link = "probit"))

summary(eprobit3b)
coeftest(eprobit3b)

# results interpretation
# According to the result of the LR test for heteroskedasticity, the p-value < 0.05 hence we reject the null hypothesis and conclude that there is problem of heteroskedasticity..
#The best in this case would be to consider heteroskedastic probit model results instead of standard probit model.
# According to the results in the heteroskedastic model, the significant level of most of the predictors changed. none is significant at 0.001 level.
#if we set our significant level at 0.01, only variables ethnicityafam,score and father college would be significant.
#variables gender,ethnicity hispanic,mother college,unempl,distance tuition and income would be also significant at 0.05
#variables home, wage and region became inisgnificant.


#Qaulity publication table results after adding the heteroskedastic probit model as well as the model after multicollinearity check
stargazer(elogit, eprobit, eprobit1a, eprobit2, eprobit3,eprobit3a,eprobit3b, type ="text")

##Results interpretation
#The signs of the heteroskedastic model in the table are quite similar to those of  the standard probit final model. i,e The varibales which had positive signs still do in the heteroskedastic model. Same with the variables that had a negative sign.
#The only difference in the results is that some of the variables became insignificant in the heteroskedastic probit model
#Hence we omit to interprete their signs in the heteroskedastic model table column.



