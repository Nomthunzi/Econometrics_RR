# Econometrics_RR
This repository has source documentation for Econometrics analysis where AER library was used.
The study has used a dataset called College Distance that comes from a survey 
conducted by the depqrtment of Educqtion in 1980.
Source: https://www.britannica.com/topic/education/Education-and-economic-development
The aim of the paper was to identify externally induced factors which can affect an individuals’ level of education. 
A standard probit model was build to explain the existing relationship however some important tests as such multicollinearity test and heteroskasticity test were ignored
In order to improve the paper we performed these two tests. Thankflully the results showed that there is no problem of multicollinearity and heteroskedasticity however in case these problems were identified,for each pair of correlated predictors only one would have been included in the model, if heteroskedasticity was present it would have meant that
the standard errors that are shown in the output table of the regression may be unreliable and probably using the heteroskedastic probit model would have been more efficient.
Another observation is that when a heteroskedastic model is considered, almost all predictors are significant at p-value equals 0,1 only.
