*** Vector Error-correction model and multivariate cointegration ***
clear all
use http://www.stata-press.com/data/r13/balance2
describe
*y is ln(GDP) 
*i is ln(income) 
*c is ln(consumption)

tsset t
tsline y i c
dfuller d.y
dfuller d.i
dfuller d.c

dfuller d2.y
dfuller d2.i
dfuller d2.c

tsline d.(y i c)
tsline d2.(y i c)

reg y i c
estat dwatson
reg i y c
estat dwatson
reg c y i
estat dwatson

reg d.(y i c)
estat dwatson
reg d.(i y c)
estat dwatson
reg d.(c y i)
estat dwatson

//therefore, not the initial time series but there growth rates are most likely cointegrated.

// to choose number of lags in VAR
varsoc d.(y i c)
varsoc d2.(y i c)

var d2.(y i c), lags(1/3)

vargranger

//Johansen test is implemented to choose number of cointegrating equations with the given above number of lags
vecrank d.(y i c), lags(2) 
vecrank d2.(y i c), lags(3) 

// 1st order cointegration is not rejected
vecrank d.(y i c), lags(2) trend(rtrend) // trend in the Y_t model for cointegration
vecrank d.(y i c), lags(2) trend(constant) // constant in the Y_t model
vecrank d.(y i c), lags(2) trend(rconstant) // constant in the Y_t model for cointegration
// no cointegration in the model
vecrank d.(y i c), lags(2) trend(none) // include nothing
* r=0 - all Y_t are I(1) processes without cointegration
* r=max_rank - all Y_t are I(0) (stationary) processes
* take the lowest max rank where test statistics less than critical value

vecrank d.(y i c), lags(2)
vecrank d.(y i c), lags(2) max notrace levela //  maximum-eigenvalue statistic instead of trace statistics
// levela - report both 1% and 5% critical values
vecrank d.(y i c), lags(2) ic notrace // SBIC, HQIC, AIC criteria

*vec - comand for vector error-correction model. the model for the initial time series
vec y i c
* compare the estimated model with the one above. d2.x means the growth rate of x to the value two periods before.
vec d.(y i c) 
predict ce1, ce //predictors from the 1st cointegration equation



tsline D2.y D2.c ce

vecstable, graph // If the process is stable, the moduli of the remaining r eigenvalues are strictly less than one
//The graph of the eigenvalues shows that none of the remaining 
//eigenvalues appears close to the unit circle. The stability check does not indicate that our model is misspecifed

//we use veclmar to test for serial correlation in the residuals
 veclmar, mlag(4)

//We use vecnorm to test the null hypothesis that the errors are normally distributed
vecnorm

//forecasting 
fcast compute m1_, step(24)
fcast graph m1_D_y
