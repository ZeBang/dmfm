from dataclasses import dataclass
from typing import Optional

import numpy as np
import pandas as pd
import rpy2.robjects as ro
from rpy2.robjects import numpy2ri
from rpy2.robjects.conversion import localconverter


@dataclass(eq=False)
class DynamicMatrixFactorModel_rmES:
    """
    A forecaster which predicts using Dynamic Matrix Factor Model implemented in R.
    """

    def __init__(self, r1, r2):
        """
        Initialize the model by specifying the ranks to be used.
        """

        self._r1 = r1
        self._r2 = r2

    # n_workers: Optional[int] = None

    def fit(self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None):
        """Fit the model--

        Parameters
        ----------
        y: pd.DataFrame
        past_covariates: Optional[pd.DataFrame]
            Not supported by this model (default=``None``).

        Returns
        -------
        self
        """

        self._y = y
        # print(y)

        # We assume that input data is of shape (T, 5200), we convert the data to a (T, 260, 20) tensor
        T = y.shape[0]
        y_array = y.to_numpy()

        y_tensor = np.zeros((T, 260, 20))
        for i in range(260):
            y_tensor[:, i, :] = y_array[:, (i * 20) : (i * 20 + 20)]

        # Convert the data and some parameters to R data type
        with localconverter(ro.default_converter + numpy2ri.converter):
            ro.globalenv["data"] = ro.conversion.py2rpy(y_tensor)
            ro.globalenv["r1"] = ro.conversion.py2rpy(self._r1)
            ro.globalenv["r2"] = ro.conversion.py2rpy(self._r2)

        # Running models in R, this is similar to writing a script and running it in command line

        r_script = """ 
        
        T = dim(data)[1]
        d1 = dim(data)[2]
        d2 = dim(data)[3]
        
        # calculate the trend.
        exponential.smooth <- function(x, lambda){
          if(length(lambda) > 1)
            stop("lambda must be a single number")
          if(lambda > 1 || lambda <= 0)
            stop("lambda must be between zero and one")
          xlam <- x * lambda
          xlam[1] <- x[1]
          stats::filter(xlam, filter = 1 - lambda, method = "rec")
        }
        
        
        trend = array(0,dim(data))
        for(i in 1:dim(data)[2]){
          for(j in 1:dim(data)[3]){
            trend[,i,j] = exponential.smooth(data[,i,j], 1/4)
          }
        }


        y = data - trend

        tenFM_ans = tenFM.est(y,r=c(r1,r2))
        
        Q1.hat = tenFM_ans$Q[[1]]
        Q2.hat = tenFM_ans$Q[[2]]
        f.hat <- tensor(tensor(aperm(tenFM_ans$x.hat,c(2,3,1)), t(Q1.hat), 1, 2),t(Q2.hat), 1, 2)
        out.F.MAR1 = tenAR.est(f.hat,P=1)
        
        ans_params = list(Q1=Q1.hat,Q2=Q2.hat,
                          A1=out.F.MAR1$A[[1]][[1]][[1]],A2=out.F.MAR1$A[[1]][[1]][[2]],
                          fhat_last=f.hat[T,,],trend_last=trend[T,,])
        ans_params
        """

        ans_params = ro.r(r_script)
        # Q1,Q2,A1,A2,fhat_last,data_trend_last = ans_params

        self.ans = ans_params
        # self.coef = [Q1,Q2,A1,A2,fhat_last]
        # self.trend_last = np.array(data_trend_last)

        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """Make predictions."""

        with localconverter(ro.default_converter + numpy2ri.converter):
            ro.globalenv["n_ahead"] = ro.conversion.py2rpy(len(x))
            ro.globalenv["ans"] = ro.conversion.py2rpy(self.ans)

        """
        with localconverter(ro.default_converter + numpy2ri.converter):
            ro.globalenv['n_ahead'] = ro.conversion.py2rpy(len(x))
            ro.globalenv['Q1'] = ro.conversion.py2rpy(self.coef[0])
            ro.globalenv['Q2'] = ro.conversion.py2rpy(self.coef[1])
            ro.globalenv['A1'] = ro.conversion.py2rpy(self.coef[2])
            ro.globalenv['A2'] = ro.conversion.py2rpy(self.coef[3])
            ro.globalenv['fhat_current'] = ro.conversion.py2rpy(self.coef[4])
            ro.globalenv['trend'] = ro.conversion.py2rpy(self.trend_last)

        
        pred = array(0,c(n_ahead,260*20))
        for(i in 1:n_ahead){
            fhat_current = A1 %*% fhat_current %*% t(A2)
            xx = Q1 %*% fhat_current %*% t(Q2)
            pred[i,] = array(t(xx+trend))
        }
        pred
        """

        r_script = """
        
        fhat_current = ans$fhat_last
        trend_last = ans$trend_last
        pred = array(0,c(n_ahead,260*20))
        for(i in 1:n_ahead){
            fhat_current = ans$A1 %*% fhat_current %*% t(ans$A2)
            xx = ans$Q1 %*% fhat_current %*% t(ans$Q2)
            pred[i,] = array(t(xx+trend_last))
        }
        pred
        """

        with localconverter(ro.default_converter + numpy2ri.converter):
            pred_np = ro.r(r_script)
        pred = pd.DataFrame(pred_np, columns=self._y.columns)
        pred[pred < 0] = 0
        pred.index = x

        return pred


@dataclass(eq=False)
class AveragingModel:
    """
    A forecaster which predicts using Dynamic Matrix Factor Model implemented in R.
    """

    def __init__(self, r1, r2):
        """
        Initialize the model by specifying the ranks to be used.
        """

        self._r1 = r1
        self._r2 = r2

    def fit(self, y: pd.DataFrame, past_covariates: Optional[pd.DataFrame] = None):
        """Fit the model--

        Parameters
        ----------
        y: pd.DataFrame
        past_covariates: Optional[pd.DataFrame]
            Not supported by this model (default=``None``).

        Returns
        -------
        self
        """

        self._y = y

        # We assume that input data is of shape (T, 5200), we convert the data to a (T, 260, 20) tensor
        T = y.shape[0]
        y_array = y.to_numpy()
        countries = y.columns.levels[0].tolist()
        events = y.columns.levels[1].tolist()
        self.d1 = len(countries)
        self.d2 = len(events)

        y_tensor = np.zeros((T, self.d1, self.d2))
        for i in range(self.d1):
            y_tensor[:, i, :] = y_array[:, (i * self.d2) : (i * self.d2 + self.d2)]

        # Convert the data and some parameters to R data type
        with localconverter(ro.default_converter + numpy2ri.converter):
            ro.globalenv["data"] = ro.conversion.py2rpy(y_tensor)
            ro.globalenv["r1"] = ro.conversion.py2rpy(self._r1)
            ro.globalenv["r2"] = ro.conversion.py2rpy(self._r2)
            ro.globalenv["countries"] = ro.conversion.py2rpy(countries)
            ro.globalenv["events"] = ro.conversion.py2rpy(events)

        # Running models in R, this is similar to writing a script and running it in command line

        r_script = """ 
        dimnames(data) = list(NULL, countries, events)
        T = dim(data)[1]
        steps = c("step1","step2","step3","step4")


        drop = 30
        w=1/8
        model_names = c("ZeroPred", "RandomWalkPred", "Exponential", "ARIMA", "DynamicFactor")
        num_model = length(model_names)
        n.ahead = 4
        d1 = dim(data)[2]
        d2 = dim(data)[3]
        pred.fac = array(0,c(d1,d2,4), dimnames = list(countries, events, steps))  # dynamic factor
        pred.wei = array(0,c(d1,d2,4), dimnames = list(countries, events, steps))  # weighted avg
        pred.w = array(0,c(d1,d2,4,num_model), dimnames = list(countries, events, steps, model_names))  # weights for each model, lag, i, j, step

        trend = array(0,dim(data),dimnames=dimnames(data))

        for(ii in dimnames(data)[[2]]){
            for(jj in dimnames(data)[[3]]){
              trend[,ii,jj] = exponential.smooth(data[,ii,jj], w)
            }
        }      
        
        inSampleResFact = array(0,c(d1,d2,4), dimnames = list(countries, events, steps))
        inSamplePredFact = array(0, c(T-drop+1, d1, d2, 4), dimnames = list(NULL, countries, events, steps))
        step = 1; insampleMASE = apply(abs(data[(1+step):T,,] - data[1:(T-step),,]), c(2,3), mean)
            
        # dynamic factor using all countries
        xx = data - trend
        res = tenFM.est(xx,c(r1,r2))
        res_A = array(0, c(n.ahead, r1, r1))
        res_B = array(0, c(n.ahead, r2, r2))
        Q1.hat = res$Q[[1]]
        Q2.hat = res$Q[[2]]
        f.hat <- tensor(tensor(res$x.hat, Q1.hat, 2, 1), Q2.hat, 2, 1)
        F.MAR1.pred = f.hat[1:T,,]
        for(step in 1:4){
            out.F.MAR1 = tenAR.MLEP(f.hat, PP=step)
            A = out.F.MAR1$A[[step]][[1]][[1]]
            B = out.F.MAR1$A[[step]][[1]][[2]]
            res_A[step,,] = A
            res_B[step,,] = B
            F.MAR1.pred = f.hat[1:T,,]
            F.MAR1.pred =  tensor(tensor(F.MAR1.pred, A, 2, 2), B, 2, 2)
            X.F.MAR1.pred = tensor(tensor(F.MAR1.pred, Q1.hat, 2, 2), Q2.hat, 2, 2)
            if (step==1) {dimnames(X.F.MAR1.pred) = dimnames(xx)}
            pred.fac[,,step] = X.F.MAR1.pred[T-step+1,,] + trend[T,,]
            inSamplePredFact[,,,step] = X.F.MAR1.pred[(drop-step):(T-step),,] + trend[(drop-step):(T-step),,]
            temp = aperm(array(insampleMASE, c(d1, d2, T+1-drop)), c(3,1,2))
            inSampleResFact[,,step] = apply(abs(xx[drop:T,,] - X.F.MAR1.pred[(drop-step):(T-step),,]) / temp, c(2,3), mean)
        }
              
        for(i in countries){
            for(j in events){
              y = array(data[1:T,i,j])
              if (sum(y)<=((T+1) %/% 2)) next
              if (sum(y)==0){
                pred.Arim = rep(0, 4)
              } else {
                fitArima = try(arima(y, order=c(1,1,1)))
                if(class(fitArima) != "try-error"){
                  pred.Arim = predict(fitArima, n.ahead = 4)$pred
                } else {
                  pred.Arim = rep(trend[T,i,j], 4)
                }
              }
              if(class(fitArima) != "try-error"){
                phi1 = fitArima$coef[1]
                theta1 = fitArima$coef[2]
                fittedArim = array(0, c(T, 4))
                fittedArim[,1] = fitted(fitArima)[1:T]
                fittedArim[4:T,2] = ((1+phi1)*fittedArim[3:T, 1] - phi1*y[2:(T-1)])[1:(T-3)]
                fittedArim[5:T,3] = ((1+phi1)*fittedArim[4:T, 2] - phi1*fittedArim[3:(T-1), 1])[1:(T-4)]
                fittedArim[6:T,4] = ((1+phi1)*fittedArim[5:T, 3] - phi1*fittedArim[4:(T-1), 2])[1:(T-5)]
              }
              fitted.Expo = array(trend[,i,j])

              for (step in c(1:4)){

                if(class(fitArima) != "try-error"){
                  # fitted.Arim = fitted(fitArima, h=step)[drop:T]
                  fitted.Arim = fittedArim[drop:T, step]
                } else {
                  fitted.Arim = fitted.Expo[(drop-step):(T-step)]
                }


                inSampleResRand = mean(abs(y[drop:T] - y[(drop-step):(T-step)])  / insampleMASE[i,j])
                inSampleResZero = mean(abs(y[drop:T]) / insampleMASE[i,j])
                inSampleResExpo = mean(abs(y[drop:T] - fitted.Expo[(drop-step):(T-step)]) / insampleMASE[i,j])
                inSampleResArim = mean(abs(y[drop:T] - fitted.Arim) / insampleMASE[i,j])


                inSampleRes = c(inSampleResZero, inSampleResRand, inSampleResExpo, inSampleResArim, inSampleResFact[i,j,step])
                
                
                if (sum(inSampleRes==0) == 0){
                  inSampleResInv = 1/inSampleRes
                  weights = inSampleResInv/sum(inSampleResInv)
                } else {
                  weights = rep(0, num_model)
                  weights[which.min(inSampleRes)] = 1
                }
                pred = c(0, y[T], fitted.Expo[T], pred.Arim[step], pred.fac[i,j,step])
                pred.w[i,j,step,] = weights
                pred.wei[i,j,step] = sum(weights * pred)
              }
            }
          }
        ans_params = list(
        pred.wei = pred.wei,
        pred.w = pred.w,
        A = res_A,
        B = res_B,
        Q1 = Q1.hat,
        Q2 = Q2.hat,
        model_names = model_names
        )
        ans_params
        """

        ans_params = ro.r(r_script)
        self.ans = ans_params

        return self

    def predict(self, x: pd.Index) -> pd.DataFrame:
        """
        Make predictions.

        """

        with localconverter(ro.default_converter + numpy2ri.converter):
            ro.globalenv["n_ahead"] = ro.conversion.py2rpy(len(x))
            ro.globalenv["ans"] = ro.conversion.py2rpy(self.ans)
            ro.globalenv["d1"] = ro.conversion.py2rpy(self.d1)
            ro.globalenv["d2"] = ro.conversion.py2rpy(self.d2)

        r_script = """
        
        pred = array(0, c(n_ahead, d1*d2))
        for(step in 1:4){
            pred[step,] = array(t(ans$pred.wei[,,step]))
        }
        
        pred
        """

        with localconverter(ro.default_converter + numpy2ri.converter):
            pred = pd.DataFrame(ro.r(r_script))
        pred[pred < 0] = 0
        pred.columns = self._y.columns

        return pred.set_axis(x)

    def get_weights(self):
        with localconverter(ro.default_converter + numpy2ri.converter):
            ro.globalenv["ans"] = ro.conversion.py2rpy(self.ans)

        r_script = """
        weights = ans$pred.w
        weights
        """

        with localconverter(ro.default_converter + numpy2ri.converter):
            weights = ro.r(r_script)

        return weights
