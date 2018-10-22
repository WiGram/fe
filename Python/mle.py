# -*- coding: utf-8 -*-
"""
Created on Thu Sep 20 22:18:21 2018

@author: wigr11ab
"""
!cls
import sys
sys.path.append("C:/Users/wigr11ab/Dropbox/KU/K3/FE/Code/Python/")
from ar_function import ar_function
import pandas as pd
import numpy as np
from scipy.optimize import minimize
from matplotlib import pyplot as plt

data   = pd.read_excel("C:/Users/wigr11ab/Dropbox/KU/K3/FE/Exercises/USspreadraw.xls")
spread = data.Y_uncor
end    = len(spread)

test = ar_function(theta[1], theta[0], theta[2], len(spread))

def llf_ar(theta, y):
    end = len(y)
    mu, sd, rho  = theta
    
    mu_cond = mu + rho * y[:end-1]
    log_sd2 = np.log(sd ** 2)
    
    return - 0.5 * sum(log_sd2 + ((y[1:] - mu_cond) / sd) ** 2)

theta = np.array([ 5, 1, 0.05])

llf_ar(theta, test)

help(minimize)

minimize(llf_ar, test, args = theta, method = "L-BFGS-B")
