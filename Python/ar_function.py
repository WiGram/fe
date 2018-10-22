# -*- coding: utf-8 -*-
"""
Created on Sat Sep 22 18:53:57 2018

@author: wigr11ab
"""

def ar_function(sig, mu, rho, periods):
    y = np.zeros(len(spread))
    
    eps = np.random.normal(0,1,periods)
    
    for t in range(periods):
        y[t] = mu + rho * y[t-1] + eps[t]
        
    return y
