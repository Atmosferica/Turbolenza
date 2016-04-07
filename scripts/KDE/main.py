#!/usr/bin/python

#default module
import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack as fftp
import scipy.optimize as opt
import sys
import os
import string

from statsmodels.nonparametric.kernel_density import KDEMultivariate

from bcolors import *
from funct import *


def kde_m(x, x_grid, bandwidth):
    
    #kde = KDEMultivariate(x, bw=bandwidth * np.ones_like(x),var_type='c')
    kde = KDEMultivariate(x, bw=[0.004,0], var_type='c')
    print kde.bw

    return kde.pdf(x_grid)





if __name__ == '__main__':
    print "FFT and KD estimation"
    print "Usage: ./main.py [File]\n"
    n = len(sys.argv)
    if n < 2:
        print_fail("Error: too few arguments")
        exit(1)
    if n > 2:
        print_fail("Error: too many arguments")
        exit(1)
   
    x,y,z,t=load_file(sys.argv[1])
    
    name=sys.argv[1]
    name=name.split('/')
    name=name[len(name)-1]
    name=name.split('.')[0]+"."+name.split('.')[1]
    
    z_grid = np.linspace(-0.2, 0.2, 5000)
    pdf=kde_m(z, z_grid, 0.003)

    plt.figure(1)
    plt.plot(z_grid, pdf, color='red', lw=1)
    plt.hist(z,40, normed=True)
    plt.show()


