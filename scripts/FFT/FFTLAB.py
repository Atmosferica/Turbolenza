#!/usr/bin/python

#default module
import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack as fftp
import scipy.optimize as opt
import sys
import os
import string

from funct import *



if __name__ == '__main__':
    print "FFT - build periodigram"
    print "Usage: ./FFTLAB.py [File]\n"
    n = len(sys.argv)
    if n < 2:
        print "Error: too few arguments"
        exit(1)
    if n > 2:
        print "Error: too many arguments"
        exit(2)
        
    x,y,z,t=load_file(sys.argv[1])
    hvel=np.sqrt(x*x+y*y)
        
    pfz=np.polyfit(t,z,1)
    zz=pfz[0]*t+pfz[1]
    z= z-zz

    pfh=np.polyfit(t,hvel,1)
    hvell=pfh[0]*t+pfh[1]
    hvel=hvel-hvell
        
    F,AH,AZ=DO_FFT(hvel,z,10)


    plt.figure(1)

    plt.subplot(221)
    plt.title("Velocita` orizzontale")
    plt.plot(hvel,'k')

    plt.subplot(222)
    plt.title("FFT della velocita` orizzontale")
    plt.semilogy(F,AH,"k")
    
    plt.subplot(223)
    plt.title("Velocita` verticale")
    plt.plot(z,'k')


    plt.subplot(224)
    plt.title("FFT della velocita` verticale")
    plt.semilogy(F,AZ,"k")


    plt.figure(2)
    
    plt.subplot(211)
    plt.title("Velocita` orizzontale / Temperatura")
    plt.plot(hvel,t,'.')

    plt.subplot(212)
    plt.title("Velocita` verticale / Temperatura")
    plt.plot(z,t,'.')



    plt.show()






