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
    
    filename=["../converted/TOT_20151215.dat","../converted/20151215.12r.dat"]

    k=0;
    A=[]
    F=[]

    for i in filename:
        #x,y,z,t=load_file(sys.argv[1])
        x,y,z,t=load_file(i)
        vel=np.sqrt(x*x+y*y)
        window=np.kaiser(vel.shape[-1],5)
        vel=vel*window
        f,a=DO_FFT(vel,10)
        F.append(f)
        A.append(a)
        ++k
    
    plt.figure(1)
    plt.subplot(131)
    plt.title("FFT della velocita` orizzontale 24h")
    plt.xlim(0.000001,1)
    plt.loglog(F[0],A[0],'k')

    plt.subplot(132)
    plt.title("FFT della velocita` orizzontale 1h")
    plt.xlim(0.000001,1)
    plt.loglog(F[1],A[1],'r')

    plt.subplot(133)
    plt.xlim(0.000001,0.001)
    plt.plot(np.diff(A[1]),'b')


    plt.savefig("FFT_hor.pdf", format="pdf")







