#!/usr/bin/python

#default module
import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack as fftp
import scipy.optimize as opt
import sys
import os
import string


def f(f,sig2,fknee,alpha):
    return sig2*(1+(fknee/f)^alpha)


def DO_FFT(x,y,z):
    
    #Freq_campionamento = 1./(x[1]-x[0])

    Freq_campionamento = 10
    punti = y.shape[-1]
    
    #h_vel = np.sqrt(x*x+y*y)
    h_vel = z + 2

    t = np.arange(punti)
    freq = np.fft.fftfreq(t.shape[-1],d=1./(Freq_campionamento) )
    S = np.fft.fft(h_vel)
    F = np.abs(freq)
    A = np.abs(S*2/punti)

    #filtra
    S2=S
    S2[20:-20]=0
    h_vel2_c=fftp.ifft(S2)
    h_vel2_i=h_vel2_c.imag
    h_vel2_r=h_vel2_c.real

    h_vel2=np.sqrt(h_vel2_i*h_vel2_i+h_vel2_r*h_vel2_r)
    residui=(h_vel-h_vel2)

    SFR = np.fft.fft(residui)
    AFR = np.abs(SFR*2/punti)

    return F,AFR,A,residui,h_vel,h_vel2


def load_file(nome_file):
    try:
        dati = np.loadtxt(open(nome_file,"rb"),delimiter=",")
        print "Load data from file: "+nome_file
    except IOError as e:
        print "I/O error({0}): {1}".format(e.errno, e.strerror) 
        exit(7)

    x,y,z,t= dati[:,0], dati[:,1], dati[:,2], dati[:,3]
    return x,y,z,t


def return_peak(F,A):
    peak = np.array([]);
    massimo=0.
    
    for i in range(len(F)/2-1):
        if(abs(A[i+1]-A[i])> 1.3 ):
            massimo=A[i]
            if(A[i+1] > massimo):
                massimo=F[i+1]
                np.append(peak,[massimo])
            print massimo


    return peak



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
    #F,AFR,A,residui,h_vel,h_vel2=DO_FFT(x,y,z)


    Vwind = 313.3*np.sqrt(1+t/273.15)
    #for i in xrange(1,len(deltaV)):
    #    deltaV[i]=deltaV[i]-deltaV[i-1]
    #deltaV[0]=0
   
    z+=Vwind

    #--------------------------------------------------------------------------
    #FFT - velocity and residuals
    #--------------------------------------------------------------------------
    #plt.figure()

    #plt.subplot(221)
    #plt.title("Applicazione filtro con FFT")
    #plt.plot(h_vel,"red",h_vel2,'.')

    #plt.subplot(222)
    #plt.ylim([0.001,0.015])
    #plt.xlim([0.009,0.1])
    #plt.title("FFT della velocita`")
    #plt.loglog(F,A,"k")
    
    #plt.subplot(223)
    #plt.title("Residui")
    #plt.plot(residui,"k")

    #plt.subplot(224)
    #plt.ylim([0.001,0.015])
    #plt.xlim([0.009,0.1])
    #plt.title("FFT-residui")
    #plt.loglog(F,AFR,"k")

    ##-------------------------------------------------------------------------
    ##Correlation
    ##-------------------------------------------------------------------------

    plt.figure()
    
    #plt.subplot(121)
    #plt.title("Correlation VH-temp")
    #plt.plot(np.sqrt(x*x+y*y),t,".")
    
    #plt.subplot(122)
    plt.title("Correlation VZ-temp")
    plt.plot(t,z,".")
    
    plt.show()


