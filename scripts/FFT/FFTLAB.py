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


def DO_FFT(x,y):
    
    Freq_campionamento = 1./(x[1]-x[0])
    punti = y.shape[-1]
    
    h_vel = np.sqrt(x*x+y*y)

    t = np.arange(punti)
    freq = np.fft.fftfreq(t.shape[-1],d=1./(Freq_campionamento) )
    S = np.fft.fft(h_vel)
    F = np.abs(freq)
    A = np.abs(S*2/punti)

    #filtra
    S2=S
    S2[26:-26]=0
    h_vel2_i=fftp.ifft(S2).imag
    h_vel2_r=fftp.ifft(S2).real
    h_vel2=np.sqrt(h_vel2_i*h_vel2_i+h_vel2_r*h_vel2_r)


    plt.subplot(311)
    plt.title("Velocita`")
    plt.plot(h_vel,'k')


    #plt.subplot(412)
    #plt.yscale('log')
    #plt.xscale('log')
    #plt.plot(F,A,'k',F,A,'bo')

    plt.subplot(312)
    plt.title("Applicazione filtro con FFT")
    plt.plot(h_vel,"red",h_vel2,'.')

    plt.subplot(313)
    plt.title("Sottrazione componente a bassa frequenza")
    plt.plot(h_vel-h_vel2,"k")


    plt.show()


def load_file(nome_file):
    try:
        dati = np.loadtxt(open(nome_file,"rb"),delimiter=",")
        print "Load data from file: "+nome_file
    except IOError as e:
        print "I/O error({0}): {1}".format(e.errno, e.strerror) 
        exit(7)

    x,y,z= dati[:,0], dati[:,1], dati[:,2]
    return x,y,z

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
    
    x,y,z=load_file(sys.argv[1])
    DO_FFT(x,y)


