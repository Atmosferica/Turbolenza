#!/usr/bin/python

#default module
import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack as fftp
import scipy.optimize as opt
import sys
import os
import string

from bcolors import *
from funct import *



if __name__ == '__main__':
    print "FFT - build periodogram"
    print "Usage: ./periodogram.py [File]\n"
    n = len(sys.argv)
    if n < 2:
        print bcolors.FAIL+"Error: too few arguments"+bcolors.ENDC
        exit(1)
    if n > 2:
        print bcolors.FAIL+"Error: too many arguments"+bcolors.ENDC
        exit(2)
    
    k=0;
    A=[]
    F=[]
    
    name=sys.argv[1]
    name=name.split('/')
    name=name[len(name)-1]
    name=name.split('.')[0]

    
    x,y,z,t=load_file(sys.argv[1])
    vel=np.sqrt(x*x+y*y)
    components=[vel,z]

    for i in range(0,2):
        window=np.kaiser(components[i].shape[-1],5)
        vel=components[i]*window
        f,a=DO_FFT(vel,20)
        F.append(f)
        A.append(a)
    
    plt.figure(1)
    plt.subplot(121)
    plt.title("FFT horizontal velocity")
    #plt.xlim(0.000001,5)
    plt.loglog(F[0],A[0],'k')

    plt.subplot(122)
    plt.title("FFT vertical velocity")
    #plt.xlim(0.000001,5)
    plt.loglog(F[1],A[1],'r')

    try:
        plt.savefig("graph/"+name+"_FFT.pdf", dpi=20, format="pdf")
        print bcolors.OKGREEN+"* "+bcolors.ENDC+"Graph saved in: "+"data/"+name+"_FFT.pdf"
    except IOError as IoE:
        print bcolors.FAIL+"I/O Error! Erro number = {0} ; {1}".format(IoE.errno,IoE.strerror)+bcolors.ENDC
