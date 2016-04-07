import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack as fftp
import scipy.optimize as opt
import sys
import os
import string

from bcolors import *
from funct import *


def periodogram(x,y,z,t,name):
    vel=np.sqrt(x*x+y*y)
    components=[vel,z]
    F=[]
    A=[]

    for i in range(0,len(components)):
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
        print_ok("Graph saved in: "+"graph/"+name+"_FFT.pdf")
    except IOError as IoE:
        print_fail("I/O Error! Erro number = {0}; {1}".format(IoE.errno,IoE.strerror))
        exit(IoE.errno)
