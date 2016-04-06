import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack as fftp
import scipy.optimize as opt
import sys
import os
import string

from bcolors import *
from funct import *


def histogram(x,y,z,t,name):
    vel=np.sqrt(x*x+y*y)
    components=[vel,z]

    Npoint = len(z);
    corr_hor=[]
    corr_ver=[]
    
    #correlazioni
    for i in range(1,2000):
        corr_hor.append(np.corrcoef(vel[:-i],vel[i:])[0][1])
        corr_ver.append(np.corrcoef(z[:-i],z[i:])[0][1])

    plt.figure(2)
    plt.subplot(121)
    plt.title("zvel")
    plt.hist(z, bins=20)

    plt.subplot(122)
    plt.title("hvel")
    plt.hist(vel,bins=50)

    try:
        plt.savefig("graph/"+name+"_HIST.pdf", format="pdf")
        print bcolors.OKGREEN+"* "+bcolors.ENDC+"Graph saved in: "+"graph/"+name+"_HIST.pdf"
    except IOError as IoE:
        print bcolors.FAIL+"I/O Error! Erro number = {0} ; {1}".format(IoE.errno,IoE.strerror)+bcolors.ENDC


    plt.figure(3)
    plt.subplot(121)
    plt.title("Correlation index -- horizontal")
    plt.plot(corr_hor)
    plt.subplot(122)
    plt.title("Correlation index -- vertical")
    plt.plot(corr_ver)
    
    try:
        plt.savefig("graph/"+name+"_CORR.png", figuresize=(8,6),dpi=80, format="png")
        print bcolors.OKGREEN+"* "+bcolors.ENDC+"Graph saved in: "+"graph/"+name+"_CORR.png"
    except IOError as IoE:
        print bcolors.FAIL+"I/O Error! Erro number = {0} ; {1}".format(IoE.errno,IoE.strerror)+bcolors.ENDC

