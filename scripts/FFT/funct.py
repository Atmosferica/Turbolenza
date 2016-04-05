import numpy as np
from bcolors import *


def DO_FFT(vel,facq):
    try: 
        punti = vel.shape[-1]
        t = np.arange(punti)
        freq = np.fft.fftfreq(t.shape[-1],d=1./(facq))  
        F = np.abs(freq)
        S = np.fft.fft(vel)
        A = np.abs(S*2/punti)
        print bcolors.OKGREEN+"* "+bcolors.ENDC+"FFT done with no errors."
        return F,A

    except IndexError as ie:
        print bcolors.FAIL+"Index error({0}) during FFT: {1}".format(ie.errno, ie.strerror)+bcolors.ENDC
        return 0,0


def load_file(nome_file):
    try:
        dati = np.loadtxt(open(nome_file,"rb"),delimiter=",")
        print bcolors.OKGREEN+"* "+bcolors.ENDC+"Load data from file: "+nome_file
    except IOError as e:
        print bcolors.FAIL+"I/O error({0}): {1}".format(e.errno, e.strerror)+bcolors.ENDC
        exit(7)

    x,y,z,t= dati[:,0], dati[:,1], dati[:,2], dati[:,3]
    return x,y,z,t




