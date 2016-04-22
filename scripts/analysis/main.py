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
from histogram import *
from periodogram import *


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


    try:
        periodogram(x,y,z,t,name)
        print ""
        print_ok("Stage1 completed - FFT")
        print ""
    except:
        print_fail("Some eorros has been occured in the Stage1")
        exit(1)

    #try:
    #    histogram(x,y,z,t,name)
    #    print ""
    #    print_ok("Stage2 completed - COR")
    #    print ""
    #except:
    #    print_fail("Some eorros has been occured in the Stage2")
    #    exit(1)
