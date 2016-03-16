import numpy as np

def DO_FFT(hvel,zvel,facq):
    
    punti = hvel.shape[-1]

    t = np.arange(punti)
    freq = np.fft.fftfreq(t.shape[-1],d=1./(facq))
    SH = np.fft.fft(hvel)
    F = np.abs(freq)
    AH = np.abs(SH*2/punti)

    SZ = np.fft.fft(zvel)
    AZ = np.abs(SZ*2/punti)
    
    return F,AH,AZ


def load_file(nome_file):
    try:
        dati = np.loadtxt(open(nome_file,"rb"),delimiter=",")
        print "Load data from file: "+nome_file
    except IOError as e:
        print "I/O error({0}): {1}".format(e.errno, e.strerror) 
        exit(7)

    x,y,z,t= dati[:,0], dati[:,1], dati[:,2], dati[:,3]
    return x,y,z,t




