
%clear all
%close all

%Facq=10
%[x y z t] = importfile('../../data/TOT.dat',1,5497276);

%hvel=((x.*x) + (y.*y)).^(0.5);
%time=[1:length(hvel)]/Facq;

wo=ones(1,length(1000));
wo=wo/sum(wo)*length(wo);

wk=kaiser(length(1000),5);
wk=wk/sum(wk)*length(wk);

wh=hanning(length(1000));
wh=wh/sum(wh)*length(wh);

%zz= z.*wk;

%[f,Y] = fft_plot(zz',0.1,1000);

[fo,Yo] = fft_plot(wo,0.1,1000);
%[fk,Yk] = fft_plot(wk',0.1,1000);
%[fh,Yh] = fft_plot(wh',0.1,1000);

figure(1)
%semilogx(f,abs(Y)/length(Y));
%axis([0 1 0 0.07])
%grid on;
%figure(2)
semilogy(fo,abs(Yo))
%hold on
%semilogy(fh,abs(Yh))
%hold on
%semilogy(fk,abs(Yk))






