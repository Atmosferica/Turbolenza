
%clear all
%close all

%Facq=10
%[x y z t] = importfile('20160129.11r.dat',3,18892);

hvel=((x.*x) + (y.*y)).^(0.5);
time=[1:length(hvel)]/Facq;

wo=ones(1,length(z));
wo=wo/sum(wo)*length(wo);

wk=kaiser(length(z),5);
wk=wk/sum(wk)*length(wk);

wh=hanning(length(z));
wh=wh/sum(wh)*length(wh);

zz= z.*wk;

[f,Y] = fft_plot(zz',0.1,length(zz));

[fo,Yo] = fft_plot(wo,0.1,length(zz));
[fk,Yk] = fft_plot(wk',0.1,length(zz));
[fh,Yh] = fft_plot(wh',0.1,length(zz));

figure(1)
semilogx(f,abs(Y)/length(Y));
axis([0 1 0 0.07])
grid on;
figure(2)
semilogy(fo,abs(Yo))
hold on
semilogy(fh,abs(Yh))
hold on
%semilogy(fk,abs(Yk))






