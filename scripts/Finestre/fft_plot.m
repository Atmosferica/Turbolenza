function [f,Y]= fft_plot(y,TC,N)
    Y = fft(y,N);
    L = length(Y);
    LR = floor(L/2)+1;
    f = linspace(0,1/TC,L+1);
    f = f(1:end-1);
    f = [-1/TC+f(LR+1:end) f(1:LR)];
    Y = [Y(LR+1:end) Y(1:LR)];
end