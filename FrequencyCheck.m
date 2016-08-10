[y,Fs] = audioread('Knockin.wav');
nf = 44100; %number of point in DTFT
Y = fft(y,nf);
f = Fs/2*linspace(0,1,nf/2+1);
plot(f,abs(Y(1:nf/2+1)));