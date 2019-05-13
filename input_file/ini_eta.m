clear all;clc;
dx=1;
dy=1;
x=[1:dx:400];
y=[1:dy:200];

n=length(x);
m=length(y);

eta0=zeros(m,n)-2;

a=100/dx;%j=a+1;

for i=1:m
    for j=1:a
        eta0(i,j)=0;
    end
end

x=x';


pcolor(x,y,eta0);
shading interp


if(exist('eta0.txt'))
    delete('eta0.txt')
end

save -ASCII eta0.txt eta0

% plot(x,-dep(2,:))