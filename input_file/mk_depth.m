clear all;clc;
dx=1;
dy=1;
x=[1:dx:400]; % 264
y=[1:dy:200];

n=length(x);
m=length(y);

stru=zeros(m,n)+2;
impr=zeros(m,n)+2;

for j=1:n
    for i=1:m
        if((dy*i)>95 && (dy*i)<105 && (dx*j)>195 && (dx*j)<205)
            stru(i,j)=1;
        end
    end
end

a=100/dx;%j=a+1;

for i=1:m
    if((dy*i)<90 || (dy*i)>110)
        impr(i,a+1)=0;
        impr(i,a)=0;
    end
end

x=x';
figure(2)
subplot(211)
pcolor(x,y,stru);
shading interp
subplot(212)
pcolor(x,y,impr);
shading interp

if(exist('stru.txt'))
    delete('stru.txt')
end
if(exist('impr.txt'))
    delete('impr.txt')
end
save -ASCII stru.txt stru
save -ASCII impr.txt impr
% plot(x,-dep(2,:))