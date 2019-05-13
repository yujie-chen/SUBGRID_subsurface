clc;clear all;

stru=load('../input_file/stru.txt');
impr=load('../input_file/impr.txt');

[a b]=size(stru);

% stru(find(stru==2))=NaN;

% eta0=load('../input_file/eta0.txt');
eta0=load('../Output/eta_0005');

% for i=1:a
%     for j=1:b
%         if(soil(i,j)==2)
%             soil(i,j)=nan;
%         end
%     end
% end


% x=[0:1:400];
% y=[0:1:200];
x=[1:1:400];
y=[1:1:200];

m1=195;m2=205;
n1=95;n2=105;
xs=[m1:1:m2];
ys=[n1:1:n2];

% csoil=cat(3,zeros(size(stru))+0.7,zeros(size(stru))+0.7,zeros(size(stru))+0.65);
% cwater=cat(3,zeros(size(eta0)),zeros(size(eta0))+0.2,zeros(size(eta0))+0.8);

figure(1)
% s=surf(xs,ys,-stru(n1:n2,m1:m2),csoil,'facealpha',0.7);
s=surf(xs,ys,-stru(n1:n2,m1:m2),'facealpha',0.7);

hold on
% p=surf(x,y,-impr,csoil,'facealpha',0.7);
p=surf(x,y,-impr,'facealpha',0.7);

set(p,'edgecolor','none')
% set(s,'edgecolor','none')
camlight;
lighting gouraud;
hold on
% w=surf(x,y,eta0,cwater,'facealpha',0.3);
% (2:end-1,2:end-1)
% w=surf(x,y,eta0(2:end-1,2:end-1),cwater,'facealpha',0.3);
w=surf(x,y,eta0(2:end-1,2:end-1),'facealpha',0.3);

set(w,'edgecolor','none')

view(30,77);
xlabel('x (m)');ylabel('y (m)');zlabel('z (m)');
axis tight

% set(gcf,'Position',[100,300,500,400])
