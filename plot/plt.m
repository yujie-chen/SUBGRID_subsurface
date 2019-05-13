clc;clear all;

i=25;
% eta0=load('../input_file/eta0.txt');
eta = load(['../Output/eta_00',num2str(i)]);
es = load(['../Output/Etascreen_00',num2str(i)]);
hu = load(['../Output/hu_00',num2str(i)]);
du = load(['../Output/Dugrn_00',num2str(i)]);
m = load(['../Output/mask_00',num2str(i)]);
mg = load(['../Output/maskgrn_00',num2str(i)]);
p = load(['../Output/p_00',num2str(i)]);
pgrn = load(['../Output/pgrn_00',num2str(i)]);

% eta = load(['../Output/eta_0',num2str(i)]);
% es = load(['../Output/Etascreen_0',num2str(i)]);
% hu = load(['../Output/hu_0',num2str(i)]);
% du = load(['../Output/Dugrn_0',num2str(i)]);
% m = load(['../Output/mask_0',num2str(i)]);
% mg = load(['../Output/maskgrn_0',num2str(i)]);
% u = load(['../Output/u_0',num2str(i)]);
% v = load(['../Output/v_0',num2str(i)]);


% eta = load(['../Output/eta_000',num2str(i)]);
% es = load(['../Output/Etascreen_000',num2str(i)]);
% hu = load(['../Output/hu_000',num2str(i)]);
% du = load(['../Output/Dugrn_000',num2str(i)]);
% m = load(['../Output/mask_000',num2str(i)]);
% mg = load(['../Output/maskgrn_000',num2str(i)]);
% u = load(['../Output/u_000',num2str(i)]);
% v = load(['../Output/v_000',num2str(i)]);


% for i=1:a
%     for j=1:b
%         if(soil(i,j)==2)
%             soil(i,j)=nan;
%         end
%     end
% end

% % x=[0:1:400];
% % y=[0:1:200];
% x=[1:1:400];
% y=[1:1:200];
% 
% m1=195;m2=205;
% n1=95;n2=105;
% xs=[m1:1:m2];
% ys=[n1:1:n2];

figure(i)
subplot(421)
pcolor(eta);
shading interp
title('eta')
subplot(422)
pcolor(es);
shading interp
title('etascreen')
subplot(423)
pcolor(hu);
shading interp
title('hu')
subplot(424)
pcolor(du);
shading interp
title('Du')
subplot(425)
pcolor(m);
shading interp
title('mask')
subplot(426)
pcolor(mg);
shading interp
title('maskground')
subplot(427)
pcolor(p);
shading interp
title('p')
subplot(428)
pcolor(p);
shading interp
title('pgrn')