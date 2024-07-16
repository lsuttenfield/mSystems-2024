%%   IC and various parameters

Nl=6; % number of lysogen pairs

% fraction in each location
F=[1/6 1/6 1/6 1/6 1/6 1/6]; 
% fraction lytic
PHI = [0.0 0.0 0.0 0.0 0.0 0.0];
% Initial conditions first L5 and L6
P01 = [zeros(1,Nl-2) F(5)*(1-PHI(5))*1e-4  F(6)*(1-PHI(6))*1e-4  zeros(1,Nl)  zeros(1,Nl)]; 
% phase 1 time
Tend1 =10;

options=odeset('RelTol', 1e-9, 'NonNegative', 1:1:3*Nl);

% SPI
Q= [0.1 0.102 0.17 0.175 0.288 0.293];

%                 lysogens6noS(T, P, Q)
[T1,P1]=ode45(@(t,p)lysogens6noS(t, p, Q),[0,Tend1],P01,options);
%%
L1  = zeros(length(T1),Nl);
IQ1 = zeros(length(T1),Nl);
V1  = zeros(length(T1),Nl);

for i=1:Nl
L1(:,i)=P1(:,i);							
IQ1(:,i)=P1(:,Nl+i);
V1(:,i)=P1(:,2*Nl+i);
end
     
   %% phase 2 time
Tend2 =28.8-Tend1;
% Initial Conditions first L5 and L6 then L3 and L4 at 10 time units
P02 = [zeros(1,2) F(3:4).*(1-PHI(3:4))*1e-4  P1(end,5) P1(end,Nl) zeros(1,4) P1(end,Nl+5) P1(end,2*Nl) zeros(1,4) P1(end,2*Nl+5) P1(end,3*Nl)]; 

[T2,P2]=ode45(@(t,p)lysogens6noS(t, p, Q),[0,Tend2],P02,options);


L2  = zeros(length(T2),Nl);
IQ2 = zeros(length(T2),Nl);
V2  = zeros(length(T2),Nl);

for i=1:Nl
L2(:,i)=P2(:,i);							
IQ2(:,i)=P2(:,Nl+i);
V2(:,i)=P2(:,2*Nl+i);
end   
   
%% now concatenate
T = [T1' (T2+Tend1)']';
L = [L1' L2']';
IQ = [IQ1' IQ2']';
V = [V1' V2']';

    
%% stage 3 time 
TT=28.8*7;
Tend3 =TT-Tend2;
% Initial Conditions first L5 and L6, then L3 and L3 at 10 time units then
% L1 and L2 after one day
P03 = [F(1:2).*(1-PHI(1:2))*1e-4  L(end,3) L(end,4) L(end,5) L(end,Nl) zeros(1,2) IQ(end,3) IQ(end,4) IQ(end,5) IQ(end,Nl) zeros(1,2) V(end,3) V(end,4) V(end,5) V(end,Nl)]; 


[T3,P3]=ode45(@(t,p)lysogens6noS(t, p, Q),[0,Tend3],P03,options);

L3  = zeros(length(T3),Nl);
IQ3 = zeros(length(T3),Nl);
V3  = zeros(length(T3),Nl);

for i=1:Nl
L3(:,i)=P3(:,i);							
IQ3(:,i)=P3(:,Nl+i);
V3(:,i)=P3(:,2*Nl+i);
end

Tall = [T1' (T2+Tend1)' (T3+Tend1+Tend2)']';
Lall = [L1' L2' L3']';
IQall = [IQ1' IQ2' IQ3']';
Vall = [V1' V2' V3']';

%% 
   
figure(102)
   plot(Tall,Lall(:,1),'-','Color', [0.0 0.7 0.4],'Linewidth',3);
   hold on
   plot(Tall,Lall(:,2),'--','Color', [0.0 0.7 0.4],'Linewidth',3);
   plot(Tall,Lall(:,3),'-','Color', [0.9 0.7 0.0],'Linewidth',3);
   plot(Tall,Lall(:,4),'--','Color', [0.9 0.7 0.0],'Linewidth',3);
   plot(Tall,Lall(:,5),'-','Color', [0.95 0.0 0.3],'Linewidth',3);
   plot(Tall,Lall(:,6),'--','Color', [0.95 0.0 0.3],'Linewidth',3);
   hold off
   legend('L_1','L_2','L_3','L_4','L_5','L_6','Location','East','Fontsize',12,'FontName','Arial')
   ylabel('L','Rotation',0,'Fontsize',12,'FontName','Arial');
   xlabel('time (generations)','Fontsize',12,'FontName','Arial');
   axis([0 180 0 0.53]) 
   ax4 = gca(); 
   ax4.XTick = 0:20:TT; 
   %ax1.XLim = [0,202];
   %ax1.YLim = [0, 0.52];
   tickLabels = {'0', '28', '56', '84', '112', '140', '168','196','224','252','280'};
   ax4.XTickLabel = tickLabels; 
   set(gca,'TickDir','out'); % The only other option is 'in'
   ax4.TickLength = [0.005, 0.005]; % Make tick marks longer.
   ax4.LineWidth =0.9; % Make tick marks thicker.
   box off
   %%
   %XX=find(Lall(:,4)>=1/6*1e-4);
   XX=find(Lall(:,6)>=1/6*1e-4);
   %Ind1 = min(XX);
   Ind2 = max(XX)+1;
   
   %Dmin = Tall(Ind1)/28.8;
   Dmin=1;
   Dmax = Tall(Ind2)/28.8;
   
   DeltaD= Dmax-Dmin;
   