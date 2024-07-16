 function Pprime = lysogens6noS(T, P, Q)
%% parameters for the lysogen model

r = 1.0; % 0.72-1.53 % Sara: 0.5-3, baseline=1
q = [Q(1) Q(2) Q(3) Q(4) Q(5) Q(6)]; %1.27e-3 - 8.74e-2
beta = [45 45 45 45 45 45]; % 18-49 % Sara: 10-1000, baseline=100
delta = [4 4 4 4 4 4]; %Sara: 1.5-7.8 Baseline=4
mu =1; %0.01-0.1 over log(2)/35 = 0.5 - 5 % Sara: 0.9-3.6, baseline=1
%% Variables

for i=1:6
L(i)=P(i);							
IQ(i)=P(6+i);
V(i)=P(12+i);
end
%% system of ordinary equations


for i=1:6
dL(i) = r*L(i)*(1-(sum(L)+sum(IQ))) - q(i)*L(i);
dI(i) = - delta(i)*IQ(i)+q(i)*L(i);
dV(i) = beta(i)*delta(i)*IQ(i)-mu*V(i);
end

Pprime=[dL'; dI'; dV'];