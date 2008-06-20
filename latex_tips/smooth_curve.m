% prepare the screen
figure(1); clf; hold on; axis equal; axis([0 1 0 1]); 

disp('Click with the left mouse button to select points.')
disp('Right mouse button selects the last point.');

but=1; % but==1 for the left mouse button, ==2 for the middle one, etc
i=1;   % a counter
x=[]; y=[]; % store the points inputed by the user

% record the points
while but==1
   [x(i), y(i), but]=ginput(1); % ginput() = graphical input from mouse
   plot(x(i), y(i), 'r*')
   disp(sprintf('Point: %d', i))
   i=i+1;
end

n=length(x); 
P=5; Q=n+2*P+1; % P will denote the amount of overlap

% Make the 'periodic' sequence xp=[x(1) x(2) x(3) ... x(n) x(1) x(2) x(3) ... ]
% of length Q. Same for yp.
for i=1:Q
   j=rem(i, n)+1; % rem() is the remainder of division of i by n
   xp(i)=x(j);
   yp(i)=y(j);
end

% do the spline interpolation
t=1:length(xp);
N=100; % how fine to make the interpolation
tt=1:(1/N):length(xp);
xx=spline(t, xp, tt);
yy=spline(t, yp, tt);

% discard the reduntant pieces
start=N*(P-1)+1;
stop=N*(n+P-1)+1;
xx=xx(start:stop); 
yy=yy(start:stop);

% plot, and save to file
linewidth=1.0;
figure(1); plot(xx, yy, 'b', 'LineWidth', linewidth)
figure(2); clf; hold on; axis equal; axis off 
plot(xx, yy, 'b', 'LineWidth', linewidth)
saveas(gcf, 'spline.eps', 'psc2') 
disp('Done! The curve is saved as "spline.eps".')
