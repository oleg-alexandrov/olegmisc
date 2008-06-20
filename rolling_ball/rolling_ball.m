%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The matlab code starts here.

function rolling_ball()
% A matlab code to simulate a circle rolling on a curve.

% Save all of this  (including the auxiliary functions) into a file called 'rolling_ball.m'.
% To run it from matlab, type  'rolling_ball' (without the quotes).

   clear;                          % delete whatever variables exist in the memory
   set(gcf, 'color', 'white');     % make the background white
   global R Bx By N L H;           % declare some global variables

   N=1000;    % a large number, used for plotting and other things
   nf=10;     % 4*nf-2 is the number of frames (large nf makes the gif image large)
   L=5; H=2;  % the curve the circle will roll onto is f(x)=0.5*H*(cos(2*pi*x/L)+1), 0 <= x <= L
   nr=10;     % 4*nr is the total number of rotations the circle will make (going back and forth)
   R=arc_length(0, L/2)/nr; % the circle radius -- so,  make the curve length a multiple of the circumference

%  In this code we assume that the speed of the circle changes as if it is in free fall. This is
%  clearly wrong since we are on a curve, but still, it makes some illusion that the circle
%  accelerates as it goes down
   g=1; % the gravitational constant. Note: changing g does not change the ball speed.
   t0=sqrt(H/g); % 4*t0 = the total time the circle is rolling (down, up, and then in reverse)
   tt=linspace(0, t0, nf); % the i-th frame shows the position of the circle at time tt(i)

   Ty=max(H-g*tt.^2, 0); Tx=(L/2/pi)*acos(2*Ty/H-1); % (Tx(i), Ty(i)) = the point on the circle tangent to the curve
   Dy=-(pi*H/L)*sin(2*pi*Tx/L); % Dy(i) = the derivative at Tx(i)
   Cx=Tx+R*cos(pi/2+atan(Dy)); Cy=Ty+R*sin(pi/2+atan(Dy)); % (Cx(i), Cy(i)) = the center of the circle

   %extend these from [0, L/2] to [0, L]
   Tx=[Tx L-Tx(1+length(Tx)-(2:length(Tx)))];  Ty=0.5*H*(cos(2*pi*Tx/L)+1);
   Cx=[Cx L-Cx(1+length(Cx)-(2:length(Cx)))];  Cy=[Cy Cy(1+length(Cy)-(2:length(Cy)))];
 
   Fx=0:(1/N):L; Fy=0.5*H*(cos(2*pi*Fx/L)+1); % the graph of the curve (used for plotting)

   Theta=0:1/N:2*pi; % a mesh on the interval [0, 2pi]
   Bx=R*cos(Theta); By=R*sin(Theta); % the ball to be plotted

   cf=0; %current frame
   for i=1:(2*nf-1)
      %prepare the screen
      clf; hold on; axis equal; axis off; axis([-1.1*R L+1.1*R -0.1*R 2.1*R+H]);

      theta=arc_length(Tx(1), Tx(i))/R; %the amount of rotation for the circle, depends on the length traveled so far
      plot(Fx,Fy,'LineWidth', 2, 'color', [0 0 0]) %plot the curve
      rot_angle(Cx(i), Cy(i), theta);  %plot the ball
      pause(0.01)  %take a break

      cf=cf+1;
      saveas(gcf, sprintf('frame%d', 1000+cf), 'png') %save the current frame.
      %Note: Saving the frames as postcript instead of jpg produces a better quality of the final .gif picture.
      disp(sprintf('frame%d', 1000+cf)); %show the frame number we are at
   end

   %same thing as above, but in reverse
   for i=(2*nf-1):(-1):1
      %prepare the screen
      clf; hold on; axis equal; axis off; axis([-1.1*R L+1.1*R -0.1*R 2.1*R+H]);

      theta=arc_length(Tx(1), Tx(i))/R; %the amount of rotation for the circle, depends on the length traveled so far
      plot(Fx,Fy,'LineWidth', 2, 'color', [0 0 0]) %plot the curve
      rot_angle(Cx(i), Cy(i), theta);  %plot the ball
      pause(0.01)  %take a break

      cf=cf+1;
      saveas(gcf, sprintf('frame%d', 1000+cf), 'png') %save the current frame
      disp(sprintf('frame%d', 1000+cf)); %show the frame number we are at
   end

%  end of the main program.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%  an auxiliary function to plot the ball.
%  Inputs: the coordinates of the center and angle of rotation
function rot_angle(a, b, theta)
   global R Bx By;

   %the triangle vertices
   x1=R*cos(theta);       y1=-R*sin(theta);
   x2=(-x1-y1*sqrt(3))/2; y2=(-y1+x1*sqrt(3))/2;
   x3=(-x2-y2*sqrt(3))/2; y3=(-y2+x2*sqrt(3))/2;

   %shift to put the center at (a, b)
   x1=x1+a; x2=x2+a; x3=x3+a;
   y1=y1+b; y2=y2+b; y3=y3+b;

   %graph the triangle
   plot([x1 x2], [y1, y2], 'LineWidth', 2, 'color', [0 0 0]);
   plot([x2 x3], [y2, y3], 'LineWidth', 2, 'color', [0 0 0]);
   plot([x3 x1], [y3, y1], 'LineWidth', 2, 'color', [0 0 0]);

%  graph the circle
   plot(Bx+a, By+b, 'LineWidth', 2, 'color', [0 0 0]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  an auxiliary function to find the length of the graph of f(x) for x1 <= x <= x2
function l=arc_length(x1, x2)
   global N L H;

   M=floor(N*(x2-x1)); % the number of intervals of size 1/N in the interval [x1, x2]

   l=0;
   for i=1:M
      x=x1+(i-1)/N;
      s=-(pi*H/L)*sin(2*pi*x/L); %the derivative of the curve
      l=l+sqrt(1+s^2);
   end
   l=l/N;

% End of the matlab code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%