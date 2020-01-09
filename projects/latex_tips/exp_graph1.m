function exp_graph () % draw a nice graph of an exponential function

   fontsize=20;    % the font size for the x and y labels
   arrowsize=6;    % how big to make the arrows
   thickness=1.5;  % how thick to make the lines and curves
   small=0.1;      % a small number used for text placement
%  the parameters above need tweaking to obtain a good looking picture
  
   x1=-2; x2=2; y1=-2; y2=2;          % the viewing window is [x1, x2] x [y1, y2]
   x=x1:0.01:x2;                      % a mesh between x1 and x2 with spacing 0.01
   c=1; y=exp(x)-c;                   % the function to be graphed is y=e^x-c

   figure(1); clf; hold on; axis equal; axis off;           % prepare the plotting window

   arrow([x1, 0], [x2, 0], thickness, arrowsize)            % draw the x-axis and its arrow
   H=text(x2, -2*small, sprintf('x'));                      % label the x-axis
   set(H, 'FontSize', fontsize);                            % the label font size
  
   arrow([0, y1], [0, y2], thickness, arrowsize)            % draw the y-axis and its arrow
   H=text(-3*small, y2, sprintf('y'));                      % label the y-axis
   set(H, 'FontSize', fontsize);                            % the label font size

   plot(x, y, 'LineWidth', thickness);                      % draw the graph
   plot([x1, x2], [-c, -c], '--', 'LineWidth', thickness/2) % draw the asymptote
   H=text(-3*small, -c-2*small, sprintf('%2.0f', -c));      % label the asymptote
   set(H, 'FontSize', fontsize);                            % the label font size

   H=text(x2, y2, sprintf('y=e^x-%2.0f', c));               % label the graph
   set(H, 'FontSize', fontsize);                            % the label font size

   small2=0.02; % another small number. Extends the viewing window slightly, so that
   axis([x1 x2+small2 y1 y2+small2]); % arrows don't get blunted. Now set the viewing window
  
   saveas(gcf, 'exp_graph1.eps') % save the resulting picture as an eps file
                                 % also can save as jpg or bmp