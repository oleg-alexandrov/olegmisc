disp('Please click with the mouse where you want the text placed');
[x, y, but]=ginput(1);      % read the input from the mouse
H=text(x, y, 'some text');  % place the text
set(H, 'Fontsize', 15);     % Set the text font size