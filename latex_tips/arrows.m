arrowsize=6;     % how big to make the arrows
sharpness=pi/12; % how sharp to make the arrows
thickness=1.5;   % how thick to make the lines


figure(1); clf; hold on; axis equal; axis off;           % prepare the plotting window

arrow_type=0;
arrow([0 0], [2 0], thickness, arrowsize, sharpness, arrow_type);

arrow_type=1;
arrow([0 0], sqrt(2)*[1 1], thickness, arrowsize, sharpness, arrow_type);

arrow_type=2;
arrow([0 0], [0 2], thickness, arrowsize, sharpness, arrow_type);

arrow_type=3;
arrow([0 0], sqrt(2)*[-1 1], thickness, arrowsize, sharpness, arrow_type);

saveas(gcf, 'arrows.eps'); % save to a postscript file