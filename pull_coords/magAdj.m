function [adjX,adjY] = magAdj(localx,localy,treeInfo,offset)
%adjust for declination
%------------------------------------------------------------------------%
centerDist = sqrt((localx.^2)+(localy.^2));
%generate array of x/y
tanVals = (localy./localx);
%angle values are modified differenty if Y/X is positive or negative
posVals = (tanVals)>0;
negVals = (tanVals)<0;
degmod = posVals.*deg2rad(90);
abstanVals = abs(tanVals);

centerAzi1 = atan(abstanVals.*negVals);
centerAzi2 = posVals.*(degmod-atan(abstanVals));
centerAzi = centerAzi1 + centerAzi2;

%create arrays containing cell values
A1stems = strncmp('A1',treeInfo.Cell,2);
A2stems = strncmp('A2',treeInfo.Cell,2);
B1stems = strncmp('B1',treeInfo.Cell,2);
B2stems = strncmp('B2',treeInfo.Cell,2);

A1Vals = (centerAzi+deg2rad(270)).*A1stems;
A2Vals = centerAzi.*A2stems;
B1Vals = (centerAzi+deg2rad(180)).*B1stems;
B2Vals = (centerAzi+deg2rad(90)).*B2stems;

%array of declination adjusted azi angles
adjAzi = A1Vals + A2Vals + B1Vals + B2Vals + deg2rad(offset);

sinVals = sin(adjAzi);
cosVals = cos(adjAzi);

adjX = sinVals.*centerDist;
adjY = cosVals.*centerDist;
%------------------------------------------------------------------------%
end