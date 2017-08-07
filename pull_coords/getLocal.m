%return local coordinates for stem locations
function [treeInfo,localx,localy] = getLocal(path,sheet,range)
%read into table
treeInfo = readtable(path,'Sheet',sheet,'Range',range);
%check for proper headers
hdrs = treeInfo.Properties.VariableNames;

if strncmp('Tree', hdrs(1),4) ~= 1
    error('Start column has wrong header!')
elseif strncmp('Measure',hdrs(end),4) ~= 1
    error('End column has wrong header!')
end

workspace;
%make necessary calcs
rads = degtorad(treeInfo.Azi);
sinArray = sin(rads);
cosArray = cos(rads);
relx = sinArray.*treeInfo.Distance;
rely = cosArray.*treeInfo.Distance;

%define empty arrays for later conditional operations
cellSize = size(relx);
localx = cell(cellSize);
localy = cell(cellSize);

iter_end = length(relx);
cellList = treeInfo.Cell;

%conditional for local arrays
for i = 1:iter_end
    currentx = relx(i);
    currenty = rely(i);
    cellVal = cellList(i);
    %x coordinate conditional
    if strcmp('A1',cellVal) == 1 || strcmp('B1',cellVal) == 1
        xcoord = currentx - 10;
    else
        xcoord = currentx + 10;
    end
    %y coordinate conditional
    if strcmp('A1',cellVal) == 1 || strcmp('A2',cellVal) == 1
        ycoord = currenty + 10;
    else
        ycoord = currenty -  10;
    end    
    %fill cell arrays
    localx{i,1} = xcoord;
    localy{i,1} = ycoord;
    
end
    %convert to array
    localx = cell2mat(localx);
    localy = cell2mat(localy);
end