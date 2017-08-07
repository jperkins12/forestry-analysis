%inputs the excel file and the worksheet, outputs string with cell range
function range = findRange(path,sheet)
%find size of first column
rawData = readtable(path,'ReadVariableNames',false,'Sheet',sheet);
firstCol = rawData.Var1;
%notChars = ~ischar(firstCol);
%notNums = ~isinteger(firstCol);
%toDelete = notChars & notNums;
toDelete = strcmp(firstCol,'');
firstCol(toDelete,:) = [];
%start and endIndex are row values for table import
[endIndex,~] = size(firstCol);
start = find(strncmp('Tree',firstCol,4),1);

cellVals = 'A%d:I%d';
%format row values into string
range = sprintf(cellVals,start,endIndex);

%close out data
clear rawData
clear firstCol
end