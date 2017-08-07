function [ flist ] = getflist( dirPath )
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here
%Generate File List
%------------------------------------------------------------------------%

%dirPath = fullfile('E:\OR_Perkins\OR_PlotData_2015\*\*\*_raw.*');
dirInfo = subdir(dirPath);
dirLen = length(dirInfo);

flist = cell(1,1);
fcount = 1;
for i = 1:dirLen
    
    fn = char(dirInfo(i).name);
    fend = '_raw.';
    if sum(strfind(fn,fend)) > 0
        flist{fcount,1} = fn;
        fcount = fcount + 1;
    end
end
%------------------------------------------------------------------------%

end

