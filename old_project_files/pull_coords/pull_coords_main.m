%Extract_Coords
%Use to extract geographic coords for trees from OR data collected year
%2015
%Author: Jamie Perkins
%jp2081@wildcats.unh.edu

%clear out workspace
%------------------------------------------------------------------------%
close all
clear variables
clc
%------------------------------------------------------------------------%

%pull list of raw data files
%choose 
%------------------------------------------------------------------------%
dirPath = uigetdir('F:\Box Sync\OR_Data','Select Raw Data Directory');
flist = getflist(dirPath);
iterable = length(flist);
%------------------------------------------------------------------------%

%Spreadsheet Params
%[sheet,fname,path] = chooseFile();


%iterate over all files
for i = 1:iterable
    
    fullpath = char(flist{i,1});
    [path,fname,ext] = fileparts(fullpath);
    fname = strcat(fname,ext);
    if strncmpi(fname,'~$',2)
        continue
    end
    sheet = 'Stem Map';

    %GPS Data Params
    %------------------------------------------------------------------------%
    %GPSPath = 'E:\OR_Perkins\OR_2015_Extra\OR_Data_2015\OR_Coords_AllPlots.xlsx';
    GPSPath = 'F:\Box Sync\OR_Data_Extra\OR_Data_2015\OR_Coords_AllPlots.xlsx';
    %plotCode = 'OH2';
    exti = strfind(fname,'.');
    if exti ~= 0
        fnamesplit = strsplit(fname,'_raw.');
        plotCode = char(fnamesplit(1));
    else
        h = msgbox('No Plot Code!', 'Error', 'error');
    end
    pointRef = 'CENTER';
    %------------------------------------------------------------------------%

    %initiate code
    %------------------------------------------------------------------------%
    cellStr = findRange(fullpath,sheet);
    [treeInfo,localx,localy] = getLocal(fullpath,sheet,cellStr);
    [GPStable,lat,long,alt] = pullGPS(GPSPath,plotCode,pointRef);
    %------------------------------------------------------------------------%

    %use coords to find declination
    %------------------------------------------------------------------------%
    if ~isnan(alt)
        [~,~,offset,~,~] = wrldmagm(alt,lat,long,decyear(2015,7,15),'2015');
        %use declination to adjust local coordinates
        [adjX,adjY] = magAdj(localx,localy,treeInfo,offset);
        %------------------------------------------------------------------------%

        degX = km2deg(adjX./1000)+long;
        degY = km2deg(adjY./1000)+lat;

        %plot(degX,degY,'o')
        %plot(adjX,adjY,'o')

        %output coordinates to new csv file
        %------------------------------------------------------------------------%
        vnames = {'TreeID','Cell','Species','DBH','Long','Lat','X','Y'};
        outTable = table(treeInfo.Tree_,treeInfo.Cell,treeInfo.Species,treeInfo.DBH_cm_,degX,degY,localx,localy,'VariableNames',vnames);
        outName = strcat(plotCode,'_Geo.csv');
        oPath = 'E:\OR_Perkins\OR_PlotData_2015\geo_files';
        outPath = fullfile(oPath,outName);
        writetable(outTable,outPath);
        outConsole = strcat(plotCode,' Complete');
        disp(outConsole)
    else
        continue
    end
    %------------------------------------------------------------------------%
end