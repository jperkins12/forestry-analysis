%-------------------------------------------------------------------------%
%Stand_spatial_analysis_main
%
%Use this script to compute spatial distribution indicies of
%stems in a stand, given (X,Y) stem coordinates
%
%Author: Jamie Perkins
%Email: jp2081@wildcats.unh.edu
%-------------------------------------------------------------------------%

%clear out workspace
%------------------------------------------------------------------------%
close all
clear variables
clc
%------------------------------------------------------------------------%

%Select Stem Locations File
%pull list of data files
%------------------------------------------------------------------------%
currentFolder = pwd;
dirPath = 'E:\OR_Perkins\OR_PlotData_2015\geo_files\';
cd(dirPath)
flist = dir('*Geo*');
fileIter = length(flist);
cd(currentFolder)
%------------------------------------------------------------------------%
%initialise results
results = 0;

%Iterate through files in directory
for q = 1:fileIter
    
    pathName = flist(q).name;
    disp(pathName)
    fullpath = fullfile(dirPath,pathName);
    %------------------------------------------------------------------------%
    %ftypes = {'*.csv';'*.xlsx';'*.xls';'*.*'};
    %[fname,path] = uigetfile(ftypes,'Select Stem Location File','E:\OR_Data_2015');
    %fullpath = fullfile(path,fname);
    %fullpath = 'E:\OR_Perkins\OR_PlotData_2015\geo_files\OH2_Geo.csv';
    [path,fname,ext] = fileparts(fullpath);
    pcode = strsplit(fname,'_');

    %pull info on stems to table
    stemInfo = readtable(fullpath);

    %#of iterations is number of stems
    iterend = length(stemInfo.X);

    %store spatial data
    spatialMat = zeros(iterend,3);
    
    %store distances
    distList = zeros(iterend,1);

    %pull mean dbh off the bat
    meanDBH = mean(stemInfo.DBH,'omitnan');
    
    qmd = sqrt(sum(stemInfo.DBH.^2,'omitnan')/iterend);
    
    %setup species counts
    speciesCount = {'abicon',0;'abigra',0;'junocc',0;'larocc',0;'piceng',0;'pincon',0;'pinpon',0;'psemen',0};

    %for each tree in plot
    for i = 1:iterend

        %pull reference tree vals
        refx = stemInfo.X(i);
        refy = stemInfo.Y(i);
        refSpc = char(stemInfo.Species(i));
        if ~ischar(refSpc)
            refSpc = 'SPCNON';
        end
        %add to species count
        addSpc = strcmpi(refSpc,speciesCount(:,1));
        spc2add = cell2mat(speciesCount(:,2)) + addSpc;
        speciesCount(:,2) = num2cell(spc2add);

        refDBH = stemInfo.DBH(i);

        count = 0;
        %cell holds distance and angle values
        wmat = zeros(iterend-1,6);
        
        %compare reference tree to other stems
        for j = 1:iterend

            count = count + 1;

            %pull coords for surrounding stems
            x = stemInfo.X(j);
            y = stemInfo.Y(j);

            %pull DBH and calculate DBH differentiation index
            dbh = stemInfo.DBH(j);
            dbhA = [refDBH, dbh];
            dbhT = 1-(min(dbhA)/max(dbhA));

            %check if species is different
            spc = char(stemInfo.Species(j));
            if length(spc)<1
                spc = 'NONSPC';
            end
            if strcmp(spc,refSpc) == 1
                spcVal = 0;
            else
                spcVal = 1;
            end

            %make sure distance is not 0
            %pass over same stem
            if refx == x && refy == y
                count = count - 1;
                continue
            end

            %xy differences to find angle
            xlen = x-refx;
            ylen = y-refy;
            ang = atan2d(ylen,xlen);
            %correct negative values
            if ang < 0
                ang = ang + 360;
            end

            %use distance function to find closest stems
            points = [refx,refy;x,y];
            dist = pdist(points);

            %add values to new cell
            wmat(count,1) = dist;
            wmat(count,2) = ang;
            wmat(count,3) = x;
            wmat(count,4) = y;
            wmat(count,5) = spcVal;
            wmat(count,6) = dbhT;

        end

        sorted = sortrows(wmat);
        %need to sort based on azi angles
        wvals = sortrows(sorted(1:4,2));
        %sort closest three neighbors for mingling
        mvals = sorted(1:3,5);
        %sort closest 3 for dbh differentiation
        tvals = sorted(1:3,6);
        
        %store closest neighbor
        closest = sorted(1,1);
        distList(i,1) = closest;
        
        %new cell to hold angle values
        angles = zeros(4,2);

        for k = 1:4

            t = k + 1;
            %ensure index is within cell bounds
            if t == 5
                t = 1;
            end

            %angle is difference between azi's
            acir = wvals(k,1);
            bcir = wvals(t,1);
            angle = abs(bcir-acir);

            %add to angle cell
            if k == 4
                angle = 360 - angle;
            end

            angles(k,1) = angle;

            %add W value
            if angle > 90
                w = 0;
            else
                w = 1;
            end

            angles(k,2) = w;

        end

        %calculate spatial indices value
        %contagion = wtot
        %species minging = ming
        %DBH differentiation = diff
        %Total #stems = stemTot
        %National Forest Land = publicLand (T/F)
        %#Each species
        wtot = mean(angles(:,2));
        ming = mean(mvals);
        diff = mean(tvals);

        %assign to table
        spatialMat(i,1) = wtot;
        spatialMat(i,2) = ming;
        spatialMat(i,3) = diff;

    end

    stemTot = iterend;
    %get each species
    %iterate through all species
    numSpc = size(speciesCount);
    spcCount = numSpc(1,1);
    numCell = cell(2,(spcCount*2+3));
    %cell val index
    sindex = 0;
    
    %species richness
    rich = 0;
    
    for si = 1:spcCount
        
        sindex = sindex + 1;
        
        %pull species name string and count
        sname = char(speciesCount(si,1));
        rname = strcat('rel_',sname);
        scount = speciesCount{si,2};
        rcount = scount/stemTot;
        
        if strcmp(sname,'abigra') == 1
            abigrai = sindex;
        elseif strcmp(sname,'abicon') == 1
            abiconi = sindex;
        end
        
        %add to species richness
        
        if scount ~= 0
            rich = rich + 1;
        end
        
        %place values in cell array
        numCell{1,sindex} = sname;
        numCell{2,sindex} = scount;
        sindex = sindex + 1;
        numCell{1,sindex} = rname;
        numCell{2,sindex} = rcount;
    end
    
    firs = numCell{2,abiconi} + numCell{2,abigrai};
    numCell{1,(end-2)} = 'firs';
    numCell{2,(end-2)} = firs;
    numCell{1,(end-1)} = 'relfirs';
    numCell{2,(end-1)} = firs/stemTot;
    
    %add totals
    numCell{1,end} = 'totalStems';
    numCell{2,end} = stemTot;
    
    %add mean DBH
    numCell{1,(end+1)} = 'meanDBH';
    numCell{2,(end)} = meanDBH;
    
    %add mean distance
    numCell{1,(end+1)} = 'meanDist';
    numCell{2,end} = mean(distList,'omitnan');
    
    %add richness
    numCell{1,(end+1)} = 'speciesRich';
    numCell{2,end} = rich;
    
    %add richness
    numCell{1,(end+1)} = 'qmd';
    numCell{2,end} = qmd;
    
    %create tree count table
    treeNames = numCell(1,:);
    treeVals = numCell(2,:);
    treeCell = cell2table(treeVals,'VariableNames',treeNames);
    
    varNames = {'Contagion', 'Mingling', 'DBH_Diff'};
    outTable = array2table(spatialMat, 'VariableNames', varNames);
    func = @mean;
    plotResults = varfun(func,outTable);
    
    plotTable = table(pcode(1,1),'VariableNames',{'plot'});
    plotOut = [plotTable plotResults treeCell];
    %check for results table
    if isnumeric(results)
        results = plotOut;
    else
        results = [results; plotOut];
    end
    
end

%add Ntl Forest Layer
Ntls = {'MH','OH','SM','UT'};
nmat = zeros(32,1);

for y = 1:4;
    nplot = Ntls{1,y};
    nforest = strncmpi(nplot,results.plot,2);
    nmat = nmat + nforest;
end

%add public land layer
pubTable = table(nmat,'VariableNames',{'PublicLand'});
results = [results pubTable];

csvOutPath = 'E:\OR_Perkins\OR_2015_Extra\PCORD_Analysis\spatial_data.xlsx';
writetable(results,csvOutPath)
disp('xlsx Exported')
disp('Done')