function [sheet,fname,path] = chooseFile()
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
%------------------------------------------------------------------------%
ftypes = {'*.xlsx';'*.xls';'*.csv';'*.*'};
[fname,path] = uigetfile(ftypes,'Select Stem Location File','E:\OR_Data_2015');
fullpath = fullfile(path,fname);
%path = 'E:\OR_Data_2015\Graduate-Field Season\OR_Data_2015\Crook\OH2\OH2.xlsx';
[~,sheets] = xlsfinfo(fullpath);
[sheeti,ok] = listdlg('ListString',sheets,'SelectionMode','single','Name','Select Stem Map Sheet');
if ok == 1
    sheet = char(sheets(sheeti));
else
    h = msgbox('No Sheet Selected!', 'Error', 'error');
end
%------------------------------------------------------------------------%

end

