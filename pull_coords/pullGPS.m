function [plotCords,lat,long,alt] = pullGPS(GPSPath,plotCode,pointRef)
%pulls proper GPS coords from excel table
GPSData = readtable(GPSPath);

disp(plotCode)
plotRows = strncmp(plotCode,GPSData.Plot,3);
plotCords = GPSData(plotRows,:);

disp(plotCords)

rows = strncmp(pointRef,plotCords.Point,6);
centerCords = plotCords(rows,:);

disp(rows)
disp(centerCords)

%pullVars
lat = centerCords.(13);
long = centerCords.(14);
alt = centerCords.(12);
disp([lat,long,alt])

end

