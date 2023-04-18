% castleton - added if statement at line 610 because values for divewinday and/or alldives2 were being calculated incorrectly
% for tag 1008130 on day 9/13/08, i couldn't track down the calculation error so the if statement skips an assignment when the
% values are incorrect, according to wilson this isn't an issue because it has to do with auto calculation of ILD which he isn't
% using

quickrun = 0; % skips steps just before processing of definition #3.

%%Analyze dives
if(quickrun == 0)
clear all;close all;
quickrun = 0;
end

%%%Set some parameters
tint=4;         %%water column profiles and isothermal layer depth estimates were generated every 24/tint hours
plotchk = 1;    %%1 will plot up the dives for every nthplot days, 0 won't do any plots
nthplot = 1;
constz_start=70;      %%For dive definition #3, dives begin when the fish goes deeper than a depth of constz.
constz_stop=50;  
outfilename = 'test_output';     %output file name
midnight_terminate = 1; % terminate all dive searches at midnight
midnight_offset = 0;  % adjust +/- number of hours for midnight
daily_window = 1; % filter dives in a daily start and end time
dw_start = 0; % 12am
dw_stop = 24; % 12pm

%Go to directory of interest
%cd('C:\MATLAB\code\Dive_Analysis\')
cd('E:\processing\projects\dive_analysis\input_data')
%cd('E:\processing\projects\dive_analysis\input_data\test')
%cd('/Users/polkeem/TOPP/Dive_Analysis')

if(quickrun== 0)
% load data
% load('1007019MA0107A0202_input_data_for_dive_analysis.mat')
% load('1007023PA0304P0549_input_data_for_dive_analysis.mat')
%load('1008130MA0108A0579_input_data_for_dive_analysis.mat')
load('1008130MA0108A0579_input_data_for_dive_analysis_sub60.mat')
%load('1008100LO07D1164_input_data_for_dive_analysis.mat')
%load('5108021MA0108A0652_input_data_for_dive_analysis.mat')

Te = Tb-Ta; %Thermal excess

sampint = Date(2)-Date(1);    %sampling interval in decimal days
obs_per_day = round(1/sampint);
blocklines = ceil(obs_per_day/tint);
NDays = fix(Date(end)) - fix(Date(1)) +1;

%Find local minima and maxima (i.e., inflection points)
[Max, Min] = max_min_G(D);

divemax=Max.i;
divemin=Min.i;

%##########################################################################
%##########################################################################
%%Find dives under definition #2 (excursions below and returns above the
%%iso-thermal layer depth, or ILD)
tcline_ind = [(0:1:size(tcline,1)-1)'*blocklines+1,(1:1:size(tcline,1))'*blocklines];   %Gives the start and end points in the time-series data corresponding to each ILD estimate
tcline_ind(end,2) = size(Date,1);
%%Find the start of the first dive. Needs to be at least one pt above the
%%ILD before the start.
curr = 1;
while curr <= size(tcline,1)
    above = find(D(tcline_ind(curr,1):tcline_ind(curr,2)) <= tcline(curr,1));
    if ~isempty(above)
        below = find(D(tcline_ind(curr,1):tcline_ind(curr,2)) > tcline(curr,1));
        below(below<above(1)) = [];
    else
        below = [];
    end;

    if ~isempty(below)
        divest = below(1) + tcline_ind(curr,1) -1;  %this is the first pt below the MLD that comes after at least one point above the MLD
        break;
    else
        curr = curr + 1;    %curr keeps track of the row in tcline relative to which the current dive was initiated
    end;
end;
curow = 1;
longdives2 = [];
cont = 1;

%Starting with beginning of first crossing below ILD, identify dives
if ~isempty(divest)
    while ~isempty(divest)%divest <= length(D)
        alldives2(curow,1)=divest-1;
        alldives2(curow,7)=tcline(curr,1);
        %%Find first point back above ILD, ie dive end (using ILD from dive start)
        divend = [];
        curr2=curr; %curr is the ILD interval at the dive start, curr2 is the interval under consideration for divend.
        while curr2 <= size(tcline,1)
            above = find(D(tcline_ind(curr2,1):tcline_ind(curr2,2)) <= tcline(curr,1));
            above(above <=divest-tcline_ind(curr2,1)+1)=[];
            if ~isempty(above)
                divend = above(1) + tcline_ind(curr2,1) - 1;
                alldives2(curow,2) = divend;
                if (divend-divest)*sampint > 1
                    longdives2=[longdives2;curow];    %flag if the dive lasts longer than one day
                    disp(['Dive ' num2str(curow) ' longer than 1 day']);
                end;
                curow = curow + 1;
                break;
            else
                curr2 = curr2 + 1;    %no dive end in this tcline interval, move on to next.
            end;
        end;

        if isempty(divend)
            alldives2(curow,:) = [];
            break;
        end;

        %%Now find start of next dive, if any
        divest=[];
        tt = find(tcline_ind(:,1) < divend);
        curr = tt(end);  %curr keeps track of the row in tcline relative to which the current dive was initiated
        strow = divend;
%         disp('curr')
%         curr
%         disp('tcline_ind(curr,1)')
%         tcline_ind(curr,1)
%         disp('tcline_ind(curr,2)')
%         tcline_ind(curr,2)
%         disp('tcline(curr,1)')
%         tcline(curr,1)
%         disp('size(find(D))')
%         size(find(D(tcline_ind(curr,1):tcline_ind(curr,2))))
%         disp('size(find(D(tcline_ind(curr,1):tcline_ind(curr,2)) > 0.0))')
%         size(find(D(tcline_ind(curr,1):tcline_ind(curr,2)) > 0.0))
%         disp('size(find(D)< tcline...)')
%         size(find(D(tcline_ind(curr,1):tcline_ind(curr,2)) <= tcline(curr,1)))
%         disp('size(find(D)< tcline & >0)')
%         size(find(D(tcline_ind(curr,1):tcline_ind(curr,2)) <= tcline(curr,1) & D(tcline_ind(curr,1):tcline_ind(curr,2)) > 0.0))
        while curr <= size(tcline,1)
            above = find(D(tcline_ind(curr,1):tcline_ind(curr,2)) <= tcline(curr,1));
            %above = find(D(tcline_ind(curr,1):tcline_ind(curr,2)) <= tcline(curr,1) & D(tcline_ind(curr,1):tcline_ind(curr,2)) > 0.0) % castleton
            if ~isempty(above)
                below = find(D(tcline_ind(curr,1):tcline_ind(curr,2)) > tcline(curr,1));
                below(below<above(1)) = [];
                below(below <= divend-tcline_ind(curr,1)+1)=[];
            else
                below = [];
            end;
            if ~isempty(below)
                divest = below(1) + tcline_ind(curr,1) -1;  %this is the first pt below the MLD that comes after at least one point above the MLD
                break;
            else
                curr = curr + 1;    %No dive st in this tcline row, move on to next interval
            end;
        end;
    end;
end;

%%Find first min before and after each dive, and first/last max within each
divemaxT=divemax;diveminT=divemin;
for div = 1:size(alldives2,1)
    frstmax = find(divemax>alldives2(div,1));frstmax=frstmax(1);
    lstmax = find(divemax<alldives2(div,2));lstmax=lstmax(end);
    alldives2(div,3) = divemax(frstmax);
    alldives2(div,4) = divemax(lstmax);

    frstmin = find(divemin<=alldives2(div,1));
    if ~isempty(frstmin)
        frstmin=frstmin(end);
        alldives2(div,5) = divemin(frstmin);
    else
        alldives2(div,5) = alldives2(div,1);%Not sure this line is necessary
    end;

    lstmin = find(divemin>=alldives2(div,2));
    if ~isempty(lstmin)
        lstmin=lstmin(1);
        alldives2(div,6) = divemin(lstmin);
    else
        alldives2(div,6) = alldives2(div,2);
    end;

    divemax(1:lstmax)=[];
    divemin(1:frstmin)=[];

end;
divemax=divemaxT;divemin=diveminT;clear divemaxT diveminT;

end

%##########################################################################
%##########################################################################
%%Find dives under definition #3 (descents below and then
%%returns above some constant depth specified by constz)

above_below_const_start = D>constz_start;
above_below_const_stop = D>constz_stop;
sten_const_start = diff(above_below_const_start);
sten_const_stop = diff(above_below_const_stop);
divest3_tmp=find(sten_const_start==1);
divend3_tmp=find(sten_const_stop==-1)+1;
divest3 = [];
divend3 = [];
divest3_pre = [];
divend3_pre = [];
i=1;j=1;loopvar=1;
ilim = length(divest3_tmp);
jlim = length(divend3_tmp);
divestate = 1; % 0=not diving; 1=diving
while ((i<=ilim)&&(j<=jlim))
    if(divestate) % dive started...search of end
        if(fix(Date(divend3_tmp(j))+midnight_offset/24)...
                < fix(Date(divest3_tmp(i))+midnight_offset/24))
            % end day is earlier...adjust day
            j=j+1;
        else
            if(midnight_terminate &&...
                (fix(Date(divend3_tmp(j))+midnight_offset/24)...
                    > fix(Date(divest3_tmp(i))+midnight_offset/24)))
                % end day is later...terminate dive
                divestate = 0;
                i=i+1;
            else % dive end is on the same day
                if(divend3_tmp(j) > divest3_tmp(i)) % corresponding dive end found
                    divestate = 0; % end dive and latch values
                    divest3_pre = [divest3_pre; divest3_tmp(i)];
                    divend3_pre = [divend3_pre; divend3_tmp(j)];
                    i=i+1;
                else
                    j=j+1;
                end
            end
        end
    else % not diving...search for beginning of next dive
        if(divest3_tmp(i) >= divend3_tmp(j))
            divestate = 1; % begin dive
        else
            i=i+1; 
        end
    end
end

% if divend3(1)<=divest3(1)
%     divend3(1)=[];
% end;
% if divend3(end)<=divest3(end)
%     divest3(end)=[];
% end;

% This window filter dives that fall out of a daily window specified
% by a start and end time

dw_dayst = 0;
dw_dayend = 0;
dw_divest = 0;
dw_divend = 0;
for i=1:length(divest3_pre)
    if(daily_window)
        dw_dayst = fix(Date(divest3_pre(i))+midnight_offset/24);
        dw_dayend = fix(Date(divend3_pre(i))+midnight_offset/24);
        if(dw_dayst ~= dw_dayend) % start and end not on same day
            % filter out
        else
            dw_divest = Date(divest3_pre(i))+midnight_offset/24-dw_dayst;
            dw_divend = Date(divend3_pre(i))+midnight_offset/24-dw_dayend;
            if((dw_divest >= dw_start/24) && (dw_divend <= dw_stop/24))
                divest3 = [divest3; divest3_pre(i)];
                divend3 = [divend3; divend3_pre(i)];
            end
        end
    else
        divest3 = [divest3; divest3_pre(i)];
        divend3 = [divend3; divend3_pre(i)];
    end
end

alldives3 = [divest3,divend3];
for div = 1:length(divest3)
    frstmax = find(divemax>divest3(div));frstmax=frstmax(1);
    lstmax = find(divemax<divend3(div));lstmax=lstmax(end);
    alldives3(div,3) = divemax(frstmax);
    alldives3(div,4) = divemax(lstmax);

    frstmin = find(divemin<=divest3(div));
    if ~isempty(frstmin)
        frstmin=frstmin(end);
        alldives3(div,5) = divemin(frstmin);
    else
        alldives3(div,5) = alldives3(div,1);
    end;

    lstmin = find(divemin>=divend3(div));
    if ~isempty(lstmin)
        lstmin=lstmin(1);
        alldives3(div,6) = divemin(lstmin);
    else
        alldives3(div,6) = alldives3(div,2);
    end

end;


%##########################################################################
%##########################################################################
%%%%%Having identified the dives, now extract some dive metrics and do some
%%%%%plots

%Keep track of times of dive start/end, and dive duration
divetimes2 = [Date(alldives2(:,1)),Date(alldives2(:,2)),Date(alldives2(:,2))-Date(alldives2(:,1))];
divetimes3 = [Date(alldives3(:,1)),Date(alldives3(:,2)),Date(alldives3(:,2))-Date(alldives3(:,1))];

%Extract inter-dive interval
divetimes2(1:end-1,4) = Date(alldives2(2:end,1))-Date(alldives2(1:end-1,2));
divetimes2(end,4) = NaN;
divetimes3(1:end-1,4) = Date(alldives3(2:end,1))-Date(alldives3(1:end-1,2));
divetimes3(end,4) = NaN;


%%Dive stats for definition 2
for dii = 1:size(alldives2,1)
    %Extract mean, max, and variance of within-dive depths
    divedepths2(dii,1) = mean(D(alldives2(dii,1):alldives2(dii,2)));
    divedepths2(dii,2) = max(D(alldives2(dii,1):alldives2(dii,2)));
    divedepths2(dii,3) = var(D(alldives2(dii,1):alldives2(dii,2)));
    %Extract mean and variance of depths between the first and last maxima
    divedepths2(dii,4) = mean(D(alldives2(dii,3):alldives2(dii,4)));
    divedepths2(dii,5) = var(D(alldives2(dii,3):alldives2(dii,4)));
    divedepths2(dii,6) = D(alldives2(dii,1));   %depth at start of dive
    divedepths2(dii,7) = D(alldives2(dii,2));   %depth at end of dive
    divedepths2(dii,8) = divedepths2(dii,1) - alldives2(dii,7); %distance by which mean dive depth exceeds ILD

    %Extract mean, max, and variance of within-dive ambient temps
    divetemps2(dii,1) = mean(Ta(alldives2(dii,1):alldives2(dii,2)));
    divetemps2(dii,2) = min(Ta(alldives2(dii,1):alldives2(dii,2)));
    divetemps2(dii,3) = var(Ta(alldives2(dii,1):alldives2(dii,2)));
    %Extract mean and variance between the first and last maxima
    divetemps2(dii,4) = mean(Ta(alldives2(dii,3):alldives2(dii,4)));
    divetemps2(dii,5) = var(Ta(alldives2(dii,3):alldives2(dii,4)));

    %Extract mean, max, and variance of within-dive body temps
    divetemps2(dii,6) = mean(Tb(alldives2(dii,1):alldives2(dii,2)));
    divetemps2(dii,7) = min(Tb(alldives2(dii,1):alldives2(dii,2)));
    divetemps2(dii,8) = var(Tb(alldives2(dii,1):alldives2(dii,2)));
    %Extract mean and variance between the first and last maxima
    divetemps2(dii,9) = mean(Tb(alldives2(dii,3):alldives2(dii,4)));
    divetemps2(dii,10) = var(Tb(alldives2(dii,3):alldives2(dii,4)));

    %Extract mean, max, and variance of within-dive thermal excesses
    divetemps2(dii,11) = mean(Te(alldives2(dii,1):alldives2(dii,2)));
    divetemps2(dii,12) = min(Te(alldives2(dii,1):alldives2(dii,2)));
    divetemps2(dii,13) = var(Te(alldives2(dii,1):alldives2(dii,2)));
    %Extract mean and variance between the first and last maxima
    divetemps2(dii,14) = mean(Te(alldives2(dii,3):alldives2(dii,4)));
    divetemps2(dii,15) = var(Te(alldives2(dii,3):alldives2(dii,4)));

    %Extract mean, max, and variance of within-dive light levels
    divelight2(dii,1) = mean(hv(alldives2(dii,1):alldives2(dii,2)));
    divelight2(dii,2) = min(hv(alldives2(dii,1):alldives2(dii,2)));
    divelight2(dii,3) = var(hv(alldives2(dii,1):alldives2(dii,2)));
    %Extract mean and variance between the first and last maxima
    divelight2(dii,4) = mean(hv(alldives2(dii,3):alldives2(dii,4)));
    divelight2(dii,5) = var(hv(alldives2(dii,3):alldives2(dii,4)));


    %Calculate mean, max, min, variance of descent/ascent rates in m/s
    desc_rates2(dii,1) = mean(diff(D(alldives2(dii,1):alldives2(dii,3)))/(sampint*24*60*60));
    desc_rates2(dii,2) = min(diff(D(alldives2(dii,1):alldives2(dii,3)))/(sampint*24*60*60));
    desc_rates2(dii,3) = max(diff(D(alldives2(dii,1):alldives2(dii,3)))/(sampint*24*60*60));
    desc_rates2(dii,4) = var(diff(D(alldives2(dii,1):alldives2(dii,3)))/(sampint*24*60*60));

    asc_rates2(dii,1) = mean(diff(D(alldives2(dii,4):alldives2(dii,2)))/(sampint*24*60*60));
    asc_rates2(dii,2) = max(diff(D(alldives2(dii,4):alldives2(dii,2)))/(sampint*24*60*60));
    asc_rates2(dii,3) = min(diff(D(alldives2(dii,4):alldives2(dii,2)))/(sampint*24*60*60));
    asc_rates2(dii,4) = var(diff(D(alldives2(dii,4):alldives2(dii,2)))/(sampint*24*60*60));
end;

%%Between dive stats
for dii = 1:size(alldives2,1)-1
    intervaltemps2(dii,1) = mean(Ta(alldives2(dii,2):alldives2(dii+1,1)));
    intervaltemps2(dii,2) = min(Ta(alldives2(dii,2):alldives2(dii+1,1)));
    intervaltemps2(dii,3) = max(Ta(alldives2(dii,2):alldives2(dii+1,1)));
    intervaltemps2(dii,4) = var(Ta(alldives2(dii,2):alldives2(dii+1,1)));
    intervaltemps2(dii,5) = mean(Tb(alldives2(dii,2):alldives2(dii+1,1)));
    intervaltemps2(dii,6) = min(Tb(alldives2(dii,2):alldives2(dii+1,1)));
    intervaltemps2(dii,7) = max(Tb(alldives2(dii,2):alldives2(dii+1,1)));
    intervaltemps2(dii,8) = var(Tb(alldives2(dii,2):alldives2(dii+1,1)));

    intervaldepths2(dii,1) = mean(D(alldives2(dii,2):alldives2(dii+1,1)));
    intervaldepths2(dii,2) = min(D(alldives2(dii,2):alldives2(dii+1,1)));
    intervaldepths2(dii,3) = max(D(alldives2(dii,2):alldives2(dii+1,1)));
    intervaldepths2(dii,4) = var(D(alldives2(dii,2):alldives2(dii+1,1)));
end;
intervaltemps2(size(alldives2,1),:)=NaN;
intervaldepths2(size(alldives2,1),:)=NaN;

%%Dive stats for definition 3
for dii = 1:size(alldives3,1)
    %Extract mean, max, and variance of within-dive depths
    divedepths3(dii,1) = mean(D(alldives3(dii,1):alldives3(dii,2)));
    divedepths3(dii,2) = max(D(alldives3(dii,1):alldives3(dii,2)));
    divedepths3(dii,3) = var(D(alldives3(dii,1):alldives3(dii,2)));
    %Extract mean and variance of depths between the first and last maxima
    divedepths3(dii,4) = mean(D(alldives3(dii,3):alldives3(dii,4)));
    divedepths3(dii,5) = var(D(alldives3(dii,3):alldives3(dii,4)));
    divedepths3(dii,6) = D(alldives3(dii,1));
    divedepths3(dii,7) = D(alldives3(dii,2));

    %Extract mean, max, and variance of within-dive ambient temps
    divetemps3(dii,1) = mean(Ta(alldives3(dii,1):alldives3(dii,2)));
    divetemps3(dii,2) = min(Ta(alldives3(dii,1):alldives3(dii,2)));
    divetemps3(dii,3) = var(Ta(alldives3(dii,1):alldives3(dii,2)));
    %Extract mean and variance between the first and last maxima
    divetemps3(dii,4) = mean(Ta(alldives3(dii,3):alldives3(dii,4)));
    divetemps3(dii,5) = var(Ta(alldives3(dii,3):alldives3(dii,4)));

    %Extract mean, max, and variance of within-dive body temps
    divetemps3(dii,6) = mean(Tb(alldives3(dii,1):alldives3(dii,2)));
    divetemps3(dii,7) = min(Tb(alldives3(dii,1):alldives3(dii,2)));
    divetemps3(dii,8) = var(Tb(alldives3(dii,1):alldives3(dii,2)));
    %Extract mean and variance between the first and last maxima
    divetemps3(dii,9) = mean(Tb(alldives3(dii,3):alldives3(dii,4)));
    divetemps3(dii,10) = var(Tb(alldives3(dii,3):alldives3(dii,4)));

    %Extract mean, max, and variance of within-dive thermal excesses
    divetemps3(dii,11) = mean(Te(alldives3(dii,1):alldives3(dii,2)));
    divetemps3(dii,12) = min(Te(alldives3(dii,1):alldives3(dii,2)));
    divetemps3(dii,13) = var(Te(alldives3(dii,1):alldives3(dii,2)));
    %Extract mean and variance  between the first and last maxima
    divetemps3(dii,14) = mean(Te(alldives3(dii,3):alldives3(dii,4)));
    divetemps3(dii,15) = var(Te(alldives3(dii,3):alldives3(dii,4)));

    %Extract mean, max, and variance of within-dive light levels
    divelight3(dii,1) = mean(hv(alldives3(dii,1):alldives3(dii,2)));
    divelight3(dii,2) = min(hv(alldives3(dii,1):alldives3(dii,2)));
    divelight3(dii,3) = var(hv(alldives3(dii,1):alldives3(dii,2)));
    %Extract mean and variance between the first and last maxima
    divelight3(dii,4) = mean(hv(alldives3(dii,3):alldives3(dii,4)));
    divelight3(dii,5) = var(hv(alldives3(dii,3):alldives3(dii,4)));

    %Calculate mean, max, min, variance of descent/ascent rates
    desc_rates3(dii,1) = mean(diff(D(alldives3(dii,1):alldives3(dii,3)))/(sampint*24*60*60));
    desc_rates3(dii,2) = min(diff(D(alldives3(dii,1):alldives3(dii,3)))/(sampint*24*60*60));
    desc_rates3(dii,3) = max(diff(D(alldives3(dii,1):alldives3(dii,3)))/(sampint*24*60*60));
    desc_rates3(dii,4) = var(diff(D(alldives3(dii,1):alldives3(dii,3)))/(sampint*24*60*60));

    asc_rates3(dii,1) = mean(diff(D(alldives3(dii,4):alldives3(dii,2)))/(sampint*24*60*60));
    asc_rates3(dii,2) = max(diff(D(alldives3(dii,4):alldives3(dii,2)))/(sampint*24*60*60));
    asc_rates3(dii,3) = min(diff(D(alldives3(dii,4):alldives3(dii,2)))/(sampint*24*60*60));
    asc_rates3(dii,4) = var(diff(D(alldives3(dii,4):alldives3(dii,2)))/(sampint*24*60*60));
end;

%%Between dive stats
for dii = 1:size(alldives3,1)-1
    intervaltemps3(dii,1) = mean(Ta(alldives3(dii,2):alldives3(dii+1,1)));
    intervaltemps3(dii,2) = min(Ta(alldives3(dii,2):alldives3(dii+1,1)));
    intervaltemps3(dii,3) = max(Ta(alldives3(dii,2):alldives3(dii+1,1)));
    intervaltemps3(dii,4) = var(Ta(alldives3(dii,2):alldives3(dii+1,1)));
    intervaltemps3(dii,5) = mean(Tb(alldives3(dii,2):alldives3(dii+1,1)));
    intervaltemps3(dii,6) = min(Tb(alldives3(dii,2):alldives3(dii+1,1)));
    intervaltemps3(dii,7) = max(Tb(alldives3(dii,2):alldives3(dii+1,1)));
    intervaltemps3(dii,8) = var(Tb(alldives3(dii,2):alldives3(dii+1,1)));

    intervaldepths3(dii,1) = mean(D(alldives3(dii,2):alldives3(dii+1,1)));
    intervaldepths3(dii,2) = min(D(alldives3(dii,2):alldives3(dii+1,1)));
    intervaldepths3(dii,3) = max(D(alldives3(dii,2):alldives3(dii+1,1)));
    intervaldepths3(dii,4) = var(D(alldives3(dii,2):alldives3(dii+1,1)));
end;
intervaltemps3(size(alldives3,1),:)=NaN;
intervaldepths3(size(alldives3,1),:)=NaN;



%Extract daily dive stats
days =unique(fix(Date));
for dd = 1:length(days)

    %%%%%%%%%%%%%%%%defn 2
    toda = (fix(divetimes2(:,1))==days(dd));
    daily_dives2(dd,1) = days(dd);
    daily_ints2(dd,1) = days(dd);
    if sum(toda)>0
        daily_dives2(dd,2) = sum(toda);%#dives
        daily_dives2(dd,3) = mean(divetimes2(toda,3))*24;%dive duration
        daily_dives2(dd,4) = nanmean(divetimes2(toda,4))*24;%dive interval
        daily_dives2(dd,5) = mean(divedepths2(toda,1));%mean mean depth
        daily_dives2(dd,6) = mean(divedepths2(toda,4));%mean mean depth b/t max
        daily_dives2(dd,7) = max(divedepths2(toda,2));%max dive depth
        daily_dives2(dd,8) = mean(divedepths2(toda,3));%mean var
        daily_dives2(dd,9) = mean(divedepths2(toda,5));%mean var b/t max
        daily_dives2(dd,10) = mean(desc_rates2(toda,1));%mean mean desc rate
        daily_dives2(dd,11) = max(desc_rates2(toda,2));%max desc rate
        daily_dives2(dd,12) = mean(asc_rates2(toda,1));%mean mean asc rate
        daily_dives2(dd,13) = min(asc_rates2(toda,2));%max asc rate
        daily_dives2(dd,14) = mean(divetemps2(toda,1));%mean mean temp
        daily_dives2(dd,15) = min(divetemps2(toda,2));%min temp
        daily_dives2(dd,16) = max(divetemps2(toda,2));%max temp
        daily_dives2(dd,17) = mean(divetemps2(toda,6));%mean mean temp
        daily_dives2(dd,18) = min(divetemps2(toda,7));%min temp
        daily_dives2(dd,19) = mean(divetemps2(toda,8));%mean var temp
        daily_dives2(dd,20) = mean(divetemps2(toda,11));%mean mean temp
        daily_dives2(dd,21) = min(divetemps2(toda,12));%min temp
        daily_dives2(dd,22) = mean(divetemps2(toda,13));%mean var temp
        daily_dives2(dd,23) = min(divelight2(toda,2));%min min light
        daily_dives2(dd,24) = mean(divelight2(toda,1));%mean mean light
        daily_dives2(dd,25) = mean(divedepths2(toda,8));%mean d by which mean dive depth exceeds ILD

        daily_ints2(dd,2) = nanmean(intervaldepths2(toda,1));% mean mean depth
        daily_ints2(dd,3) = nanmean(intervaltemps2(toda,1));% mean mean Ta
        daily_ints2(dd,4) = nanmean(intervaltemps2(toda,5));% mean mean Tb
        daily_ints2(dd,5) = nanmean(intervaltemps2(toda,8));% mean var Tb


        daily_dives2_hist(dd,:) = hist((divetimes2(toda,1)-fix(divetimes2(toda,1)))*24,0.5:1:24);
        for hr = 0:23
            rthr = (divetimes2(toda,1)-fix(divetimes2(toda,1)))*24;
            rthr = rthr>=hr & rthr<hr+1;
            temdeps = divedepths2(toda,2);
            daily_dives2_histz(dd,hr+1)=nanmean(temdeps(rthr));
            clear temdep;
        end;

    else
        daily_dives2(dd,2) = 0;
        daily_dives2(dd,3:25)=NaN;
        daily_dives2_hist(dd,1:24)=0;
        daily_dives2_histz(dd,1:24)=0;

        daily_ints2(dd,2:5)=NaN;

    end

    %%%%%%%%%%%%%%%%defn 3
    toda = (fix(divetimes3(:,1))==days(dd));
    daily_dives3(dd,1) = days(dd);
    daily_ints3(dd,1) = days(dd);
    if sum(toda)>0
        daily_dives3(dd,2) = sum(toda);%#dives
        daily_dives3(dd,3) = mean(divetimes3(toda,3))*24;%dive duration
        daily_dives3(dd,4) = nanmean(divetimes3(toda,4))*24;%dive interval
        daily_dives3(dd,5) = mean(divedepths3(toda,1));%mean mean depth
        daily_dives3(dd,6) = mean(divedepths3(toda,4));%mean mean depth b/t max
        daily_dives3(dd,7) = max(divedepths3(toda,2));%max dive depth
        daily_dives3(dd,8) = mean(divedepths3(toda,3));%mean var
        daily_dives3(dd,9) = mean(divedepths3(toda,5));%mean var b/t max
        daily_dives3(dd,10) = mean(desc_rates3(toda,1));%mean mean desc rate
        daily_dives3(dd,11) = max(desc_rates3(toda,2));%max desc rate
        daily_dives3(dd,12) = mean(asc_rates3(toda,1));%mean mean asc rate
        daily_dives3(dd,13) = min(asc_rates3(toda,2));%max asc rate
        daily_dives3(dd,14) = mean(divetemps3(toda,1));%mean mean temp
        daily_dives3(dd,15) = min(divetemps3(toda,2));%min temp
        daily_dives3(dd,16) = max(divetemps3(toda,2));%max temp
        daily_dives3(dd,17) = mean(divetemps3(toda,6));%mean mean temp
        daily_dives3(dd,18) = min(divetemps3(toda,7));%min temp
        daily_dives3(dd,19) = mean(divetemps3(toda,8));%mean var temp
        daily_dives3(dd,20) = mean(divetemps3(toda,11));%mean mean temp
        daily_dives3(dd,21) = min(divetemps3(toda,12));%min temp
        daily_dives3(dd,22) = mean(divetemps3(toda,13));%mean var temp
        daily_dives3(dd,23) = min(divelight3(toda,2));%min min light
        daily_dives3(dd,24) = mean(divelight3(toda,1));%mean mean light

        daily_ints3(dd,2) = nanmean(intervaldepths3(toda,1));% mean mean depth
        daily_ints3(dd,3) = nanmean(intervaltemps3(toda,1));% mean mean Ta
        daily_ints3(dd,4) = nanmean(intervaltemps3(toda,5));% mean mean Tb
        daily_ints3(dd,5) = nanmean(intervaltemps3(toda,8));% mean var Tb

        daily_dives3_hist(dd,:) = hist((divetimes3(toda,1)-fix(divetimes3(toda,1)))*24,0.5:1:24);
        for hr = 0:23
            rthr = (divetimes3(toda,1)-fix(divetimes3(toda,1)))*24;
            rthr = rthr>=hr & rthr<hr+1;
            temdeps = divedepths3(toda,2);
            daily_dives3_histz(dd,hr+1)=nanmean(temdeps(rthr));
            clear temdep;
        end;

    else
        daily_dives3(dd,2) = 0;
        daily_dives3(dd,3:24)=NaN;
        daily_dives3_hist(dd,1:24)=0;
        daily_dives3_histz(dd,1:24)=0;

        daily_ints3(dd,2:5)=NaN;

    end
end;%%Loop over days

if plotchk
    figZts=figure;
    for dd = 1:nthplot:length(days)
        wantedrows=find(fix(Date)==days(dd));
        daydepth = D(wantedrows);
        dayatemp = Ta(wantedrows);
        daybtemp = Tb(wantedrows);
        timeax = (Date(wantedrows)-fix(Date(wantedrows)))*24;


        figure(figZts);Z2ts=subplot('position',[.05 .5 .75 .4]);plot(timeax,daydepth,'.-');set(gca,'ydir','rev');hold on;
        deplims = get(gca,'ylim');
        plot(timeax,daydepth,'.-');set(gca,'ydir','rev');hold on;set(gca,'xlim',[0 24]);set(gca,'xtick',[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]);

        title(datestr(days(dd)));


        Z3ts=subplot('position',[.05 .05 .75 .4]);plot(timeax,daydepth,'.-');set(gca,'ydir','rev');hold on;set(gca,'xlim',[0 24]);
        deplims = get(gca,'ylim');
        plot(timeax,daydepth,'.-');set(gca,'ydir','rev');hold on;set(gca,'xlim',[0 24]);set(gca,'xtick',[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]);



        subplot(Z2ts);
        divewinday = find(Date(alldives2(:,1))>=Date(wantedrows(1)) & Date(alldives2(:,2))<=Date(wantedrows(end)));
        if ~isempty(divewinday)
            today = fix(Date(alldives2(divewinday(1),1)));
            if dd>1 & divewinday(1)>1
                if Date(alldives2(divewinday(1)-1,2))>=Date(wantedrows(1))
                    divewinday = [divewinday(1)-1;divewinday];
                end;
            end;
            if divewinday(end)+1 <= size(alldives2,1) % castleton - added because tag 1008130 was causing an error on day 9/13/08, couldn't track down calc error
                if dd<NDays & Date(alldives2(divewinday(end)+1,1))<=Date(wantedrows(end))
                    divewinday = [divewinday;divewinday(end)+1;];
                end;
            end; 
            divecolor = 'g';
            for dii = 1:length(divewinday)
                temtime=(Date(alldives2(divewinday(dii),1):alldives2(divewinday(dii),2))-today)*24;
                temz=D(alldives2(divewinday(dii),1):alldives2(divewinday(dii),2));
                plot(temtime(temtime<24),temz(temtime<24),'color',divecolor,'linewidth',2);
                if strcmp(divecolor,'g')
                    divecolor='y';
                else
                    divecolor='g';
                end;
            end;
        end;

        subplot(Z3ts);
        divewinday = find(Date(alldives3(:,1))>=Date(wantedrows(1)) & Date(alldives3(:,2))<=Date(wantedrows(end)));
        if ~isempty(divewinday)
            today = fix(Date(alldives3(divewinday(1),1)));
            if dd>1 & divewinday(1)>1
                if Date(alldives3(divewinday(1)-1,2))>=Date(wantedrows(1))
                    divewinday = [divewinday(1)-1;divewinday];
                end;
            end;
            if dd<NDays & Date(alldives3(divewinday(end)+1,1))<=Date(wantedrows(end))
                divewinday = [divewinday;divewinday(end)+1;];
            end;
            divecolor = 'g';
            for dii = 1:length(divewinday)
                temtime=(Date(alldives3(divewinday(dii),1):alldives3(divewinday(dii),2))-today)*24;
                temz=D(alldives3(divewinday(dii),1):alldives3(divewinday(dii),2));
                plot(temtime(temtime<24),temz(temtime<24),'color',divecolor,'linewidth',2);
                if strcmp(divecolor,'g')
                    divecolor='y';
                else
                    divecolor='g';
                end;
            end;
        end;

        disp('Paused - hit any key to continue');
        pause;clf;

    end;
    close(figZts);
end;

eval(['save ' outfilename ' alldives* asc* desc* daily_ints* daily_dives* divedepths* divetimes* divetemps* divelight* intervaltemps* intervaldepths*']);


% clear alldives* asc* desc* daily_dives* daily_ints* daydives* divedepths* divetimes* daily_dives* divetemps* intervaltemps* intervaldepths* divelight*
