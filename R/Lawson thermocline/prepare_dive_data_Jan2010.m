%%Extract some quantities of interest from the time-series data
clear;

%%%%%PARAMETERS TO SET%%%%%%%%%%%%
%dirofinterest = 'C:\MATLAB\input_data';    %directory where DC.mat files live
dirofinterest = 'E:\processing\projects\dive_analysis\input_data';
tagname = '1008130MA0108A0579';   %toppid and tagcode for fish of interest
outfilesuf = 'input_data_for_dive_analysis_no912';   %output file name suffix (will be appended to toppid/tagcode)
tint=4; %%water column profiles are generated every 24/tint hours
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%load the data
cd(dirofinterest);
eval(['load ' tagname 'DC_no912.mat']);

%Calculate date
Date = datenum(M(:,6:11));
gd = find(Date <= datenum([2008 09 14])); % end date filter
M = M(gd,:);
Date = Date(gd);
clear gd;


%figure out number of time-series observations per day, and number of lines
%that correspond to tint of time
obs_per_day = 1/(round((Date(2)-Date(1))*24*3600)/24/3600);    %sampling interval in decimal days
blocklines = ceil(obs_per_day/tint);

%%Only want the first full day of data through the last full day, so cut
%%any partial days:
if rem(Date(1),floor(Date(1))) > 0
    gd = Date >= floor(Date(1)) + 1;
end;
if rem(Date(end),floor(Date(end))) ~= 1-1/obs_per_day
    gd = gd & Date < floor(Date(end));
end;

%rename some things
Date = Date(gd);
D = M(gd,1);
Ta = M(gd,4);
Tb = M(gd,3);
hv = M(gd,2);


%estimate the isothermal layer depth (ILD)
%[tcline tcline_dates tclineflaggs tgrad] = find_tcline_fn_6May2008(Date,D,Ta,0.8,tint,blocklines,0,0.6,50,50,3,0,50);
[tcline tcline_dates tclineflaggs tgrad] = find_tcline_fn_6May2008(Date,D,Ta,0.8,tint,blocklines,0,0.6,50,50,3,0,1);

%save
eval(['save ' tagname '_' outfilesuf '.mat D* T* hv tcline tint']);
clear all;



return;


%%%%The mess below is where I identify Gulf of Maine portions of each
%%%%track, calculate certain things (eg mean depth per day) and such. I'm
%%%%including it here in case there's anything useful...but it might be a
%%%%bit impenetrable...

clear all;close all;

%%%Set some parameters
tclinethresh = 0.8;   %.8;%threshold criterion for estimating ILD
tint=4; %%water column profiles are generated every 24/tint hours
tempint = 1;
zint = 10;
bins_temp = 0:tempint:40; %bins for temperature histograms
bins_etemp = -40:tempint:40; %bins for temperature excess histograms
bins_depth = 5:zint:1000; %%bins for depth histograms

load c:\gareth\tuna_data\abft\TAG_Geodatabase_28May08_interpolated;
fishofinterest = ismember(FGadkey_interp,fishofinterest);
toppid_interp = toppid_interp(fishofinterest);
tagcode_interp = tagcode_interp(fishofinterest);
FGadkey_all = FGadkey_interp;
FGadkey_interp = FGadkey_interp(fishofinterest);
length_interp = length_interp(fishofinterest);
date_at_dep_interp = date_at_dep_interp(fishofinterest);
series_interp = series_interp(fishofinterest);
stock_interp = stock_interp(fishofinterest);
tagtype_interp = tagtype_interp(fishofinterest);
%%Note: the vectors 'lotek', 'wc', etc, give the rows for each tag type
%%within these toppid_interp and such vectors AFTER the latter have been
%%truncated to only the fish of interest

%%Cut out a few tags where the TS data proved to be bad
lotek([15]) = [];   %2159. Bad Ta after Feb
wc([2 8 9]) = [];     %97027 and 97089 bad Ta data.
pat_archival(12) = [];  %02-643 Bad depth data after march

%%Load up bathymetry data for later plots
[Bathy,map] = imread('c:\gareth\matlab_code\mapping\worldbathy_shelves_miller_1000t.tif');

patloop=0;  %set to 1 if doing the pat tags, with no Tb

%%Loop over tags to extract summary data
for ta = 3%1:length(lotek)%length(lotek):-1:1

    %%load TS data
    toppid = toppid_interp(lotek(ta));
    tagcode = cell2mat(tagcode_interp(lotek(ta)));
    tag = strcat(num2str(toppid),tagcode);
    date_at_dep = date_at_dep_interp(lotek(ta));
    series = series_interp(lotek(ta));
    stock = stock_interp(lotek(ta));
    tagtype = tagtype_interp(lotek(ta));
    FGadkey = FGadkey_interp(lotek(ta));
    disp(['Tag ' num2str(ta) ' of ' num2str(length(lotek)) ', ' tag]);
    eval(['load C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\lotek\' tag 'DC']);
    TS = M;clear M;
    %%Format is col1=depth, 2=light at surface, 3=internal temp, 4=external temperature,
    %%5=Julian day (EST), 6=year, 7=month, 8=day, 9=hour, 10=min, 12=sec

    if strcmp(tagcode,'PA0100P0703');
        TS(115924:end,:)=[];
    end;
    %%pull out GL data
    rta = find(FGadkey_all==FGadkey_interp(lotek(ta)));
    GL = pos_interp(pos_interp(:,1)==FGadkey_interp(lotek(ta)),2:end);
    bathy = bathy_exact(pos_interp(:,1)==FGadkey_interp(lotek(ta)));
    %%Format is col1=date,col2=lat,col3=lon,col4=postype

    datesGL =fix(GL(:,1));
    yearGL = datevec(datesGL);yearGL = yearGL(:,1);
    datesTS = datenum(TS(:,6:11));
    sampint = datesTS(2)-datesTS(1);    %sampling interval in decimal days
    sampint = round(sampint*24*3600)/24/3600;
    sampint*24*3600
    obs_per_day = round(1/sampint);
    blocklines = ceil(obs_per_day/tint);
    n_hr = round((1/24)/sampint);   %number of samples in an hour...for smoothing


    %%Estimate year-class
    stdat = date_at_dep_interp(lotek(ta));
    yrclass = yrclass_fn(length_interp(lotek(ta)),stdat,stock_interp(lotek(ta)));

    %%Map of the NW Atlantic
    othr = GL(:,4)==2 | GL(:,4)==3;
    GL(othr,:)=[];yearGL(othr,:)=[];datesGL(othr,:)=[];bathy(othr)=[];
    axset=[];
    figGoM=basemap([-50 -78 55 35],'low',[1165 40 425 330],'Gulf of Maine',Bathy,map); %Map of the Gulf of Maine study region
    m_plot(GL(:,3),GL(:,2),'k.-','LineWidth',1.2);
    yrs = unique(yearGL(GL(:,4)~=2&GL(:,4)~=3));
    colo = ['y';'r';'g';'w';'b';'c';'m'];
    for yy = 1:length(yrs)
        yrnames(yy,:)=num2str(yrs(yy));
        axset(yy)=m_plot(GL(yearGL==yrs(yy) & GL(:,4)~=2 & GL(:,4)~=3,3),GL(yearGL==yrs(yy)& GL(:,4)~=2 & GL(:,4)~=3,2),'o','MarkerEdgeColor','k','MarkerFaceColor',colo(yy),'Markersize',5);
    end;
    legend(axset,yrnames);
    title(tag);

    %%Now identify just the Gulf of Maine and Canada part of the track
    studyreg = (GL(:,3)<-53 & GL(:,2)>38);
    if strcmp(tagcode,'LO01A2159')
        %%this fish spent time off Grand Banks, not of interest here. No
        %%longer in fact even part of the analysis
        studyreg = studyreg & GL(:,1)>=732273;
    elseif strcmp(tagcode,'LO01A0014')
        %%2nd year in region only a coupla points
        studyreg = studyreg & GL(:,1)<datenum(2003,0,0);
    elseif strcmp(tagcode,'LO01A0560')
        %%2nd year in region only a coupla points
        studyreg = studyreg & GL(:,1)<datenum(2004,0,0);
    elseif strcmp(tagcode,'LO01A2187')
        %%2nd year in region only a coupla points
        studyreg = studyreg & GL(:,1)>datenum(2005,0,0);
    elseif strcmp(tagcode,'MK7098518')
        %%2nd year in region only a coupla points
        studyreg = studyreg & GL(:,1)<=datenum(1999,6,2);
    end;

    glinreg=datesGL(studyreg);
    GLreg=GL(studyreg,:);
    bathyreg=bathy(studyreg);
    %     m_plot(GL(~studyreg,3),GL(~studyreg,2),'o','MarkerEdgeColor','k','MarkerFaceColor','k','Markersize',5);  %Plots tag track

    TSofinterest = zeros(size(datesTS));
    GLofint = zeros(size(datesGL));
    TSdaysinreg = [];
    if strcmp(tagcode,'MK7098508')
        disp(tagcode)
        yloopend=1
    else
        yloopend=length(yrs);
    end;
    for yy = 1:yloopend
        glinreg_yr = datevec(glinreg);glinreg_yr=glinreg_yr(:,1);
        winyr = glinreg(glinreg_yr==yrs(yy));
        if ~isempty(winyr) & sum(ismember(winyr,unique(fix(datesTS))))>0
            TSofinterest_tem = (datesTS>=winyr(1) & datesTS<winyr(end)+1);
            stens = find(TSofinterest_tem==1);
            TSdaysinreg = [TSdaysinreg;fix((datesTS(stens(1)):1:datesTS(stens(end)))')];
            TSofinterest = TSofinterest | TSofinterest_tem;
            GLofinterest_tem = (datesGL>=winyr(1) & datesGL<winyr(end)+1);
            GLofint = GLofint | GLofinterest_tem;
        end;
        clear TSofinterest_tem winyr glinreg_yr yrnames GLofinterest_tem;
    end;
    m_plot(GL(~GLofint,3),GL(~GLofint,2),'o','MarkerEdgeColor','k','MarkerFaceColor','k','Markersize',5);  %Plots tag track

    tt = find(GL(:,4)>3);
    datestr([datesGL([tt(1) tt(end)]);TSdaysinreg([1 end])])
    clear tt;

    %%Estimate length for each day with TS data
    [daily_length daily_age] = trackgrowfn(length_interp(lotek(ta)),date_at_dep_interp(lotek(ta)),stock_interp(lotek(ta)),TSdaysinreg);


    %%Get water column temperature profiles every 24/tint hours
    try
        eval(['load C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\lotek\' tag '_temp_profile_' num2str(24/tint) 'hrs'])
    catch
        [temp_profiles temp_date_vec] = xbt_g_new(TS,tag,tint);
        eval(['save C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\lotek\' tag '_temp_profile_' num2str(24/tint) 'hrs temp_profiles temp_date_vec'])
    end;
    figprof=figure;
    c = colormap;
    c(1,:) = 1;
    colormap(c);
    imagesc(temp_profiles');
    caxis([0 28]);
    hold on;
    tt=(temp_profiles<0);
    disp(['Spline fits to temp-depth data resulted in ' num2str(sum(sum(tt))) ' points <0']);
    clear tt;

    %%Get isothermal layer depth
    try
        eval(['load C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\lotek\' tag '_tcline_' num2str(24/tint) 'hrs tcline tcline_dates tclineflaggs tgrad'])
    catch
        [tcline tcline_dates tclineflaggs tgrad] = find_tcline_fn_6May2008(datesTS,TS(:,1),TS(:,4),0.8,tint,blocklines,0,0.6,50,50,3,0,50);
        eval(['save C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\lotek\' tag '_tcline_' num2str(24/tint) 'hrs tcline tcline_dates tclineflaggs tgrad'])
    end;
    figure(figprof);
    temcline = tcline(:,1);temcline(temcline==9999)=NaN;
    plot(temcline,'k','linewidth',2)


    if sum(temp_date_vec - tcline_dates)>0
        disp('Dates funny tcline vs. profiles...');
    end;


    %%Pull out or derive some data useful for later analyses
    %temp_profiles = temp_profiles;
    if sum(TSofinterest)>0
        Date = datesTS(TSofinterest);
        D = TS(TSofinterest,1);
        Ta = TS(TSofinterest,4);
        Tb = TS(TSofinterest,3);
        if patloop
            Tb = TS(TSofinterest,3)*NaN;    %pat archivals have 0s for Tb. Need to set to nan.
            disp('Processing pats');
        end;
        hv = TS(TSofinterest,2);
        [lengths_TS ages_TS] = trackgrowfn(length_interp(lotek(ta)),date_at_dep_interp(lotek(ta)),stock_interp(lotek(ta)),Date);


        TSdaysofinterest = ismember(fix(temp_date_vec),TSdaysinreg);
        prof = temp_profiles(TSdaysofinterest,:);
        prof_date = temp_date_vec(TSdaysofinterest,:);
        tcline = tcline(TSdaysofinterest,:);
        tcline_dates = tcline_dates(TSdaysofinterest,:);
        tclineflaggs = tclineflaggs(TSdaysofinterest,:);
        tgrad = tgrad(TSdaysofinterest,:);


        %%%%%%%%START LOOP OVER DAYS%%%%%%%%%%
        %%First go through all days to get dawn/dusk
        daytime = zeros(size(Date));
        daytime_sm = 0*daytime;
        dayindices = [];dayindices_sm=[];
        %     if strcmp(tagcode(1),'L')
        %         %%Load up the sunrise-sunset data for lotek tags
        %         eval(['ssss = load(''C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\lotek\' tag '_SSSS.csv'');']);
        %         ssdates = datenum(ssss(:,1:3));
        %         sunriseset = NaN*ones(length(TSdaysinreg),3);
        %         sunriseset(:,1) = TSdaysinreg;
        %         for dd = 1:length(TSdaysinreg)
        %             rrr = find(ssdates==TSdaysinreg(dd));
        %             if ~isempty(rrr)
        %                 sunriseset(dd,2:3)=ssss(rrr,4:5)-5/24;
        %             end;
        %         end;
        %         sunriseset(sunriseset(:,3)<0,3)=1+sunriseset(sunriseset(:,3)<0,3);
        %         clear ssss ssdates;
        %     end;

        for dd = 1:length(TSdaysinreg)
            dayslight = hv(fix(Date)==TSdaysinreg(dd));
            dayslight_sm = smooth(dayslight,n_hr);
            daysdate = Date(fix(Date)==TSdaysinreg(dd));
            %Identify day/night. Dayindices has the dawn/dusk points for each
            %day. daytime has 1s where it is day.
            daytem1 = (dayslight_sm-mean(dayslight_sm))/std(dayslight_sm);
            daytem2 = (dayslight_sm(2:end)-mean(dayslight_sm))/std(dayslight_sm);daytem2(end+1)=NaN;
            daystend = daytem1>0 & daytem2>0;%%requires 2 points in a row to be>0 to avoid nighttime spikes. Means that the last point can't be sunset.
            dayindx = find(daystend==1);
            day = logical(zeros(size(dayslight)));
            day(dayindx(1):dayindx(end)) = 1;
            if ~isempty(dayindx)
                dayindices = [dayindices;dayindx(1) dayindx(end)];
                daytime = daytime | ismember(Date,daysdate(dayindx(1):dayindx(end)));
            else
                dayindices = [dayindices;NaN NaN];
            end;

        end;

        %%Now create a smoothed dayindices
        dayindices_sm(:,1) = round(smooth(dayindices(:,1),5));
        dayindices_sm(:,2) = round(smooth(dayindices(:,2),5));

        for dd = 1:length(TSdaysinreg)
            daysdate = Date(fix(Date)==TSdaysinreg(dd));
            if sum(isnan(dayindices_sm(dd,:)))==0
                daytime_sm = daytime_sm | ismember(Date,daysdate(dayindices_sm(dd,1):dayindices_sm(dd,2)));
            end;
        end;

        figure;%plot(sunriseset(:,1),sunriseset(:,2),'.-');
        hold on;
        plot(TSdaysinreg,dayindices(:,1)/obs_per_day,'r.-');
        plot(TSdaysinreg,dayindices_sm(:,1)/obs_per_day,'g.-');
        legend('Light','Light smoothed');%legend('LTD','Light','Light smoothed');
        datetick('x','keeplimits');
        figure;%plot(sunriseset(:,1),sunriseset(:,3),'.-');
        hold on;
        plot(TSdaysinreg,dayindices(:,2)/obs_per_day,'r.-');
        plot(TSdaysinreg,dayindices_sm(:,2)/obs_per_day,'g.-');
        legend('Light','Light smoothed');%legend('LTD','Light','Light smoothed');
        datetick('x','keeplimits');


        %     figdaylight=figure;
        %     for dd = 1:20:length(TSdaysinreg);
        %         dayslight = hv(fix(Date)==TSdaysinreg(dd));
        %         dayslight_sm = smooth(dayslight,n_hr);
        %         daysdate = Date(fix(Date)==TSdaysinreg(dd));
        %         day = daytime(fix(Date)==TSdaysinreg(dd));
        %         day_sm = daytime_sm(fix(Date)==TSdaysinreg(dd));
        %         figure(figdaylight);
        %         temtime = (daysdate-floor(daysdate(1)))*24;
        %         plot(temtime,dayslight);hold on;
        %         plot(temtime,dayslight_sm,'r');
        % %         plot(temtime(day),dayslight_sm(day),'k.');
        %         plot(temtime(day),15*ones(size(dayslight_sm(day))),'g.-');
        %         plot(temtime(day_sm),10*ones(size(dayslight_sm(day_sm))),'m.-');
        %         plot(temtime([dayindices(dd,1) dayindices(dd,2)]),dayslight_sm([dayindices(dd,1) dayindices(dd,2)]),'ko','markerfacecolor','g');
        %         plot(temtime([dayindices_sm(dd,1) dayindices_sm(dd,2)]),dayslight_sm([dayindices_sm(dd,1) dayindices_sm(dd,2)]),'ko','markerfacecolor','m');
        % %         if ~isnan(sunriseset(dd,2))
        % %             pts = round(sunriseset(dd,2:3)*obs_per_day);
        % %             plot(temtime(pts),dayslight_sm(pts),'ko','markerfacecolor','c');
        % %         end;
        %         ylim([0 400]);
        %         title(datestr(TSdaysinreg(dd)));pause;
        %         clf;
        %     end;

        %%Now for each day go through and get various pieces of info.
        hh=waitbar(0,'Looping over days');
        for dd = 1:length(TSdaysinreg)

            %%Pull out the day's data
            dayslight = hv(fix(Date)==TSdaysinreg(dd));
            dayslight_sm = smooth(dayslight,n_hr);
            daysdate = Date(fix(Date)==TSdaysinreg(dd));
            daysdepth = D(fix(Date)==TSdaysinreg(dd));
            daysatemp = Ta(fix(Date)==TSdaysinreg(dd));
            daysbtemp = Tb(fix(Date)==TSdaysinreg(dd));
            daysetemp = daysbtemp - daysatemp;
            day = daytime(fix(Date)==TSdaysinreg(dd));

            %%Get position and depth
            daily_pos(dd,1) = TSdaysinreg(dd);
            %%There are TS data for every day but geopositions for only some.
            %%Acct for this here
            rtda = find(datesGL==TSdaysinreg(dd));
            if ~isempty(rtda)
                daily_pos(dd,2:4) = GL(rtda,2:4);
                daily_bathy(dd,1) = -1*bathy(rtda);
            else
                daily_pos(dd,2:4) = NaN;
                daily_bathy(dd,1) = NaN;
            end;
            clear rtda

            %%Determine whether fish is above or below bottom of isothermal
            %%layer, for each time-series observation. The vector above_below
            %%has 1s where the measured depth is below the estimated ILD
            for bb=1:tint
                startrow = (bb*blocklines)-(blocklines-1) + (obs_per_day*(dd-1));
                if startrow>length(D)
                    break;
                end;
                if bb==1;sta1=startrow;end;
                endrow = (bb*blocklines) + (obs_per_day*(dd-1));
                if endrow>length(D)
                    endrow=length(D);
                    en1=endrow;
                    above_below(startrow:endrow,1) = D(startrow:endrow)>=tcline((dd-1)*tint+bb,1);
                end;
                if bb==tint;en1=endrow;end;
                above_below(startrow:endrow,1) = D(startrow:endrow)>=tcline((dd-1)*tint+bb,1);
            end;
            daysabovebelow = above_below(sta1:en1);


            %         if rem(dd,20)==0
            %             figure(figdaylight);
            %             temtime = (daysdate-floor(daysdate(1)))*24;
            %             plot(temtime,dayslight);hold on;
            %             plot(temtime,dayslight_sm,'r');
            %             plot(temtime(day),dayslight_sm(day),'k.');
            %             plot(temtime([dayindices(dd,1) dayindices(dd,2)]),dayslight_sm([dayindices(dd,1) dayindices(dd,2)]),'ko','markerfacecolor','g');
            %             title(datestr(TSdaysinreg(dd)));pause;
            %             clf;
            %         end;



            %%Calculate some basic descriptive statistics
            %%DEPTH
            daily_depth(dd,1) = nanmean(daysdepth);
            daily_depth(dd,2) = max(nanmin(daysdepth),0);
            daily_depth(dd,3) = nanmax(daysdepth);
            daily_depth(dd,4) = nanstd(daysdepth);
            daily_depth(dd,5:9) = prctile(daysdepth,[10 25 50 75 90]);
            rtema = find(daysatemp==min(daysatemp));
            if sum(~isnan(rtema))>0
                daily_depth(dd,10) = daysdepth(rtema(1));clear rtema
            else
                daily_depth(dd,10) = NaN;clear rtema;
            end;
            %         daily_depth(dd,11) = nansum(daysdepth);
            %         daily_depth(dd,12) = obs_per_day;
            daily_depth_hist(dd,:) = hist(daysdepth,bins_depth);

            if sum(day)>0
                daily_depth_day(dd,1) = nanmean(daysdepth(day));
                daily_depth_day(dd,2) = max(nanmin(daysdepth(day)),0);
                daily_depth_day(dd,3) = nanmax(daysdepth(day));
                daily_depth_day(dd,4) = nanstd(daysdepth(day));
                daily_depth_day(dd,5:9) = prctile(daysdepth(day),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(~day)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_day(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_day(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_day_hist(dd,:) = hist(daysdepth(day),bins_depth);
            else
                daily_depth_day(dd,1:10)=NaN;
                daily_depth_day_hist(dd,1:100) = NaN;
            end;

            if sum(~day)>0
                daily_depth_night(dd,1) = nanmean(daysdepth(~day));
                daily_depth_night(dd,2) = max(nanmin(daysdepth(~day)),0);
                daily_depth_night(dd,3) = nanmax(daysdepth(~day));
                daily_depth_night(dd,4) = nanstd(daysdepth(~day));
                daily_depth_night(dd,5:9) = prctile(daysdepth(~day),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(day)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_night(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_night(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_night_hist(dd,:) = hist(daysdepth(~day),bins_depth);
            else
                daily_depth_night(dd,1:10)=NaN;
                daily_depth_night_hist(dd,1:100) = NaN;
            end;

            if sum(~daysabovebelow)>0
                daily_depth_above(dd,1) = nanmean(daysdepth(~daysabovebelow));
                daily_depth_above(dd,2) = max(nanmin(daysdepth(~daysabovebelow)),0);
                daily_depth_above(dd,3) = nanmax(daysdepth(~daysabovebelow));
                daily_depth_above(dd,4) = nanstd(daysdepth(~daysabovebelow));
                daily_depth_above(dd,5:9) = prctile(daysdepth(~daysabovebelow),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(daysabovebelow)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_above(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_above(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_above_hist(dd,:) = hist(daysdepth(~daysabovebelow),bins_depth);
            else
                daily_depth_above(dd,1:10)=NaN;
                daily_depth_above_hist(dd,1:100) = NaN;
            end;

            if sum(daysabovebelow)>0
                daily_depth_below(dd,1) = nanmean(daysdepth(daysabovebelow));
                daily_depth_below(dd,2) = max(nanmin(daysdepth(daysabovebelow)),0);
                daily_depth_below(dd,3) = nanmax(daysdepth(daysabovebelow));
                daily_depth_below(dd,4) = nanstd(daysdepth(daysabovebelow));
                daily_depth_below(dd,5:9) = prctile(daysdepth(daysabovebelow),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(~daysabovebelow)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_below(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_below(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_below_hist(dd,:) = hist(daysdepth(daysabovebelow),bins_depth);
            else
                daily_depth_below(dd,1:10)=NaN;
                daily_depth_below_hist(dd,1:100) = NaN;
            end;

            if sum(day&daysabovebelow)>0
                daily_depth_daybelow(dd,1) = nanmean(daysdepth(day&daysabovebelow));
                daily_depth_daybelow(dd,2) = nanmin(daysdepth(day&daysabovebelow));
                daily_depth_daybelow(dd,3) = nanmax(daysdepth(day&daysabovebelow));
                daily_depth_daybelow(dd,4) = nanstd(daysdepth(day&daysabovebelow));
                daily_depth_daybelow(dd,5:9) = prctile(daysdepth(day&daysabovebelow),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(~daysabovebelow|~day)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_daybelow(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_daybelow(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_daybelow_hist(dd,:) = hist(daysdepth(day&daysabovebelow),bins_depth);
            else
                daily_depth_daybelow(dd,1:10)=NaN;
                daily_depth_daybelow_hist(dd,1:100) = NaN;
            end;

            if sum(day&~daysabovebelow)>0
                daily_depth_dayabove(dd,1) = nanmean(daysdepth(day&~daysabovebelow));
                daily_depth_dayabove(dd,2) = nanmin(daysdepth(day&~daysabovebelow));
                daily_depth_dayabove(dd,3) = nanmax(daysdepth(day&~daysabovebelow));
                daily_depth_dayabove(dd,4) = nanstd(daysdepth(day&~daysabovebelow));
                daily_depth_dayabove(dd,5:9) = prctile(daysdepth(day&~daysabovebelow),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(daysabovebelow|~day)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_dayabove(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_dayabove(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_dayabove_hist(dd,:) = hist(daysdepth(day&~daysabovebelow),bins_depth);
            else
                daily_depth_dayabove(dd,1:10)=NaN;
                daily_depth_dayabove_hist(dd,1:100) = NaN;
            end;

            if sum(~day&daysabovebelow)>0
                daily_depth_nightbelow(dd,1) = nanmean(daysdepth(~day&daysabovebelow));
                daily_depth_nightbelow(dd,2) = nanmin(daysdepth(~day&daysabovebelow));
                daily_depth_nightbelow(dd,3) = nanmax(daysdepth(~day&daysabovebelow));
                daily_depth_nightbelow(dd,4) = nanstd(daysdepth(~day&daysabovebelow));
                daily_depth_nightbelow(dd,5:9) = prctile(daysdepth(~day&daysabovebelow),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(~daysabovebelow|day)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_nightbelow(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_nightbelow(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_nightbelow_hist(dd,:) = hist(daysdepth(~day&daysabovebelow),bins_depth);
            else
                daily_depth_nightbelow(dd,1:10)=NaN;
                daily_depth_nightbelow_hist(dd,1:100) = NaN;
            end;

            if sum(~day&~daysabovebelow)>0
                daily_depth_nightabove(dd,1) = nanmean(daysdepth(~day&~daysabovebelow));
                daily_depth_nightabove(dd,2) = nanmin(daysdepth(~day&~daysabovebelow));
                daily_depth_nightabove(dd,3) = nanmax(daysdepth(~day&~daysabovebelow));
                daily_depth_nightabove(dd,4) = nanstd(daysdepth(~day&~daysabovebelow));
                daily_depth_nightabove(dd,5:9) = prctile(daysdepth(~day&~daysabovebelow),[10 25 50 75 90]);
                temdaysatemp = daysatemp;temdaysatemp(daysabovebelow|day)=NaN;rtdayatemp=find(temdaysatemp==nanmin(temdaysatemp));
                if sum(~isnan(rtdayatemp))>0
                    daily_depth_nightabove(dd,10) = daysdepth(rtdayatemp(1));clear temdaysatemp rtdayatemp;
                else
                    daily_depth_nightabove(dd,10) = NaN;clear temdaysatemp rtdayatemp;
                end;
                daily_depth_nightabove_hist(dd,:) = hist(daysdepth(~day&~daysabovebelow),bins_depth);
            else
                daily_depth_nightabove(dd,1:10)=NaN;
                daily_depth_nightabove_hist(dd,1:100) = NaN;
            end;


            %%%AMBIENT TEMPERATURE
            daily_atemp(dd,1) = nanmean(daysatemp);
            daily_atemp(dd,2) = nanmin(daysatemp);
            daily_atemp(dd,3) = nanmax(daysatemp);
            daily_atemp(dd,4) = nanstd(daysatemp);
            daily_atemp(dd,5:9) = prctile(daysatemp,[10 25 50 75 90]);
            rz = find(daysdepth==max(daysdepth));
            if ~isempty(rz)
                daily_atemp(dd,10) = daysatemp(rz(1));
            else
                daily_atemp(dd,10) = NaN;
            end;
            daily_atemp_hist(dd,:) = hist(daysatemp,bins_temp);

            if sum(day)>0
                daily_atemp_day(dd,1) = nanmean(daysatemp(day));
                daily_atemp_day(dd,2) = nanmin(daysatemp(day));
                daily_atemp_day(dd,3) = nanmax(daysatemp(day));
                daily_atemp_day(dd,4) = nanstd(daysatemp(day));
                daily_atemp_day(dd,5:9) = prctile(daysatemp(day),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~day)=NaN;
                rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                if ~isempty(rtdaydepth)
                    daily_atemp_day(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                else
                    daily_atemp_day(dd,10) = NaN;clear temdaysdepth rtdaydepth;
                end;
                daily_atemp_day_hist(dd,:) = hist(daysatemp(day),bins_temp);
            else
                daily_atemp_day(dd,1:10)=NaN;
                daily_atemp_day_hist(dd,1:41) = NaN;
            end;

            if sum(~day)>0
                daily_atemp_night(dd,1) = nanmean(daysatemp(~day));
                daily_atemp_night(dd,2) = nanmin(daysatemp(~day));
                daily_atemp_night(dd,3) = nanmax(daysatemp(~day));
                daily_atemp_night(dd,4) = nanstd(daysatemp(~day));
                daily_atemp_night(dd,5:9) = prctile(daysatemp(~day),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                if ~isempty(rtdaydepth)
                    daily_atemp_night(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                else
                    daily_atemp_night(dd,10) = NaN;clear temdaysdepth rtdaydepth;
                end;
                daily_atemp_night_hist(dd,:) = hist(daysatemp(~day),bins_temp);
            else
                daily_atemp_night(dd,1:10)=NaN;
                daily_atemp_night_hist(dd,1:41) = NaN;
            end;

            if sum(~daysabovebelow)>0
                daily_atemp_above(dd,1) = nanmean(daysatemp(~daysabovebelow));
                daily_atemp_above(dd,2) = nanmin(daysatemp(~daysabovebelow));
                daily_atemp_above(dd,3) = nanmax(daysatemp(~daysabovebelow));
                daily_atemp_above(dd,4) = nanstd(daysatemp(~daysabovebelow));
                daily_atemp_above(dd,5:9) = prctile(daysatemp(~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));

                if ~isempty(rtdaydepth)
                    daily_atemp_above(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                else
                    daily_atemp_above(dd,10) = NaN;clear temdaysdepth rtdaydepth;
                end;

                daily_atemp_above_hist(dd,:) = hist(daysatemp(~daysabovebelow),bins_temp);
            else
                daily_atemp_above(dd,1:10)=NaN;
                daily_atemp_above_hist(dd,1:41) = NaN;
            end;

            if sum(daysabovebelow)>0
                daily_atemp_below(dd,1) = nanmean(daysatemp(daysabovebelow));
                daily_atemp_below(dd,2) = nanmin(daysatemp(daysabovebelow));
                daily_atemp_below(dd,3) = nanmax(daysatemp(daysabovebelow));
                daily_atemp_below(dd,4) = nanstd(daysatemp(daysabovebelow));
                daily_atemp_below(dd,5:9) = prctile(daysatemp(daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                if ~isempty(rtdaydepth)
                    daily_atemp_below(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                else
                    daily_atemp_below(dd,10) = NaN;clear temdaysdepth rtdaydepth;
                end;
                daily_atemp_below_hist(dd,:) = hist(daysatemp(daysabovebelow),bins_temp);
            else
                daily_atemp_below(dd,1:10)=NaN;
                daily_atemp_below_hist(dd,1:41) = NaN;
            end;

            if sum(day&daysabovebelow)>0
                daily_atemp_daybelow(dd,1) = nanmean(daysatemp(day&daysabovebelow));
                daily_atemp_daybelow(dd,2) = nanmin(daysatemp(day&daysabovebelow));
                daily_atemp_daybelow(dd,3) = nanmax(daysatemp(day&daysabovebelow));
                daily_atemp_daybelow(dd,4) = nanstd(daysatemp(day&daysabovebelow));
                daily_atemp_daybelow(dd,5:9) = prctile(daysatemp(day&daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow|~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_atemp_daybelow(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_atemp_daybelow_hist(dd,:) = hist(daysatemp(day&daysabovebelow),bins_temp);
            else
                daily_atemp_daybelow(dd,1:10)=NaN;
                daily_atemp_daybelow_hist(dd,1:41) = NaN;
            end;

            if sum(day&~daysabovebelow)>0
                daily_atemp_dayabove(dd,1) = nanmean(daysatemp(day&~daysabovebelow));
                daily_atemp_dayabove(dd,2) = nanmin(daysatemp(day&~daysabovebelow));
                daily_atemp_dayabove(dd,3) = nanmax(daysatemp(day&~daysabovebelow));
                daily_atemp_dayabove(dd,4) = nanstd(daysatemp(day&~daysabovebelow));
                daily_atemp_dayabove(dd,5:9) = prctile(daysatemp(day&~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow|~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_atemp_dayabove(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_atemp_dayabove_hist(dd,:) = hist(daysatemp(day&~daysabovebelow),bins_temp);
            else
                daily_atemp_dayabove(dd,1:10)=NaN;
                daily_atemp_dayabove_hist(dd,1:41) = NaN;
            end;

            if sum(~day&daysabovebelow)>0
                daily_atemp_nightbelow(dd,1) = nanmean(daysatemp(~day&daysabovebelow));
                daily_atemp_nightbelow(dd,2) = nanmin(daysatemp(~day&daysabovebelow));
                daily_atemp_nightbelow(dd,3) = nanmax(daysatemp(~day&daysabovebelow));
                daily_atemp_nightbelow(dd,4) = nanstd(daysatemp(~day&daysabovebelow));
                daily_atemp_nightbelow(dd,5:9) = prctile(daysatemp(~day&daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow|day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_atemp_nightbelow(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_atemp_nightbelow_hist(dd,:) = hist(daysatemp(~day&daysabovebelow),bins_temp);
            else
                daily_atemp_nightbelow(dd,1:10)=NaN;
                daily_atemp_nightbelow_hist(dd,1:41) = NaN;
            end;

            if sum(~day&~daysabovebelow)>0
                daily_atemp_nightabove(dd,1) = nanmean(daysatemp(~day&~daysabovebelow));
                daily_atemp_nightabove(dd,2) = nanmin(daysatemp(~day&~daysabovebelow));
                daily_atemp_nightabove(dd,3) = nanmax(daysatemp(~day&~daysabovebelow));
                daily_atemp_nightabove(dd,4) = nanstd(daysatemp(~day&~daysabovebelow));
                daily_atemp_nightabove(dd,5:9) = prctile(daysatemp(~day&~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow|day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_atemp_nightabove(dd,10) = daysatemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_atemp_nightabove_hist(dd,:) = hist(daysatemp(~day&~daysabovebelow),bins_temp);
            else
                daily_atemp_nightabove(dd,1:10)=NaN;
                daily_atemp_nightabove_hist(dd,1:41) = NaN;
            end;


            %%%INTERNAL TEMPERATURE
            daily_btemp(dd,1) = nanmean(daysbtemp);
            daily_btemp(dd,2) = nanmin(daysbtemp);
            daily_btemp(dd,3) = nanmax(daysbtemp);
            daily_btemp(dd,4) = nanstd(daysbtemp);
            daily_btemp(dd,5:9) = prctile(daysbtemp,[10 25 50 75 90]);
            daily_btemp(dd,10) = daysbtemp(rz(1));
            daily_btemp_hist(dd,:) = hist(daysbtemp,bins_temp);

            if sum(day)>0
                daily_btemp_day(dd,1) = nanmean(daysbtemp(day));
                daily_btemp_day(dd,2) = nanmin(daysbtemp(day));
                daily_btemp_day(dd,3) = nanmax(daysbtemp(day));
                daily_btemp_day(dd,4) = nanstd(daysbtemp(day));
                daily_btemp_day(dd,5:9) = prctile(daysbtemp(day),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_day(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_day_hist(dd,:) = hist(daysbtemp(day),bins_temp);
            else
                daily_btemp_day(dd,1:10)=NaN;
                daily_btemp_day_hist(dd,1:41) = NaN;
            end;

            if sum(~day)>0
                daily_btemp_night(dd,1) = nanmean(daysbtemp(~day));
                daily_btemp_night(dd,2) = nanmin(daysbtemp(~day));
                daily_btemp_night(dd,3) = nanmax(daysbtemp(~day));
                daily_btemp_night(dd,4) = nanstd(daysbtemp(~day));
                daily_btemp_night(dd,5:9) = prctile(daysbtemp(~day),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_night(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_night_hist(dd,:) = hist(daysbtemp(~day),bins_temp);
            else
                daily_btemp_night(dd,1:10)=NaN;
                daily_btemp_night_hist(dd,1:41) = NaN;
            end;

            if sum(~daysabovebelow)>0
                daily_btemp_above(dd,1) = nanmean(daysbtemp(~daysabovebelow));
                daily_btemp_above(dd,2) = nanmin(daysbtemp(~daysabovebelow));
                daily_btemp_above(dd,3) = nanmax(daysbtemp(~daysabovebelow));
                daily_btemp_above(dd,4) = nanstd(daysbtemp(~daysabovebelow));
                daily_btemp_above(dd,5:9) = prctile(daysbtemp(~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_above(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_above_hist(dd,:) = hist(daysbtemp(~daysabovebelow),bins_temp);
            else
                daily_btemp_above(dd,1:10)=NaN;
                daily_btemp_above_hist(dd,1:41) = NaN;
            end;

            if sum(daysabovebelow)>0
                daily_btemp_below(dd,1) = nanmean(daysbtemp(daysabovebelow));
                daily_btemp_below(dd,2) = nanmin(daysbtemp(daysabovebelow));
                daily_btemp_below(dd,3) = nanmax(daysbtemp(daysabovebelow));
                daily_btemp_below(dd,4) = nanstd(daysbtemp(daysabovebelow));
                daily_btemp_below(dd,5:9) = prctile(daysbtemp(daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_below(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_below_hist(dd,:) = hist(daysbtemp(daysabovebelow),bins_temp);
            else
                daily_btemp_below(dd,1:10)=NaN;
                daily_btemp_below_hist(dd,1:41) = NaN;
            end;

            if sum(day&daysabovebelow)>0
                daily_btemp_daybelow(dd,1) = nanmean(daysbtemp(day&daysabovebelow));
                daily_btemp_daybelow(dd,2) = nanmin(daysbtemp(day&daysabovebelow));
                daily_btemp_daybelow(dd,3) = nanmax(daysbtemp(day&daysabovebelow));
                daily_btemp_daybelow(dd,4) = nanstd(daysbtemp(day&daysabovebelow));
                daily_btemp_daybelow(dd,5:9) = prctile(daysbtemp(day&daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow|~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_daybelow(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_daybelow_hist(dd,:) = hist(daysbtemp(day&daysabovebelow),bins_temp);
            else
                daily_btemp_daybelow(dd,1:10)=NaN;
                daily_btemp_daybelow_hist(dd,1:41) = NaN;
            end;

            if sum(day&~daysabovebelow)>0
                daily_btemp_dayabove(dd,1) = nanmean(daysbtemp(day&~daysabovebelow));
                daily_btemp_dayabove(dd,2) = nanmin(daysbtemp(day&~daysabovebelow));
                daily_btemp_dayabove(dd,3) = nanmax(daysbtemp(day&~daysabovebelow));
                daily_btemp_dayabove(dd,4) = nanstd(daysbtemp(day&~daysabovebelow));
                daily_btemp_dayabove(dd,5:9) = prctile(daysbtemp(day&~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow|~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_dayabove(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_dayabove_hist(dd,:) = hist(daysbtemp(day&~daysabovebelow),bins_temp);
            else
                daily_btemp_dayabove(dd,1:10)=NaN;
                daily_btemp_dayabove_hist(dd,1:41) = NaN;
            end;

            if sum(~day&daysabovebelow)>0
                daily_btemp_nightbelow(dd,1) = nanmean(daysbtemp(~day&daysabovebelow));
                daily_btemp_nightbelow(dd,2) = nanmin(daysbtemp(~day&daysabovebelow));
                daily_btemp_nightbelow(dd,3) = nanmax(daysbtemp(~day&daysabovebelow));
                daily_btemp_nightbelow(dd,4) = nanstd(daysbtemp(~day&daysabovebelow));
                daily_btemp_nightbelow(dd,5:9) = prctile(daysbtemp(~day&daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow|day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_nightbelow(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_nightbelow_hist(dd,:) = hist(daysbtemp(~day&daysabovebelow),bins_temp);
            else
                daily_btemp_nightbelow(dd,1:10)=NaN;
                daily_btemp_nightbelow_hist(dd,1:41) = NaN;
            end;

            if sum(~day&~daysabovebelow)>0
                daily_btemp_nightabove(dd,1) = nanmean(daysbtemp(~day&~daysabovebelow));
                daily_btemp_nightabove(dd,2) = nanmin(daysbtemp(~day&~daysabovebelow));
                daily_btemp_nightabove(dd,3) = nanmax(daysbtemp(~day&~daysabovebelow));
                daily_btemp_nightabove(dd,4) = nanstd(daysbtemp(~day&~daysabovebelow));
                daily_btemp_nightabove(dd,5:9) = prctile(daysbtemp(~day&~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow|day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_btemp_nightabove(dd,10) = daysbtemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_btemp_nightabove_hist(dd,:) = hist(daysbtemp(~day&~daysabovebelow),bins_temp);
            else
                daily_btemp_nightabove(dd,1:10)=NaN;
                daily_btemp_nightabove_hist(dd,1:41) = NaN;
            end;


            %%%TEMPERATURE EXCESS
            daily_etemp(dd,1) = nanmean(daysetemp);
            daily_etemp(dd,2) = nanmin(daysetemp);
            daily_etemp(dd,3) = nanmax(daysetemp);
            daily_etemp(dd,4) = nanstd(daysetemp);
            daily_etemp(dd,5:9) = prctile(daysetemp,[10 25 50 75 90]);
            daily_etemp(dd,10) = daysetemp(rz(1));clear rz;
            daily_etemp_hist(dd,:) = hist(daysetemp,bins_etemp);

            if sum(day)>0
                daily_etemp_day(dd,1) = nanmean(daysetemp(day));
                daily_etemp_day(dd,2) = nanmin(daysetemp(day));
                daily_etemp_day(dd,3) = nanmax(daysetemp(day));
                daily_etemp_day(dd,4) = nanstd(daysetemp(day));
                daily_etemp_day(dd,5:9) = prctile(daysetemp(day),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_day(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_day_hist(dd,:) = hist(daysetemp(day),bins_etemp);
            else
                daily_etemp_day(dd,1:10)=NaN;
                daily_etemp_day_hist(dd,1:81) = NaN;
            end;

            if sum(~day)>0
                daily_etemp_night(dd,1) = nanmean(daysetemp(~day));
                daily_etemp_night(dd,2) = nanmin(daysetemp(~day));
                daily_etemp_night(dd,3) = nanmax(daysetemp(~day));
                daily_etemp_night(dd,4) = nanstd(daysetemp(~day));
                daily_etemp_night(dd,5:9) = prctile(daysetemp(~day),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_night(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_night_hist(dd,:) = hist(daysetemp(~day),bins_etemp);
            else
                daily_etemp_night(dd,1:10)=NaN;
                daily_etemp_night_hist(dd,1:81) = NaN;
            end;

            if sum(~daysabovebelow)>0
                daily_etemp_above(dd,1) = nanmean(daysetemp(~daysabovebelow));
                daily_etemp_above(dd,2) = nanmin(daysetemp(~daysabovebelow));
                daily_etemp_above(dd,3) = nanmax(daysetemp(~daysabovebelow));
                daily_etemp_above(dd,4) = nanstd(daysetemp(~daysabovebelow));
                daily_etemp_above(dd,5:9) = prctile(daysetemp(~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_above(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_above_hist(dd,:) = hist(daysetemp(~daysabovebelow),bins_etemp);
            else
                daily_etemp_above(dd,1:10)=NaN;
                daily_etemp_above_hist(dd,1:81) = NaN;
            end;

            if sum(daysabovebelow)>0
                daily_etemp_below(dd,1) = nanmean(daysetemp(daysabovebelow));
                daily_etemp_below(dd,2) = nanmin(daysetemp(daysabovebelow));
                daily_etemp_below(dd,3) = nanmax(daysetemp(daysabovebelow));
                daily_etemp_below(dd,4) = nanstd(daysetemp(daysabovebelow));
                daily_etemp_below(dd,5:9) = prctile(daysetemp(daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_below(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_below_hist(dd,:) = hist(daysetemp(daysabovebelow),bins_etemp);
            else
                daily_etemp_below(dd,1:10)=NaN;
                daily_etemp_below_hist(dd,1:81) = NaN;
            end;

            if sum(day&daysabovebelow)>0
                daily_etemp_daybelow(dd,1) = nanmean(daysetemp(day&daysabovebelow));
                daily_etemp_daybelow(dd,2) = nanmin(daysetemp(day&daysabovebelow));
                daily_etemp_daybelow(dd,3) = nanmax(daysetemp(day&daysabovebelow));
                daily_etemp_daybelow(dd,4) = nanstd(daysetemp(day&daysabovebelow));
                daily_etemp_daybelow(dd,5:9) = prctile(daysetemp(day&daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow|~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_daybelow(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_daybelow_hist(dd,:) = hist(daysetemp(day&daysabovebelow),bins_etemp);
            else
                daily_etemp_daybelow(dd,1:10)=NaN;
                daily_etemp_daybelow_hist(dd,1:81) = NaN;
            end;

            if sum(day&~daysabovebelow)>0
                daily_etemp_dayabove(dd,1) = nanmean(daysetemp(day&~daysabovebelow));
                daily_etemp_dayabove(dd,2) = nanmin(daysetemp(day&~daysabovebelow));
                daily_etemp_dayabove(dd,3) = nanmax(daysetemp(day&~daysabovebelow));
                daily_etemp_dayabove(dd,4) = nanstd(daysetemp(day&~daysabovebelow));
                daily_etemp_dayabove(dd,5:9) = prctile(daysetemp(day&~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow|~day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_dayabove(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_dayabove_hist(dd,:) = hist(daysetemp(day&~daysabovebelow),bins_etemp);
            else
                daily_etemp_dayabove(dd,1:10)=NaN;
                daily_etemp_dayabove_hist(dd,1:81) = NaN;
            end;

            if sum(~day&daysabovebelow)>0
                daily_etemp_nightbelow(dd,1) = nanmean(daysetemp(~day&daysabovebelow));
                daily_etemp_nightbelow(dd,2) = nanmin(daysetemp(~day&daysabovebelow));
                daily_etemp_nightbelow(dd,3) = nanmax(daysetemp(~day&daysabovebelow));
                daily_etemp_nightbelow(dd,4) = nanstd(daysetemp(~day&daysabovebelow));
                daily_etemp_nightbelow(dd,5:9) = prctile(daysetemp(~day&daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(~daysabovebelow|day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_nightbelow(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_nightbelow_hist(dd,:) = hist(daysetemp(~day&daysabovebelow),bins_etemp);
            else
                daily_etemp_nightbelow(dd,1:10)=NaN;
                daily_etemp_nightbelow_hist(dd,1:81) = NaN;
            end;

            if sum(~day&~daysabovebelow)>0
                daily_etemp_nightabove(dd,1) = nanmean(daysetemp(~day&~daysabovebelow));
                daily_etemp_nightabove(dd,2) = nanmin(daysetemp(~day&~daysabovebelow));
                daily_etemp_nightabove(dd,3) = nanmax(daysetemp(~day&~daysabovebelow));
                daily_etemp_nightabove(dd,4) = nanstd(daysetemp(~day&~daysabovebelow));
                daily_etemp_nightabove(dd,5:9) = prctile(daysetemp(~day&~daysabovebelow),[10 25 50 75 90]);
                temdaysdepth = daysdepth;temdaysdepth(daysabovebelow|day)=NaN;rtdaydepth=find(temdaysdepth==nanmax(temdaysdepth));
                daily_etemp_nightabove(dd,10) = daysetemp(rtdaydepth(1));clear temdaysdepth rtdaydepth;
                daily_etemp_nightabove_hist(dd,:) = hist(daysetemp(~day&~daysabovebelow),bins_etemp);
            else
                daily_etemp_nightabove(dd,1:10)=NaN;
                daily_etemp_nightabove_hist(dd,1:81) = NaN;
            end;


            %%Proportion of time spent above or below ILD. col1=prop above,
            %%col2=prop below, col3=prop above day, col4=prop below day,
            %%col5=prop above night, col6=prop below night
            daily_above_below(dd,1) = sum(~daysabovebelow)/obs_per_day;
            daily_above_below(dd,2) = sum(daysabovebelow)/obs_per_day;
            daily_above_below(dd,3) = sum(~daysabovebelow(day))/sum(day);
            daily_above_below(dd,4) = sum(daysabovebelow(day))/sum(day);
            daily_above_below(dd,5) = sum(~daysabovebelow(~day))/sum(~day);
            daily_above_below(dd,6) = sum(daysabovebelow(~day))/sum(~day);

            daily_above_below_hist(dd,1) = sum(~daysabovebelow);
            daily_above_below_hist(dd,2) = sum(daysabovebelow);
            daily_above_below_hist(dd,3) = sum(~daysabovebelow(day));
            daily_above_below_hist(dd,4) = sum(daysabovebelow(day));
            daily_above_below_hist(dd,5) = sum(~daysabovebelow(~day));
            daily_above_below_hist(dd,6) = sum(daysabovebelow(~day));


            %%Get SST. col1=daily mean, col2=daytime mean, col3=nighttime mean
            if strcmp(tagcode,'LO01A1000')
                surf = daysdepth<=10;
            else
                surf = daysdepth<=5;
            end;
            if sum(surf)>=1
                daily_SST(dd,1) = nanmean(daysatemp(surf));
                daily_SST(dd,2) = nanmean(daysatemp(surf&day));
                daily_SST(dd,3) = nanmean(daysatemp(surf&~day));
            else
                daily_SST(dd,1:3)=NaN;
            end;

            %%%2-D histograms over depth and temperature
            for t = 1:length(bins_temp)
                for z = 1:length(bins_depth)
                    daily_histZTa(z,t,dd) = sum( daysdepth >= bins_depth(z)-zint & daysdepth < bins_depth(z)...
                        & daysatemp >= bins_temp(t)-tempint & daysatemp < bins_temp(t));
                    daily_histZTa_day(z,t,dd) = sum( daysdepth(day) >= bins_depth(z)-zint & daysdepth(day) < bins_depth(z)...
                        & daysatemp(day) >= bins_temp(t)-tempint & daysatemp(day) < bins_temp(t));
                    daily_histZTa_night(z,t,dd) = sum( daysdepth(~day) >= bins_depth(z)-zint & daysdepth(~day) < bins_depth(z)...
                        & daysatemp(~day) >= bins_temp(t)-tempint & daysatemp(~day) < bins_temp(t));
                end;
            end;
            waitbar(dd/length(TSdaysinreg));
        end;%%loop over days
        close(hh);

        %     figure;bars(bins_temp,sum(daily_atemp_hist)/sum(sum(daily_atemp_hist)));title([cell2mat(tagcode_interp(lotek(ta))) ' Ta']);
        %     figure;bars(bins_temp,sum(daily_btemp_hist)/sum(sum(daily_btemp_hist)));title([cell2mat(tagcode_interp(lotek(ta))) ' Tb']);
        %     figure;bars(bins_etemp,sum(daily_etemp_hist)/sum(sum(daily_etemp_hist)));title([cell2mat(tagcode_interp(lotek(ta))) ' Te']);
        %     figure;bars(bins_depth,sum(daily_depth_hist)/sum(sum(daily_depth_hist)));title([cell2mat(tagcode_interp(lotek(ta))) ' Depth']);

        figure;subplot(4,1,1);plot(daily_SST);
        subplot(4,1,2);plot(daily_bathy);
        subplot(4,1,3);plot(daily_atemp(:,1));
        subplot(4,1,4);plot(daily_btemp(:,4));

        eval(['save C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\Daily_summary\' tag '_summary daily* hv D* Ta Tb hv above_below daytime* dayindices* yrclass prof* tcline tgrad tcline_dates tclineflaggs yrs toppid tagcode tag date_at_dep series stock tagtype FGadkey sampint blocklines obs_per_day tint'])
        figure(figGoM);
        print('-djpeg','-r300',strcat('C:\Gareth\Tuna_data\ABFT\Canadian_fish\Time_series_data\Daily_summary\', tag ,'.jpg'));

        clear TS* GL* D* dates* daily* D* Ta Tb above_below daytime* dayindices* yrclass ages_TS lengths_TS bathy bathyreg day* glinreg hv prof* tcline tgrad temp_* yr* toppid tagcode tag date_at_dep series stock tagtype FGadkey sampint obs_per_day;
    end;    %end if TSofinterest>0 loop

    %     disp('Pausing...');
    %     pause;
end;    %loop over tags;
