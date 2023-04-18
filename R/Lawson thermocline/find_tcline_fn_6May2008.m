function [tcline tcline_date_vec flaggs tgrad] = find_tcline_fn_6May2008(Date,D,Ta,tclinethresh,tint,blocklines,constsmooth,constsmoothpara,smoothinterval,maxbott,surfdefn,pausetoplot,ploteveryndays);

%%Uses Kara et al 2000 definition to identify isothermal layer depth. Also
%%calculates a few other water column descriptors (gradient mostly).
%%Input parameters are:
%%Date: A vector of times in matlab date format
%%D: A vector of depths
%%Ta: A vector of temperatures (Date,D,Ta must all be the same size)
%%TSdaysinreg: A vector of the days to be analyzed in matlab date format
% % tclinethresh: Temperature difference criterion for identifying isothermal layer
% % tint: Number of time intervals per day for extracting isothermal layer
% depth (ILD)
% % constsmooth: contsmooth=1 uses the constant smoothing parameter constsmoothpara for generating representative profile from raw temp-depth data
% % constsmoothpara: Block lab default is 0.6
% % smoothinterval: contsmooth=0 smooths over intervals of smoothinterval meters.
% % maxbott: Ignores constant depth regions deeper than this depth. Set to 1010 to not use this feature. Must be a multiple of 10. Without having this the algorithms sometimes identify regions of 'uniform' temperature (ie variations of smaller than tclinethres/10) at very large depths.
% % surfdefn: depth that defines the 'surface'
% % pausetoplot: 1 pauses every ploteveryndays to plot the data and ILD estimate, 0 doesn't
% %
%%Parameters GL uses:
% tclinethresh = .8;  %Temperature difference criterion for identifying isothermal layer
% tint = 4;   %Number of time intervals per day for extracting isothermal layer depth (ILD)
% constsmooth = 0;    %contsmooth=1 uses the constant smoothing parameter constsmoothpara for generating representative profile from raw temp-depth data
% constsmoothpara = 0.6;
% smoothinterval = 50;%contsmooth=0 smooths over intervals of smoothinterval meters.
% maxbott = 50;    %Ignores constant depth regions deeper than this depth. Set to 1010 to not use this feature. Must be a multiple of 10. Without having this the algorithms sometimes identify regions of 'uniform' temperature (ie variations of smaller than tclinethres/10) at very large depths.
% surfdefn = 3;

%%Result is a vector of ILD estimates named tcline which has as many
%%elements as the number of days times the number of times per day the ILD
%%is identified. The shallowest the ILD can be is 10m. If no ILD was found
%%within the sampled depth range (ie the ILD is deeper than the max depth
%%the fish visited) a value of 9999 is returned.

warning off;

days =unique(fix(Date));
numdays = length(days);

if pausetoplot;figZts=figure;end;

flaggs = zeros(numdays*tint,3);

hh=waitbar(0,'Finding MLD...');
for dd = 1:1:numdays
    for pe = 1:tint
        currpt = (dd-1)*tint + pe;  %%keep track of where we are in the resulting tcline vector
        tcline_date_vec(currpt,1) = days(dd)+(pe-1)*(1/tint);

        %get data for time period of interest
        wantedrows = find(Date>=days(dd)+(pe-1)*(1/tint) & Date<days(dd)+(pe)*(1/tint));

        if length(wantedrows)>blocklines/4 % blocklines is how many rows per interval, why divide by 4?
                                           % blocklines is fixed depending on sample rate but should be large
                                           % say 1000, so 1000/4 = 250, you're not going to process the chunk
                                           % unless theres > 250 rows of data?

            daydepth = D(wantedrows);
            dayatemp = Ta(wantedrows);
            timeax = (Date(wantedrows)-fix(Date(wantedrows)))*24;

            %%%%%%%%For comparison find ILD via absolute del-T criterion (Weng-style)%%%%%%%%%%%%%%%%%%%%%%
            depth_vec_1 = 1:1:max(fix(max(daydepth)),1);
            temp_vec_1 = csloess_db(daydepth,dayatemp,depth_vec_1,.6,1);%Smooth the temperature-depth data to get a representative profile
            tcline_const=find(abs(temp_vec_1-nanmedian(temp_vec_1(1:min(surfdefn,length(temp_vec_1)))))>tclinethresh);
            if ~isempty(tcline_const)
                tcline(currpt,2)=tcline_const(1);
            else
                tcline(currpt,2)=9999;
            end;

            %%%%%%%%Now find ILD via Kara et al. walking method%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if max(daydepth)<=10
                tcline(currpt,1) = 9999;    %Kara method sets initial Tref to T at 10m, so ILD must be >10m. If fish didn't go deeper than 10m, set ILD to 9999 (value used to denote cases where ILD was deeper than fish's max depth).
                tcline(currpt,4:5) = NaN;
                tgrad(currpt,1:11)=NaN;
            else
                depth_vec = 0:10:fix(max(daydepth))';   %Depth vector used for finding uniform temp region and Tref, which ends at last multiple of 10 befor max depth visited
                depth_vec_f = [depth_vec,max(daydepth)];    %Depth vector used for finding ILD, which ends at max depth visited
                if constsmooth
                    smoothpara = constsmoothpara;
                else
                    smoothpara = min(smoothinterval/max(daydepth),constsmoothpara);  %Spline will be fit over an interval of the data given by smoothinterval/max depth
                end;
                temp_vec_f = csloess_db(daydepth,dayatemp,depth_vec_f,smoothpara,1)';%Smooth the temperature-depth data to get a representative profile, in 10m intervals (used by Kara in developing algorithms)
                temp_vec = temp_vec_f(1:end-1);
                if sum(isnan(temp_vec_f))>0 %If there are gaps in the temp-depth data of larger than smoothinterval meters the spline will return NaNs, which can mess up the ILD estimation. If NaNs are returned, the constant smoothing parameter constsmoothpara is used instead.
                    temp_vec_f = csloess_db(daydepth,dayatemp,depth_vec_f,constsmoothpara,1)';
                    temp_vec = temp_vec_f(1:end-1);
                end;
                if sum(isnan(temp_vec_f))>0 %If there are still NaNs in the spline output, interpolate
                    bdsplii = isnan(temp_vec_f);
                    depth_vec_t=depth_vec_f;
                    if depth_vec_t(end)==depth_vec_t(end-1)
                        depth_vec_t(end)=depth_vec_t(end)+.01;
                    end;
                    if sum(~bdsplii)>1
                    temp_vec_f(bdsplii) = interp1(depth_vec_t(~bdsplii),temp_vec_f(~bdsplii),depth_vec_f(bdsplii));
                    end;
                    temp_vec = temp_vec_f(1:end-1);
                    clear depth_vec_t;
                end;

                %Identify  uniform temperature region, if any, and set Tref to temp at its
                %bottom
                dift = diff(temp_vec(2:end));dift = [NaN;NaN;dift];
                bott = find(abs(dift)<tclinethresh/10); %uniform temperature is defined by variations between successive depths smaller than tclinethresh/10
                bott_tem=bott;
                if isempty(bott) | bott(1)>maxbott/10
                    if length(temp_vec)==1
                        bott = 1;
                    else
                        bott=2;
                    end;
                    Tref = temp_vec(bott);%If no uniform temp region, Tref is T at 10m
                elseif length(bott)==1
                    Tref = temp_vec(bott);
                else
                    difbott = diff(bott);
                    brk = find(difbott>1);
                    if ~isempty(brk)
                        bott = bott(brk(1));
                        Tref = temp_vec(bott);%Tref is T at depth at base of first uniform temp region
                    else
                        bott = bott(end);
                        Tref = temp_vec(bott);
                    end;
                end;

                %Find depth at which temperature changes by more than tclinethresh relative
                %to Tref
                delt = temp_vec_f - Tref;
                delt(1:bott) = NaN;
                hnext = find(abs(delt)>tclinethresh);

                if isempty(hnext)
                    tcline(currpt,1) = 9999;  %%Returns this value if the deepest temperature measurement is within tclinethresh of the uniform temperature layer (ie if the ILD is deeper than the max measured depth)
                    tcline(currpt,4:5) = NaN;
                else
                    hnext = hnext(1);
                    hn=hnext-1;
                    if temp_vec_f(hnext) > Tref
                        Tbase = Tref + tclinethresh;
                        flaggs(currpt,1) = 1;   %Flags cases where temperature increases with depth below ILD
                    else
                        Tbase = Tref - tclinethresh;
                    end;
                    tcline(currpt,1) = interp1([temp_vec_f(hn) temp_vec_f(hnext)],[depth_vec_f(hn) depth_vec_f(hnext)],Tbase);
                    tcline(currpt,4) = Tbase;
                    tcline(currpt,5) = nanmean(temp_vec_1(1:min(length(temp_vec_1),round(tcline(currpt,1)))));
                    flaggs(currpt,2) = Tbase > temp_vec(2); %Flags cases where temperature increases from 10 m to ILD.
                end;

                %%%Calculate some gradients
                if tcline(currpt,1)~=9999 & round(tcline(currpt,1))<length(temp_vec_1)
                    diff1 = diff(temp_vec_1(round(tcline(currpt,1)):end));
                    difz = find(abs(diff1)==max(abs(diff1)));difz=difz(1);
                    tgrad(currpt,1:2) = [diff1(difz),difz+round(tcline(currpt,1))-1];%%maximum temp gradient below ILD and its depth

                    %%Find del-T and associated depths for max and min
                    %%temperature diffs rel to IL temperature
                    shalt = nanmean(temp_vec_1(1:min(length(temp_vec_1),round(tcline(currpt,1)))));
                    deept = temp_vec_1(round(tcline(currpt,1))+1:end)-shalt;
                    deeptpos = deept;deeptpos(deeptpos<0)=NaN;
                    deepstpos = find(deeptpos==max(deeptpos));
                    deeptneg = deept;deeptneg(deeptneg>=0)=NaN;
                    deepstneg = find(deeptneg==min(deeptneg));
                    if ~isempty(deepstneg)
                        tgrad(currpt,3:4)=[deeptneg(deepstneg(1)),deepstneg(1)];
                    else
                        tgrad(currpt,3:4)=NaN;
                    end;
                    if ~isempty(deepstpos)
                        tgrad(currpt,5:6)=[deeptpos(deepstpos(1)),deepstpos(1)];
                    else
                        tgrad(currpt,5:6)=NaN;
                    end;
                    
                    %%Find next most different temp and depth rel to most
                    %%different temp (use to ID inversions)
                    if ~isempty(deepstpos) & ~isempty(deepstneg)
                        if deepstneg(1)<deepstpos(1)
                            tgrad(currpt,10:11)=[deeptpos(deepstpos(1)),deepstpos(1)];
                        else
                            tgrad(currpt,10:11)=[deeptneg(deepstneg(1)),deepstneg(1)];
                        end;
                    elseif ~isempty(deepstpos)& deepstpos<length(deept)
                        nxtmin = find(deept(deepstpos(1)+1:end)==min(deept(deepstpos(1)+1:end)));
                        tgrad(currpt,10:11) = [deept(nxtmin(1)+deepstpos(1)),nxtmin(1)+deepstpos(1)];
                    elseif ~isempty(deepstneg) & deepstneg<length(deept)
                        nxtmax = find(deept(deepstneg(1)+1:end)==max(deept(deepstneg(1)+1:end)));
                        tgrad(currpt,10:11) = [deept(nxtmax(1)+deepstneg(1)),nxtmax(1)+deepstneg(1)];
                    else                       
                        tgrad(currpt,10:11)=NaN;
                    end;
               
                    clear deept* deepst* shalt diff1 diff2 nxtmin nxtmax
                else
                    tgrad(currpt,[1:6,10:11])=NaN;
                end;

                if length(temp_vec)>10
                    tgrad(currpt,7) = temp_vec(2) - temp_vec(11);   %Temperature difference 10 vs 100m.
                else
                    tgrad(currpt,7) = NaN;
                end;

                if length(temp_vec)>1
                    temdifs=diff(temp_vec);
                    tgrad(currpt,8:9)=[min(temdifs),max(temdifs)]/10;%min and max temp diff over 10m intervals
                else
                    tgrad(currpt,8:9)=NaN;
                end;
                
                
                bdspline = find(temp_vec_1<=0);
                if ~isempty(bdspline)
                    flaggs(currpt,3)=1;
                end;

                if pausetoplot & rem(dd,ploteveryndays)==0 & pe==1
                    %Make some plots to check that everything is working ok.
                    figure(figZts);Tts=subplot('position',[.07 .07 .5 .15]);plot(timeax,dayatemp,'.-');hold on;
                    set(gca,'xlim',[(pe-1)*(24/tint) (pe)*(24/tint)]);

                    Z2ts=subplot('position',[.07 .28 .5 .65]);plot(timeax,daydepth,'.-');set(gca,'ydir','rev');hold on;set(gca,'xlim',[0 24]);
                    deplims = get(gca,'ylim');
                    plot(timeax,daydepth,'.-');set(gca,'ydir','rev');hold on;set(gca,'xlim',[(pe-1)*(24/tint) (pe)*(24/tint)]);
                    title(datestr(days(dd)+(pe-1)*(1/tint)));

                    bt=subplot('position',[.63 .28 .33 .65]);
                    plot(dayatemp,daydepth,'.');hold on;
                    set(gca,'ydir','rev');

                    plot(temp_vec_f,depth_vec_f,'ko-','markerfacecolor','g','linewidth',1.5);
                    plot(temp_vec_f(bott),depth_vec_f(bott),'ko','markerfacecolor','r');
                    temlims = get(gca,'xlim');

                    if ~isempty(tcline_const)
                        plot(temlims,[tcline_const(1) tcline_const(1)],'r','linewidth',2);
                    end;
                    if tcline(currpt,1) == 9999
                        plot(temlims,[max(daydepth) max(daydepth)],'k--','linewidth',2);
                    else
                        plot(temlims,[tcline(currpt) tcline(currpt)],'g','linewidth',2);
                        if ~isnan(tgrad(currpt,4))
                            plot(temp_vec_1(min(length(temp_vec_1),round(tgrad(currpt,4)+tcline(currpt,1)))),depth_vec_1(min(length(temp_vec_1),round(tgrad(currpt,4)+tcline(currpt,1)))),'cx','linewidth',3,'markersize',15);
                        end;
                        if ~isnan(tgrad(currpt,6))
                            plot(temp_vec_1(min(length(temp_vec_1),round(tgrad(currpt,6)+tcline(currpt,1)))),depth_vec_1(min(length(temp_vec_1),round(tgrad(currpt,6)+tcline(currpt,1)))),'cx','linewidth',3,'markersize',15);
                        end;
                    end;
                    set(gca,'xlim',temlims);
                    
                    if ~isnan(tgrad(currpt,2))
                        plot(temp_vec_1(round(tgrad(currpt,2))),depth_vec_1(round(tgrad(currpt,2))),'mx','linewidth',3,'markersize',15);
                    end;
                    tgrad(currpt,:);
                    pause;clf;
                end;    %end if pausetoplot loop                
            end;    %end if maxdepth<10m loop


            tcline(currpt,3) = max(daydepth);
        else
            tcline(currpt,1:5)=NaN;
            tgrad(currpt,1:11)=NaN;
            flaggs(currpt,1:3)=NaN;
        end;    %if wantedepth of sufficient size...

    end;    %end loop over within-day intervals

    waitbar(dd/numdays);
end;        %end loop over days
close(hh);



