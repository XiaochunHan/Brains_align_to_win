function varargout=wtc(x,y,varargin)
%% Wavelet coherence
%----------Add by Yang Jiaxin--------------
sp=0.021*5;% sample interval 


% ------validate and reformat timeseries.
[x,dt]=formatts(x);
[y,dty]=formatts(y);
if dt~=dty
    error('timestep must be equal between time series')
end
t=(max(x(1,1),y(1,1)):dt:min(x(end,1),y(end,1)))'; %common time period


if length(t)<4
    error('The two time series must overlap.')
end



n=length(t);

%----------default arguments for the wavelet transform-----------
Args=struct('Pad',1,...      % pad the time series with zeroes (recommended)
            'Dj',1/12, ...    % this will do 12 sub-octaves per octave
            'S0',2*dt,...    % this says start at a scale of 2 years
            'J1',[],...
            'Mother','Morlet', ...
            'MaxScale',[],...   %a more simple way to specify J1
            'MakeFigure',(nargout==0),...
            'MonteCarloCount',300,...
            'BlackandWhite',0,...
            'AR1','auto',...
            'ArrowDensity',[30 30],...
            'ArrowSize',1,...
            'ArrowHeadSize',1);
Args=parseArgs(varargin,Args,{'BlackandWhite'});
if isempty(Args.J1)
    if isempty(Args.MaxScale)
        Args.MaxScale=(n*.17)*2*dt; %auto maxscale
    end
    Args.J1=round(log2(Args.MaxScale/Args.S0)/Args.Dj);
end

ad=mean(Args.ArrowDensity);
Args.ArrowSize=Args.ArrowSize*30*.03/ad;
%Args.ArrowHeadSize=Args.ArrowHeadSize*Args.ArrowSize*220;
Args.ArrowHeadSize=Args.ArrowHeadSize*120/ad;

if ~strcmpi(Args.Mother,'morlet')
    warning('WTC:InappropriateSmoothingOperator','Smoothing operator is designed for morlet wavelet.')
end

if strcmpi(Args.AR1,'auto')
    Args.AR1=[ar1nv(x(:,2)) ar1nv(y(:,2))];
    if any(isnan(Args.AR1))
        error('Automatic AR1 estimation failed. Specify it manually (use arcov or arburg).')
    end
end

nx=size(x,1);
%sigmax=std(x(:,2));

ny=size(y,1);
%sigmay=std(y(:,2));



%-----------:::::::::::::--------- ANALYZE ----------::::::::::::------------

[X,period,scale,coix] = wavelet(x(:,2),dt,Args.Pad,Args.Dj,Args.S0,Args.J1,Args.Mother);%#ok
[Y,period,scale,coiy] = wavelet(y(:,2),dt,Args.Pad,Args.Dj,Args.S0,Args.J1,Args.Mother);

%Smooth X and Y before truncating!  (minimize coi)
sinv=1./(scale');


sX=smoothwavelet(sinv(:,ones(1,nx)).*(abs(X).^2),dt,period,Args.Dj,scale);
sY=smoothwavelet(sinv(:,ones(1,ny)).*(abs(Y).^2),dt,period,Args.Dj,scale);


% truncate X,Y to common time interval (this is first done here so that the coi is minimized)
dte=dt*.01; %to cricumvent round off errors with fractional timesteps
idx=find((x(:,1)>=(t(1)-dte))&(x(:,1)<=(t(end)+dte)));
X=X(:,idx);
sX=sX(:,idx);
coix=coix(idx);

idx=find((y(:,1)>=(t(1))-dte)&(y(:,1)<=(t(end)+dte)));
Y=Y(:,idx);
sY=sY(:,idx);
coiy=coiy(idx);

coi=min(coix,coiy);

% -------- Cross wavelet -------
Wxy=X.*conj(Y);

% ----------------------- Wavelet coherence ---------------------------------
sWxy=smoothwavelet(sinv(:,ones(1,n)).*Wxy,dt,period,Args.Dj,scale);
Rsq=abs(sWxy).^2./(sX.*sY);

if (nargout>0)||(Args.MakeFigure)
    wtcsig=wtcsignif(Args.MonteCarloCount,Args.AR1,dt,length(t)*2,Args.Pad,Args.Dj,Args.S0,Args.J1,Args.Mother,.6);
    wtcsig=(wtcsig(:,2))*(ones(1,n));
    wtcsig=Rsq./wtcsig;
end

if Args.MakeFigure
    

    Yticks = 2.^(fix(log2(min(period))):fix(log2(max(period))));
    
    if Args.BlackandWhite
        levels = [0 0.5 0.7 0.8 0.9 1];
        [cout,H]=safecontourf(t,log2(period),Rsq,levels);

        colorbarf(cout,H)
        cmap=[0 1;.5 .9;.8 .8;.9 .6;1 .5];
        cmap=interp1(cmap(:,1),cmap(:,2),(0:.1:1)');
        cmap=cmap(:,[1 1 1]);
        colormap(cmap)
        set(gca,'YLim',log2([min(period),max(period)]), ...
            'YDir','reverse', 'layer','top', ...
            'YTick',log2(Yticks(:)), ...
            'YTickLabel',num2str(Yticks'), ...
            'layer','top')
        ylabel('Period')
        hold on

        %phase plot
        aWxy=angle(Wxy);
        aaa=aWxy;
        aaa(Rsq<.5)=NaN;
        %[xx,yy]=meshgrid(t(1:5:end),log2(period));

        phs_dt=round(length(t)/Args.ArrowDensity(1)); tidx=max(floor(phs_dt/2),1):phs_dt:length(t);
        phs_dp=round(length(period)/Args.ArrowDensity(2)); pidx=max(floor(phs_dp/2),1):phs_dp:length(period);
        phaseplot(t(tidx),log2(period(pidx)),aaa(pidx,tidx),Args.ArrowSize,Args.ArrowHeadSize);

        if ~all(isnan(wtcsig))
            [c,h] = contour(t,log2(period),wtcsig,[1 1],'k');%#ok
            set(h,'linewidth',2)
        end
        %suptitle([sTitle ' coherence']);
        plot(t,log2(coi),'k','linewidth',3)
        hold off
    else
        % the figure is colorful      
        t=t*sp;% added by Yang Jiaxin 2018/03/17
        H=imagesc(t,log2(period),Rsq);%#ok
        %[c,H]=safecontourf(t,log2(period),Rsq,[0:.05:1]);
        %set(H,'linestyle','none')
        
        set(gca,'clim',[0 1])
        
        HCB=colorbar;%#ok
 %Modified by Yang Jiaxin
 % period one showed below: 
%         
%         set(gca,'YLim',log2([min(period),max(period)]), ...
%             'YDir','reverse', 'layer','top', ...
%             'YTick',log2(Yticks(:)), ...
%             'YTickLabel',num2str(Yticks'), ...
%             'layer','top')

       yy=num2str(Yticks'*sp); %Jiaxin Modified _ 0.018 =sample interval
        set(gca,'YLim',log2([min(period),max(period)]), ...
            'YDir','reverse', 'layer','top', ...
            'YTick',log2(Yticks(:)), ...
            'YTickLabel',yy, ...%Jiaxin Modified 'YTickLabel' 
            'layer','top')
        ylabel('Period(s)')%Modifed by Jiaxin
        xlabel('Time(s)')%Added by Jiaxin
        hold on

        %phase plot
        aWxy=angle(Wxy);
        aaa=aWxy;
        aaa(Rsq<.5)=NaN; %remove phase indication where Rsq is low
        %[xx,yy]=meshgrid(t(1:5:end),log2(period));

        phs_dt=round(length(t)/Args.ArrowDensity(1)); tidx=max(floor(phs_dt/2),1):phs_dt:length(t);
        phs_dp=round(length(period)/Args.ArrowDensity(2)); pidx=max(floor(phs_dp/2),1):phs_dp:length(period);
        phaseplot(t(tidx),log2(period(pidx)),aaa(pidx,tidx),Args.ArrowSize,Args.ArrowHeadSize);

        if ~all(isnan(wtcsig))
            [c,h] = contour(t,log2(period),wtcsig,[1 1],'k');%#ok
            set(h,'linewidth',2)
        end
        %suptitle([sTitle ' coherence']);
        tt=[t([1 1])-dt*.5;t;t([end end])+dt*.5];
        hcoi=fill(tt,log2([period([end 1]) coi period([1 end])]),'w');
        set(hcoi,'alphadatamapping','direct','facealpha',.5)
        hold off
    end
end

varargout={Rsq,period,scale,coi,wtcsig};
varargout=varargout(1:nargout);






function [cout,H]=safecontourf(varargin)
vv=sscanf(version,'%i.');
if (version('-release')<14)|(vv(1)<7)
    [cout,H]=contourf(varargin{:});
else
    [cout,H]=contourf('v6',varargin{:});
end

function hcb=safecolorbar(varargin)
vv=sscanf(version,'%i.');

if (version('-release')<14)|(vv(1)<7)
    hcb=colorbar(varargin{:});
else
    hcb=colorbar('v6',varargin{:});
end


function [wave,period,scale,coi] = ...
	wavelet(Y,dt,pad,dj,s0,J1,mother,param);

if (nargin < 8), param = -1;, end
if (nargin < 7), mother = -1;, end
if (nargin < 6), J1 = -1;, end
if (nargin < 5), s0 = -1;, end
if (nargin < 4), dj = -1;, end
if (nargin < 3), pad = 0;, end
if (nargin < 2)
	error('Must input a vector Y and sampling time DT')
end

n1 = length(Y);

if (s0 == -1), s0=2*dt;, end
if (dj == -1), dj = 1./4.;, end
if (J1 == -1), J1=fix((log(n1*dt/s0)/log(2))/dj);, end
if (mother == -1), mother = 'MORLET';, end

%....construct time series to analyze, pad if necessary
x(1:n1) = Y - mean(Y);
if (pad == 1)
	base2 = fix(log(n1)/log(2) + 0.4999);   % power of 2 nearest to N
	x = [x,zeros(1,2^(base2+1)-n1)];
end
n = length(x);

%....construct wavenumber array used in transform [Eqn(5)]
k = [1:fix(n/2)];
k = k.*((2.*pi)/(n*dt));
k = [0., k, -k(fix((n-1)/2):-1:1)];

%....compute FFT of the (padded) time series
f = fft(x);    % [Eqn(3)]

%....construct SCALE array & empty PERIOD & WAVE arrays
scale = s0*2.^((0:J1)*dj);
period = scale;
wave = zeros(J1+1,n);  % define the wavelet array
wave = wave + i*wave;  % make it complex

% loop through all scales and compute transform
for a1 = 1:J1+1
	[daughter,fourier_factor,coi,dofmin]=wave_bases(mother,k,scale(a1),param);	
	wave(a1,:) = ifft(f.*daughter);  % wavelet transform[Eqn(4)]
end

period = fourier_factor*scale;
coi = coi*dt*[1E-5,1:((n1+1)/2-1),fliplr((1:(n1/2-1))),1E-5];  % COI [Sec.3g]
wave = wave(:,1:n1);  % get rid of padding before returning

return


function swave=smoothwavelet(wave,dt,period,dj,scale)
% Smoothing as in the appendix of Torrence and Webster "Inter decadal changes in the ENSO-Monsoon System" 1998
% 
% used in wavelet coherence calculations
% 
%
% Only applicable for the Morlet wavelet. 
%
% (C) Aslak Grinsted 2002-2005
%

% -------------------------------------------------------------------------
%   Copyright (C) 2002-2005, Aslak Grinsted
%   This software may be used, copied, or redistributed as long as it is not
%   sold and this copyright notice is reproduced on each copy made.  This
%   routine is provided as is without any express or implied warranties
%   whatsoever.

% TODO: take mother argument
%


n=size(wave,2);

%swave=zeros(size(wave));
twave=zeros(size(wave));

% %filter in time:.... 
% for i=1:size(wave,1)
%     sc=period(i)/dt; % time/cycle / time/sample = samples/cycle
%     t=(-round(sc*3):round(sc*3))*dt;
%     f=exp(-t.^2/(2*scale(i)^2));
%     f=f/sum(f); %filter must have unit weight
%     
%     smooth=conv(wave(i,:),f); %slowest line of code in the wtcsig calculation. should be done like in wavelet with fft and ifft.
%     cutlen=(length(t)-1)*.5;
%     twave(i,:)=smooth((cutlen+1):(end-cutlen)); %remove paddings
% end
% 
%filter in time:....
% 
% qwave=twave;

%zero-pad to power of 2... Speeds up fft calcs if n is large
npad=2.^ceil(log2(n));

k = 1:fix(npad/2);
k = k.*((2.*pi)/npad);
k = [0., k, -k(fix((npad-1)/2):-1:1)];

k2=k.^2;
snorm=scale./dt;
for ii=1:size(wave,1)
    F=exp(-.5*(snorm(ii)^2)*k2); %Thanks to Bing Si for finding a bug here.
    smooth=ifft(F.*fft(wave(ii,:),npad));
    twave(ii,:)=smooth(1:n);
end

if isreal(wave)
    twave=real(twave); %-------hack-----------
end

%scale smoothing (boxcar with width of .6)

%
% TODO: optimize. Because this is done many times in the monte carlo run.
%


dj0=0.6;
dj0steps=dj0/(dj*2);
% for ii=1:size(twave,1)
%     number=0;
%     for l=1:size(twave,1);
%         if ((abs(ii-l)+.5)<=dj0steps)
%             number=number+1;
%             swave(ii,:)=swave(ii,:)+twave(l,:);
%         elseif ((abs(ii-l)+.5)<=(dj0steps+1))
%             fraction=mod(dj0steps,1);
%             number=number+fraction;
%             swave(ii,:)=swave(ii,:)+twave(l,:)*fraction;
%         end
%     end
%     swave(ii,:)=swave(ii,:)/number;
% end

kernel=[mod(dj0steps,1); ones(2 * round(dj0steps)-1,1); ...
   mod(dj0steps,1)]./(2*round(dj0steps)-1+2*mod(dj0steps,1));
swave=conv2(twave,kernel,'same'); %thanks for optimization by Uwe Graichen 

%swave=twave;

