% Example code. 12/17/2025
% output: WTC coherence value
function INS = interbrain_neural_coupling(vector1,vector2, fs,marktime,foi)

% vector1 and vector2 should be two vectors of the same length (e.g. the
% e.g. preprocessed neural data in channel 1 of group leader and one follower
% over the entire timeseries during the task.

% fs- sampling frequency

% marktime: time of interests (e.g. the start point and the end point of
% the decision-making phase in the 1st round,marktime = [1 114])

% foi: the frequency band of interests(e.g., foi = [6,38]%the 6-38s based on our experiment design)


%Using the MATLAB function "wtc.m" in the wtc-r16 Toolbox
[wcoh,period] = wtc(vector1, vector2,'mf',0,'mcc',0);


period_min  =max(find (period*fs<foi(1)))+1;
period_max  = max(find (period*fs<foi(2)));

  INS = mean(mean(wcoh(period_min:period_max, marktime(1): marktime(2))));
end