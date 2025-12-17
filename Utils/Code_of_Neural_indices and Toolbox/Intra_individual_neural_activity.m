% Example code. 12/17/2025
% output:  standardized neural activity

function BOLD= Intra_individual_neural_activity(vector,baseline_time,mark_time)

% vector:  the  preprocessed neural data for analysis (e.g. the entire timeseries of neural activity in channel 11)

% baseline_time: the all time points used for the baseline
% e.g. all 24-round waiting phase 

% mark_time : time of interests (e.g. the start point and the end point of
% the decision-making phase in the 1st round,mark_time = [1 114])

 % Step1  calculate the mean and sd for the baseline. 
    data_baseline=vector(baseline_time);
    M_data=mean(data_baseline);
    SD_data=std(data_baseline);

% Step 2 calculate the standardized neural activity
    data = vector(mark_time(1):mark_time(2));
    BOLD=mean((data-M_data)/SD_data);
end