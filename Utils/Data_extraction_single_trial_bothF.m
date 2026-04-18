clear;clc;
basedir = '/Users/xiaochunhan/Documents/Research/Projects/Brains_align_to_win/Data/Develop';
metafile = 'Real_WINS_BINS_FC_BOLD_bothF.mat';
reffile = 'BINS_ind_data.mat';
%%
load(fullfile(basedir, reffile));
title = BINS_data.title([1:3,5:31]);
clear BINS_data
%%
load(fullfile(basedir, metafile));

new_lead_column = repmat([1;2;3], 840, 1);

bins = array2table(BINS_data, 'VariableNames', title);
bins.("lead(L/LF=1)") = new_lead_column;

ins = array2table(INS_data, 'VariableNames', title);
ins.("lead(L/LF=1)") = new_lead_column;

writetable(bins, fullfile(basedir,'BINS_single_trial_invest_bothF.csv'),"Delimiter",",");
writetable(ins, fullfile(basedir,'INS_single_trial_invest_bothF.csv'),"Delimiter",",");

fprintf('Done!\n');