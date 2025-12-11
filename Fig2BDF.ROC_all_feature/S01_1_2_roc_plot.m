clear;clc;close all;
addpath(genpath('/Users/xiaochunhan/Documents/MATLAB/CanlabCore'));
datafile = 'Invest_auc_data.csv';
rocdata = readtable(datafile);
mycolors={[0,0,0]/255 [175,149,198]/255 [150,150,150]/255}; 
%%
figure;
roc_plot(rocdata.y_fit_all, rocdata.y_real_all,'color',mycolors{1});
set(gca, 'FontSize', 18, 'LineWidth', 2); box('off');
set(gcf,'color',[1 1 1]);
export_fig('svm_roc_study1_all.png')
%%
figure;
roc_plot(rocdata.y_fit_INS, rocdata.y_real_INS,'color',mycolors{2});
set(gca, 'FontSize', 18, 'LineWidth', 2); box('off');
set(gcf,'color',[1 1 1]);
export_fig('svm_roc_study1_WNS_BNS.png')
%%
figure;
roc_plot(rocdata.y_fit_BOLD, rocdata.y_real_BOLD,'color',mycolors{3});
set(gca, 'FontSize', 18, 'LineWidth', 2); box('off');
set(gcf,'color',[1 1 1]);
export_fig('svm_roc_study1_BOLD_FC.png')
