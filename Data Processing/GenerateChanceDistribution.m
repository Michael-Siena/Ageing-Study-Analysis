%% Prepare MATLAB environment
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirActive.Filename)); % assign active dir to cd
addpath(genpath('./Functions/')); % adds path for Functions directory

%% Initialise arena
arenaCenterX = 0;
arenaCenterY = 0;
arenaDiameter = 50;
arenaCircumfrence = arenaDiameter*pi;
arenaRadius = arenaCircumfrence/(2 * pi);

%% Check if data already generated
directory = 'ChanceDistribution/';
if ~exist(directory, 'dir')
    mkdir(directory);
else
    disp('Directory already exists - checking for conflicts with existing data files');
    if exist([directory 'ChanceDistribution.csv'], 'file')
        disp('Chance distribution already exists - terminating session!');
        return;
    end
end

%% Generate possible response coordinates for chance distribution
nPossibleResponses = 10000; 
chanceDistribution = gencoords(arenaCenterX, arenaCenterY, 1, arenaRadius, nPossibleResponses);

%% Write chance distribution to directory %%
columnNames = [{'ChanceDistX'}, {'ChanceDistZ'}];
chanceDistributionTable = cell2table(chanceDistribution, 'VariableNames', columnNames);
writetable(chanceDistributionTable, [directory 'ChanceDistribution.csv']);