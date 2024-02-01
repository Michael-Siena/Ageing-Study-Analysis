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

%% Initialise test data
nSubjects = 48;
nSessions = 1;
nBlocks = 8;
nTrialsPerBlock = 12;
nPairs = nBlocks * nTrialsPerBlock; % number of landmark-target object pairs in a test session
sectors = 1; % divide arena into quadrants 
nPairsPerSect = nPairs/sectors;
minDist = 6; % minimum distance allowed between landmarks and targets, to prevent unwanted clipping of objects

%% Generate coordinates for landmarks and targets
seedTargets = gencoords(arenaCenterX, arenaCenterY, sectors, arenaRadius, nPairs);

c = 1;
while c <= length(seedTargets)    
    % Compute pair distance
    lmCoord = [landmarks{c, :}];
    tCoord = [seedTargets{c, :}];
    dist = norm(lmCoord - tCoord);
    % Check against min dist
    if (dist < minDist)
        seedTargets = gencoords(arenaCenterX, arenaCenterY, sectors, arenaRadius, nPairs);
        c = 1;
        disp('FAILED : coord pair distance is < min allowed distance. Restarting!');
    else
        c = c + 1;
    end
end

disp('PASSED : all coord pair distances are >= min allowed distance');