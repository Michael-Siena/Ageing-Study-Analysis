%% This script scores the raw data from the spatial memory task

%% CLEAN MATLAB ENVIRONMENT
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirAc/tive.Filename)); % assign active dir to cd

%% READ IN DATA AND INITIALISE
ChanceDistribution = readtable('ChanceDistribution/ChanceDistribution10k.csv');
[nChanceCoords, ~] = size(ChanceDistribution);

value = jsonencode(fileread('YA_Pilot/SID10.json'));
trialData = split(extractBetween(value, ": \""", """"), ",");
varNames = {'SID' 'Session' 'Block' 'Trial' 'StudiedPerspective' 'TestPerspective' 'SwitchStatus' 'TargetName' 'TargetX' 'TargetZ' 'xPlacement' 'zPlacement' 'PlacementDistance' 'ResponseTime' 'DateTime'};
ResponseData = cell2table(trialData, 'VariableNames', varNames);

ResponseData.Block = str2double(ResponseData.Block);

ResponseData.StudiedPerspective = str2double(ResponseData.StudiedPerspective);
ResponseData.SwitchStatus = str2double(ResponseData.SwitchStatus);

ResponseData.TargetX = str2double(ResponseData.TargetX);
ResponseData.TargetZ = str2double(ResponseData.TargetZ);
ResponseData.PlacementDistance = str2double(ResponseData.PlacementDistance);
ResponseData.PlacementDistance(find(ResponseData.PlacementDistance == 999)) = NaN; % replace 999 with NaN to indicate timed out trials

ResponseData.xPlacement = str2double(ResponseData.xPlacement);
ResponseData.zPlacement = str2double(ResponseData.zPlacement);

ResponseData.ResponseTime = str2double(ResponseData.ResponseTime);
ResponseData.ResponseTime(find(ResponseData.ResponseTime == 999)) = NaN; % replace 999 with NaN to indicate timed out trials

ResponseData = ResponseData(ResponseData.Block ~= 0, :); % main task only - excludes practice block
[nTargetCoords, ~] = size(ResponseData);

nTimeouts = sum(isnan(ResponseData.ResponseTime));
disp(['% timeouts: ' num2str(nTimeouts/height(ResponseData))]);

aStayGuessCount = 0;
aSwitchGuessCount = 0;
eStayGuessCount = 0;
eSwitchGuessCount = 0;

%% SCORING
results = cell(nTargetCoords, 2);
tmpChanceDistances = cell(nChanceCoords, 1);
for targetCoord = 1:nTargetCoords
    %% Allocate Responses to Appropriate Conditions
    % Ego-Stay
    if(str2double(ResponseData.TestPerspective{targetCoord}) == 1 && ResponseData.SwitchStatus(targetCoord) == 2)
        results{targetCoord, 1} = "eStay"; 
    % Ego-Switch
    elseif(str2double(ResponseData.TestPerspective{targetCoord}) == 1 && ResponseData.SwitchStatus(targetCoord) == 1)
        results{targetCoord, 1} = "eSwitch";
    % Allo-Stay
    elseif(str2double(ResponseData.TestPerspective{targetCoord}) == 2 && ResponseData.SwitchStatus(targetCoord) == 2)
        results{targetCoord, 1} = "aStay";
    % Allo-Switch
    elseif(str2double(ResponseData.TestPerspective{targetCoord}) == 2 && ResponseData.SwitchStatus(targetCoord) == 1)
        results{targetCoord, 1} = "aSwitch";
    end
    
    %% Compute memory score
    % first check whether trial is invalid and skip if true
    if ((ResponseData.xPlacement(targetCoord) == 0 && ResponseData.zPlacement(targetCoord) == 0) ...
        || ResponseData.ResponseTime(targetCoord) < .5 ...
        || isnan(ResponseData.ResponseTime(targetCoord))) 

        disp("Guess @ block " + ResponseData.Block(targetCoord) + ", trial " + ResponseData.Trial(targetCoord) + ", condition " + results{targetCoord, 1});

        if (results{targetCoord, 1} == "eStay")
            eStayGuessCount = eStayGuessCount + 1;
        elseif (results{targetCoord, 1} == "eSwitch")
            eSwitchGuessCount = eSwitchGuessCount + 1;
        elseif (results{targetCoord, 1} == "aStay")
            aStayGuessCount = aStayGuessCount + 1;
        elseif (results{targetCoord, 1} == "aSwitch")
            aSwitchGuessCount = aSwitchGuessCount + 1;
        end

        results{targetCoord, 2} = NaN; % is effectively 0 in the analysis

        continue
    end
    
    % for valid trials, find accuracy percentile (i.e., memory score) for placement distance relative to chance distribution
    nLessThanPlacementDist = 0;
    tmpChanceDistances = cell(nChanceCoords);
    for chanceCoord = 1:nChanceCoords
        tmpChanceDistances{chanceCoord} = EuclideanDistance(ChanceDistribution.ChanceDistX(chanceCoord), ChanceDistribution.ChanceDistZ(chanceCoord),... 
                                                            ResponseData.TargetX(targetCoord), ResponseData.TargetZ(targetCoord));
    
        if (tmpChanceDistances{chanceCoord} < ResponseData.PlacementDistance(targetCoord))                                                
            nLessThanPlacementDist = nLessThanPlacementDist + 1;
        end 
    end
  
    normalisedError = (nLessThanPlacementDist) / nChanceCoords;
    memoryScore = 1 - normalisedError;
    
    results{targetCoord, 2} = memoryScore;
end

nTrialPerCond = length(results) / 4;

eStayResps = [results{matches([results{:, 1}], "eStay"), 2}];
eSwitchResps = [results{matches([results{:, 1}], "eSwitch"), 2}];
aStayResps = [results{matches([results{:, 1}], "aStay"), 2}];
aSwitchResps = [results{matches([results{:, 1}], "aSwitch"), 2}];

%% OUTPUT
%% Mean Placement Distance (PD)
eStayPD = mean(ResponseData.PlacementDistance(matches([results{:, 1}], "eStay") & ~isnan([results{:, 2}])), 'omitnan');
eSwitchPD = mean(ResponseData.PlacementDistance(matches([results{:, 1}], "eSwitch") & ~isnan([results{:, 2}])), 'omitnan');
aStayPD = mean(ResponseData.PlacementDistance(matches([results{:, 1}], "aStay") & ~isnan([results{:, 2}])), 'omitnan');
aSwitchPD = mean(ResponseData.PlacementDistance(matches([results{:, 1}], "aSwitch") & ~isnan([results{:, 2}])), 'omitnan');

disp("Placement Distance:");
disp(['eStay:' num2str(eStayPD) ' | eSwitch: ' num2str(eSwitchPD) ' | aStay: ' num2str(aStayPD) ' | aSwitch: ' num2str(aSwitchPD)]);
disp("-------------------");

%% Mean Memory Score (MS)
eStayMS = mean(eStayResps, 'omitnan');
eSwitchMS = mean(eSwitchResps, 'omitnan');
aStayMS = mean(aStayResps, 'omitnan');
aSwitchMS = mean(aSwitchResps, 'omitnan');

disp("Memory Scores:");
disp(['eStay:' num2str(eStayMS) ' | eSwitch: ' num2str(eSwitchMS) ' | aStay: ' num2str(aStayMS) ' | aSwitch: ' num2str(aSwitchMS)]);
disp("--------------");

%% Median Response Time (RT)
eStayRT = median(ResponseData.ResponseTime(matches([results{:, 1}], "eStay") & ~isnan([results{:, 2}])), 'omitnan');
eSwitchRT = median(ResponseData.ResponseTime(matches([results{:, 1}], "eSwitch") & ~isnan([results{:, 2}])), 'omitnan');
aStayRT = median(ResponseData.ResponseTime(matches([results{:, 1}], "aStay") & ~isnan([results{:, 2}])), 'omitnan');
aSwitchRT = median(ResponseData.ResponseTime(matches([results{:, 1}], "aSwitch") & ~isnan([results{:, 2}])), 'omitnan');

disp("Response Time:");
disp(['eStay:' num2str(eStayRT) ' | eSwitch: ' num2str(eSwitchRT) ' | aStay: ' num2str(aStayRT) ' | aSwitch: ' num2str(aSwitchRT)]);
disp("--------------");

disp("Guess Counts:");
disp(['eStay:' num2str(eStayGuessCount) ' | eSwitch:' num2str(eSwitchGuessCount) ' | aStay:' num2str(aStayGuessCount) ' | aSwitch:' num2str(aSwitchGuessCount)])

%% LOCAL FUNCTIONS 
function distance = EuclideanDistance(x0, y0, x1, y1)
    dX = x1 - x0;
    dY = y1 - y0;
    
    distance = sqrt((dX * dX) + (dY * dY));
end
