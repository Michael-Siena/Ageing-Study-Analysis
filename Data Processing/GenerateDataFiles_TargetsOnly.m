%% Prepare MATLAB environment
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirActive.Filename)); % assign active dir to cd
addpath(genpath('./Functions/')); % adds path for Functions directory

%% Initialise arena
arenaCenterX = 0;
arenaCenterY = 0;
arenaDiameter = 50 - 10;
arenaCircumfrence = arenaDiameter * pi;
arenaRadius = arenaCircumfrence / (2 * pi);

%% Initialise data
nSubjects = 12;
nSessions = 1;
nBlocks = 10;
nTrialsPerBlock = 8;
nTrials = nBlocks * nTrialsPerBlock; % number of targets in a test session
sectors = 1; % I decided to remove quadrants so now only 1 sector
nTrialsPerSect = nTrials / sectors;

%% Get stimuli names
sourcePath = './Stimuli/MainTask/';

%% Generate data files
tic;
for sub = 1:nSubjects
    targets = dir(fullfile([sourcePath 'Targets/'], '*.png'));
    targetNames = {targets(randperm(nTrials)).name}'; % shuffle indices
    targetNames = cellfun(@(t) t(1:end - 4), targetNames, 'UniformOutput', false); % strip file extension from target names
    orientationCues = dir(fullfile([sourcePath 'OrientationCues/'], '*.png'));
    orientationCueNames = {orientationCues(randperm(4 * nBlocks)).name}'; % shuffle indices
    orientationCueNames = cellfun(@(oc) oc(1:end - 4), orientationCueNames, 'UniformOutput', false); % strip file extension from names

    for sess = 1:nSessions
        %% Check if data already generated
        subDirectory = ['DataFiles/sub' num2str(sub) '/'];
        if ~exist(subDirectory, 'dir')
            mkdir(subDirectory);
        else
            disp(['Subject: ' num2str(sub) '| MSG: Folder already exists - checking for conflicts with existing data files']);
            if exist([subDirectory 'sub' num2str(sub) '/*.csv'], 'file') || exist([subDirectory 'sub' num2str(sub) '/*.csv'], 'file')
                disp(['Subject: ' num2str(sub) '| Session: ' num2str(sess) ' | MSG: File already exists - terminating session!']);
                return;
            end
        end

        %% Generate orientation cues
        ocStartingPerspective = [repmat({1}, 6, 1); repmat({2}, 6, 1)];
        ocNReps = 3;
        ocNConds = 1;
        ocCondInds = 1;
        ocStartingPerspective = pseudorand(ocStartingPerspective, ocNReps, ocNConds, ocCondInds);
        ocBlock = 1:nBlocks;
        ocColNames = [{'North'}, {'South'}, {'East'}, {'West'}, {'StartingPerspective'}, {'Block'}, {'Sess'}, {'Sub'}];

        nOrientationCues = length(orientationCueNames);
        ocFirst = 1:4:nOrientationCues;
        ocSecond = 4:4:nOrientationCues;
        orientationCueData = cell(nBlocks, length(ocColNames));
        for b = 1:nBlocks
            tmpOrientationCues = orientationCueNames(ocFirst(b):ocSecond(b))';
            orientationCueData(b, :) = horzcat(tmpOrientationCues, ocStartingPerspective(b), {ocBlock(b)}, {sess}, {sub});
        end

        orientationCueDataTable = cell2table(orientationCueData, 'VariableNames', ocColNames);
        subFileOrientationCue = ['sub' num2str(sub) '_sess' num2str(sess) '_orientationCues.csv'];
        writetable(orientationCueDataTable, [subDirectory subFileOrientationCue]);

        disp('GENERATED : orientation cue data');

        %% Generate study phase data
        % Initialise target coordinates
        targetCoordinates = cell(nTrials, 2);
        
        % Target sectors
        tmpTargetSect = arrayfun(@(s) repmat(num2str(s), nTrialsPerSect, 1), 1:sectors, 'un', false);
        targetSect = vertcat(tmpTargetSect{:});
        targetSect = str2num(targetSect);

        % Sector conditions
        sectCond = num2cell(targetSect);

        % Perspective conds
        perspCond = repmat({1; 1; 2; 2}, nTrials / 4, 1);

        % Switch/stay conditions
        swstCond = repmat({1; 2}, nTrials / 2, 1);

        % Append conditions to coordinate data
        trialDataConds = horzcat(targetNames, targetCoordinates, sectCond, perspCond, swstCond);

        % Pseudorandomise trial order
        nReps = 3; % the same combination of conditions cannot be repeat more than three times
        if (sectors > 1)
            nIgnoredCols = 6; % we ignore first six cols because there is only one sector
        else
            nIgnoredCols = 4; % we subtract 4 from the condition  number because first four columns are the (x,z) coords, names, and sectors for the targets
        end
        nCols = size(trialDataConds, 2);
        nCondsUsedForPseudorand = nCols - nIgnoredCols;
        tmpCondInds = arrayfun(@(c) nIgnoredCols + c, 1:nCondsUsedForPseudorand, 'un', false);
        condInds = [tmpCondInds{:}];
        first = 1:nTrialsPerBlock:nTrials;
        second = nTrialsPerBlock:nTrialsPerBlock:nTrials;
        for b = 1:nBlocks
            % Generate target coordinates
            trialDataConds(first(b):second(b), 2:3) = coordGen(arenaCenterX, arenaCenterY, sectors, arenaRadius, nTrialsPerBlock); 

            tmpStudyData = trialDataConds(first(b):second(b), :);
            pseudorandStudyData(first(b):second(b), :) = pseudorand(tmpStudyData, nReps, nCondsUsedForPseudorand, condInds);
        end

        % Block conditions
        tmpBlockCond = arrayfun(@(b) repmat(b, nTrials / nBlocks, 1), 1:nBlocks, 'un', false);
        blockCond = num2cell(vertcat(tmpBlockCond{:}));

        % Append block condition to rest of data
        studyData = horzcat(pseudorandStudyData, blockCond);

        disp('GENERATED : study data');

        %% Generate test data from study data
        testPerspective = cell(nTrialsPerBlock, 1);
        testData = cell(nTrials, size(studyData, 2) + 1);
        first = 1:nTrialsPerBlock:nTrials;
        second = nTrialsPerBlock:nTrialsPerBlock:nTrials;
        for b = 1:nBlocks
            block = studyData(first(b):second(b), :);
            for r = 1:nTrialsPerBlock
                % switch trial
                if (block{r, 6} == 1)
                    if (block{r, 5} == 1)
                        testPerspective{r} = 2;
                    elseif (block{r, 5} == 2)
                        testPerspective{r} = 1;
                    end
                % stay trial    
                elseif (block{r, 6} == 2)
                    testPerspective{r} = block{r, 5};
                end
            end
            blockAllConds = horzcat(block(:, 1:6), testPerspective, block(:, 7:end));
            nCondsUsedForPseudorand = 3;
            condInds = 5:7;
            testData(first(b):second(b), :) = pseudorand(blockAllConds, nReps, nCondsUsedForPseudorand, condInds);
        end

        disp('GENERATED : test data');

        %% Convert data cell arrays to tables and write to data files
        startIndices = 1:nTrials / nBlocks:nTrials;
        endIndices = nTrials / nBlocks:nTrials / nBlocks:nTrials;

        studyColumnNames = [{'TargetName'}, {'TargetX'}, {'TargetZ'}, {'Sectors'}, {'Perspective'}, {'SwitchStatus'}, {'Block'}]; 
        testColumnNames = [{'TargetName'}, {'TargetX'}, {'TargetZ'}, {'Sectors'}, {'StudiedPerspective'}, {'TestPerspective'}, {'SwitchStatus'}, {'Block'}];

        studyDataTable = cell2table(studyData, 'VariableNames', studyColumnNames);
        testDataTable = cell2table(testData, 'VariableNames', testColumnNames);

        for b = 1:nBlocks
            subFileStudy = ['sub' num2str(sub) '_sess' num2str(sess) '_block' num2str(b) '_study.csv'];
            subFileTest = ['sub' num2str(sub) '_sess' num2str(sess) '_block' num2str(b) '_test.csv'];
            writetable(studyDataTable(startIndices(b):endIndices(b), 1:end - 2), [subDirectory subFileStudy]);
            writetable(testDataTable(startIndices(b):endIndices(b), 1:end - 1), [subDirectory subFileTest]);       
        end

        disp('WROTE : all data');
        disp(['SUBJECT : ' num2str(sub) ' | SESSION : ' num2str(sess) ' | TIIME ELAPSED (SECS) : ' num2str(toc)]);
        disp('================================================================================================');
    end
end

disp('FINISHED');