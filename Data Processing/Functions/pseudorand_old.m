function out = pseudorand(data, indsConds, numReps)
    % Pseudorandomly permutes row order of input cell array with customisable
    % number of allowed repetitions of the same condition. Returns cell array
    % of same dimensions as input.
    % 
    % PSEUDORAND(data, indsConds, numReps)
    %
    % data    = requires m-by-n cell array input.
    % conds   = indices of columns representing different conditions.
    % Requires n-dimensional numeric vector input.
    % numReps = number of allowed repetitions. Requires numeric scalar input.
    % Note that all three input arguments are required.
    %
    % by Michael Siena Nov, 2019
    
    % Validate input arguments %
    [numRows, ~] = size(data); % get number of rows
    numConds     = length(indsConds); % get number of conditions
    if nargin ~= 3
        error('PSEUDORAND requires three input arguments');
    elseif ~iscell(data) || numRows <= numReps
        error('data requires cell array input where the number of rows is greater than the number of allowed repetitions');
    elseif ~isnumeric(indsConds)
        error('indsConds requires numeric vector input');
    elseif ~isnumeric(numReps) || ~isscalar(numReps) || numReps < numConds
        error('numReps requires numeric scalar input that is equal to or greater than the number of conditions');
    end
    
    % Pseudorandomise row order %
    dataPerm   = data(randperm(numRows), :); % seed randomisation
    lengWindow = numReps + 1;
    seqWindow  = 0:lengWindow;
    indRow     = 1;
    while indRow <= (numRows - numReps)
        % Populate sliding window %
        window = cell(numReps, numConds);
        for indWindow = 1:lengWindow
            window(indWindow, :) = dataPerm(indRow + seqWindow(indWindow), :);
        end
        % Check whether number of allowed repetitions exceeded %
        indCond = 1;
        while indCond <= numConds
            noUnique = (lengWindow - numReps) == length(unique(window(:, indCond)));
            if noUnique == true
                dataPerm = data(randperm(numRows), :);
                indRow   = 1;
                break;
            end
            indCond = indCond + 1; 
        end
        indRow = indRow + 1;
        if indRow == (numRows - numReps)
            out = dataPerm;
            return;
        end
    end
end