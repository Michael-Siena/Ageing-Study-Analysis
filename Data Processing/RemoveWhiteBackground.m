% Removes any pixel with white color value (i.e. [1 1 1]) from the raw object image files
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirActive.Filename)); % assign active dir to cd
addpath(genpath('./Functions/')); % adds path for Functions directory

inputStimPath  = './Stimuli/Raw/*.jpg';
outputStimPath = './Stimuli/WhiteRemoved';

filePattern = fullfile(inputStimPath, '*.jpg*');
jpegFiles   = dir(filePattern);

for k = 1:length(jpegFiles)
    baseFileName = jpegFiles(k).name;
    fullFileName = fullfile(inputStimPath, baseFileName);

    I               = imread(fullFileName); 
    Igray           = rgb2gray(I); % Convert RGB image or colormap to grayscale
    level           = .9; % higher threshold = more fuzzing
    BW              = im2bw(Igray, level); % computes global threshold (level) that can be used to convert  intensity image to  binary image with IM2BW
    BW2             = BW;
    BW2             = im2uint8(BW2);
    mask_three_chan = repmat(BW2, [1, 1, 3]);   % Create 3 channel mask
    I(~mask_three_chan == 0) = 255;             % Apply Mask
    
    [M,N,rgb] = size(I);                        % I is our image
    A = zeros(M,N);                             % Assign A as zero
    for i = 1:M                                 % Iterate through X, to assign A
       for j = 1:N
          if(I(i, j) < 255)                     % Assuming uint8, 255 would be white
             A(i, j) = 1;                       % Assign 1 to transparent color(white)
          end
       end
    end
    
   A = imfill(A); % makes sure that nothing from the center of the image gets filtered out

   cd(outputStimuli)
   transpfilename = [baseFileName(1:end - 4) '_tr.png'];
   imwrite(I, transpfilename,'Alpha', A); 
end
