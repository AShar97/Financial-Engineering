function nsedata1 = importfile(filename, startRow, endRow)
%IMPORTFILE Import numeric data from a text file as a matrix.
%   NSEDATA1 = IMPORTFILE(FILENAME) Reads data from text file FILENAME for
%   the default selection.
%
%   NSEDATA1 = IMPORTFILE(FILENAME, STARTROW, ENDROW) Reads data from rows
%   STARTROW through ENDROW of text file FILENAME.
%
% Example:
%   nsedata1 = importfile('nsedata1.csv', 3, 1236);
%
%    See also TEXTSCAN.

% Auto-generated by MATLAB on 2018/03/22 10:07:54

%% Initialize variables.
delimiter = ',';
if nargin<=2
    startRow = 3;
    endRow = inf;
end

%% Read columns of data as text:
% For more information, see the TEXTSCAN documentation.
formatSpec = '%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%[^\n\r]';

%% Open the text file.
fileID = fopen(filename,'r');

%% Read columns of data according to the format.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, endRow(1)-startRow(1)+1, 'Delimiter', delimiter, 'TextType', 'string', 'HeaderLines', startRow(1)-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
for block=2:length(startRow)
    frewind(fileID);
    dataArrayBlock = textscan(fileID, formatSpec, endRow(block)-startRow(block)+1, 'Delimiter', delimiter, 'TextType', 'string', 'HeaderLines', startRow(block)-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
    for col=1:length(dataArray)
        dataArray{col} = [dataArray{col};dataArrayBlock{col}];
    end
end

%% Close the text file.
fclose(fileID);

%% Convert the contents of columns containing numeric text to numbers.
% Replace non-numeric text with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1
    raw(1:length(dataArray{col}),col) = mat2cell(dataArray{col}, ones(length(dataArray{col}), 1));
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]
    % Converts text in the input cell array to numbers. Replaced non-numeric
    % text with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1)
        % Create a regular expression to detect and remove non-numeric prefixes and
        % suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData(row), regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if numbers.contains(',')
                thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(numbers, thousandsRegExp, 'once'))
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric text to numbers.
            if ~invalidThousandsSeparator
                numbers = textscan(char(strrep(numbers, ',', '')), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch
            raw{row, col} = rawData{row};
        end
    end
end

% Convert the contents of columns with dates to MATLAB datetimes using the
% specified date format.
try
    dates{1} = datetime(dataArray{1}, 'Format', 'dd/MM/yy', 'InputFormat', 'dd/MM/yy');
catch
    try
        % Handle dates surrounded by quotes
        dataArray{1} = cellfun(@(x) x(2:end-1), dataArray{1}, 'UniformOutput', false);
        dates{1} = datetime(dataArray{1}, 'Format', 'dd/MM/yy', 'InputFormat', 'dd/MM/yy');
    catch
        dates{1} = repmat(datetime([NaN NaN NaN]), size(dataArray{1}));
    end
end

dates = dates(:,1);

%% Split data into numeric and string columns.
rawNumericColumns = raw(:, [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]);

%% Create output variable
nsedata1 = table;
nsedata1.date = dates{:, 1};
nsedata1.index = cell2mat(rawNumericColumns(:, 1));
nsedata1.in = cell2mat(rawNumericColumns(:, 2));
nsedata1.in1 = cell2mat(rawNumericColumns(:, 3));
nsedata1.in2 = cell2mat(rawNumericColumns(:, 4));
nsedata1.in3 = cell2mat(rawNumericColumns(:, 5));
nsedata1.in4 = cell2mat(rawNumericColumns(:, 6));
nsedata1.in5 = cell2mat(rawNumericColumns(:, 7));
nsedata1.in6 = cell2mat(rawNumericColumns(:, 8));
nsedata1.in7 = cell2mat(rawNumericColumns(:, 9));
nsedata1.in8 = cell2mat(rawNumericColumns(:, 10));
nsedata1.in9 = cell2mat(rawNumericColumns(:, 11));
nsedata1.out = cell2mat(rawNumericColumns(:, 12));
nsedata1.out1 = cell2mat(rawNumericColumns(:, 13));
nsedata1.out2 = cell2mat(rawNumericColumns(:, 14));
nsedata1.out3 = cell2mat(rawNumericColumns(:, 15));
nsedata1.out4 = cell2mat(rawNumericColumns(:, 16));
nsedata1.out5 = cell2mat(rawNumericColumns(:, 17));
nsedata1.out6 = cell2mat(rawNumericColumns(:, 18));
nsedata1.out7 = cell2mat(rawNumericColumns(:, 19));
nsedata1.out8 = cell2mat(rawNumericColumns(:, 20));
nsedata1.out9 = cell2mat(rawNumericColumns(:, 21));

% For code requiring serial dates (datenum) instead of datetime, uncomment
% the following line(s) below to return the imported dates as datenum(s).

% nsedata1.date=datenum(nsedata1.date);
