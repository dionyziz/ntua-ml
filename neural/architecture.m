function a = architecture( TrainData, TrainDataTargets, TestData, TestDataTargets )
    [ TrainData, TrainDataTargets, TestData, TestDataTargets ] = ...
        preprocess( TrainData, TrainDataTargets, TestData, TestDataTargets );
    
    % bestf1 = 0;
    % for i = 5:5:30
    %     fprintf( 'Evaluating neural network with 1 hidden layer of %i neurons: ', i );
    %     f1 = measure( neural( TrainData, TrainDataTargets, [ i ], 'traingda', 'learngd' ), TestData, TestDataTargets );
    %     fprintf( '%f\n', f1 );
    %     if f1 > bestf1
    %         bestf1 = f1;
    %         bestarchitecture = i;
    %     end
    % end
    % 
    % bestf1 = 0;
    % training = { 'traingda' 'traingdx' 'trainlm' 'trainrp' 'trainbfg' 'traincgb' };
    % for i = 1:length( training )
    %     method = char( training( i ) );
    %     fprintf( 'Evaluating neural network with %s: ', method );
    %     f1 = measure( neural( TrainData, TrainDataTargets, [ 20 ], method, 'learngd' ), TestData, TestDataTargets );
    %     fprintf( '%f\n', f1 );
    %     if f1 > bestf1
    %         bestf1 = f1;
    %         bestarchitecture = i;
    %     end
    % end
    a = bestarchitecture;