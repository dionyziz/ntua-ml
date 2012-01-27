function [ TrainData, TrainDataTargets, TestData, TestDataTargets ] = ...
    preprocess( TrainData, TrainDataTargets, TestData, TestDataTargets )
    
    % Find how many items we need from each target
    howMany = min( sum( TrainDataTargets' ) );
    indexesToKeep = [];
    % find out which columns to keep (howMany items from each category)
    for i = 1:12
        indexesToKeep = [ indexesToKeep find( TrainDataTargets( i, : ), howMany ) ];
    end

    % permute the indices so that the categories are spread out in the result
    % and the neural network can be trained without bias
    indexesToKeep = indexesToKeep( :, randperm( size( indexesToKeep, 2 ) ) );
    % only keep the indices we picked
    TrainDataTargets = TrainDataTargets( :, indexesToKeep );
    % also keep the corresponding items from the targets
    TrainData = TrainData( :, indexesToKeep );

    % normalize the data by removing constant rows,
    % and taking the std. Then apply PCA to speed up
    % the training of our neural network.
    [ TrainData, PS1 ] = removeconstantrows( TrainData );
    % [ TrainData, PS2 ] = mapstd( TrainData );
    [ TrainData, PS3 ] = processpca( TrainData, 0.005 );
    [ TrainData, PS4 ] = mapminmax( TrainData );

    % Apply the same transformations on the test data set
    TestData = removeconstantrows( 'apply', TestData, PS1 );
    % TestData = mapstd( 'apply', TestData, PS2 );
    TestData = processpca( 'apply', TestData, PS3 );
    TestData = mapminmax( 'apply', TestData, PS4 );
