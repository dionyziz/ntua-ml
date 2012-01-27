function net = neural( TrainData, TrainDataTargets, architecture, training, learning )
    % architecture = [ 5 ]
    activation = { 'tansig', 'tansig', 'tansig' };
    % training = 'traingda'
    % learning = 'learngd'
    % Create a new neural network
    net = newff( ...
              TrainData, ...
              TrainDataTargets, ...
              architecture, ...
              activation, ...
              training, learning, 'mse' ...
          );
    % Divide our dataset into a training set and cross-validation set
    % in a 80/20 ratio to enable gradient descent early stopping
    net.divideParam.trainRatio = 0.8;
    net.divideParam.valRatio = 0.2;
    net.divideParam.testRatio = 0;
    % net.performParam.ratio = 15 / 16;
    net = train( net, TrainData, TrainDataTargets );
