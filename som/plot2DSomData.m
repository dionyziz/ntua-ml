function [value] = plot2DSomData(IW,distances,patterns)
%Description
%
%   Plot in the same two dimensional graph
%   a Self Organizing Feature Map and
%   a number of points (column vectors).
%
%   plot2DSomData(IW,distances,patterns) takes three arguments,
%   IW          - NxD weight matrix.
%   distances   - NxN distance matrix.
%   patterns    - DxP pattern matrix.
%
%
%Example
%
%   p = 2*rand([2 40])-ones(2,40);
%   net = newsom(minmax(p),[2 3]);
%   net.inputWeights{1}.initFcn = 'rands';
%   net = init(net);
%   plot2DSomData(net.IW{1},net.layers{1}.distances,p);

%sound([0.0128 0.0256 0.0512 0.1024 0.2048 0.4096 0.8192]);

plot(patterns(1,:),patterns(2,:),'k+');
hold on;
plotsom(IW,distances);
hold off;
pause(0.5);