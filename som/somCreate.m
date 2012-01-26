function somCreate(minMax,gridSize)

global neuronsPerRow neuronsPerColumn N IW distances;

neuronsPerRow = gridSize(1,1);
neuronsPerColumn = gridSize(1,2);
N = neuronsPerRow*neuronsPerColumn;

minFeatureValues = minMax(:,1)';
maxFeatureValues = minMax(:,2)';
dimensions = size(minMax,1);
for i = 1:N,
    IW(i,:) = rand(1,dimensions).*(maxFeatureValues-minFeatureValues)+minFeatureValues;
end

positions = gridtop(neuronsPerRow,neuronsPerColumn);
distances = boxdist(positions);