function somTrainParameters(setOrderLR,setOrderSteps,setTuneLR)

global distances maxNeighborDist tuneND orderLR orderSteps tuneLR;

maxNeighborDist = ceil(max(max(distances)));
tuneND = 1;

orderLR = setOrderLR; 
orderSteps = setOrderSteps;
tuneLR = setTuneLR;