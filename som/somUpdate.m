function somUpdate( pattern, learningRate, neighborDist )
    global N IW;
    
    a = somActivation( pattern, neighborDist );
    IW = IW + learningRate * a * ( IW - repmat( pattern', N ) );