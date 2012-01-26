function [ a ] = somActivation( pattern, neighborDist )
    global distances;
    
    ind = find( somOutput( pattern ) );
    a = distances( ind, : ) >= neighborDist;
    a = a .* 0.5;
    a( ind ) = 1;