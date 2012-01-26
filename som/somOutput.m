function [ output ] = somOutput( pattern )
    global IW N;
    
    % IW => NxD
    % pattern => Dx1
    compet( negdist( IW, pattern ) )