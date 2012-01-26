function pos = hexagonalTopology(dim1,dim2)

%HEXAGONALTOPOLOGY Hexagonal layer topology function.
%
%	Syntax
%
%	  pos = hexagonalTopology(dim1,dim2)
%
%	Description
%
%	  HEXAGONALTOPOLOGY calculates the neuron positions for layers whose
%	  neurons are arranged in a two dimensional hexagonal pattern.
%
%	  HEXAGONALTOPOLOGY(DIM1,DIM2) takes 2 arguments,
%	    DIMi - Length of layer in dimension i.
%	  and returns a 2xS matrix of 2 coordinate vectors
%	  where S is the product of DIM1*DIM2.
%
%	Examples
%
%  %  This code creates and displays a two-dimensional layer
%  %  with 48 neurons arranged in a 8x6 hexagonal pattern.
%
%	    pos = hexagonalTopology(8,6); plotsom(pos)
%
%	See also GRIDTOP, HEXTOP, RANDTOP.

% THIS TO BE USED FOR GROUP-DATA AND NIPS500 DATASETS

pos = hextop(dim1,dim2);

if mod(dim2,2)==0
    sign = 1;
    for j = 1:dim2,
        pos(1,(j-1)*dim1+1:j*dim1) = pos(1,(j-1)*dim1+1:j*dim1)+sign*0.5;    
        sign = -1*sign;
    end
end