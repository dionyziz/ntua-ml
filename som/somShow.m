function [value] = somShow(IW,gridsize)
% SOMSHOW
%
%
% PURPOSE 
%
% Shows basic visualizations of SOM: unified distance matrices (or U-matrices for short).
%
%
% SYNTAX
%
%  somShow(IW,gridsize) takes two arguments:
%  IW          - NxD weight matrix (map struct).
%  gridsize    - 1x2 vector (layer dimensions), its first value is the number of neurons 
%                across the x-axis (horizontal) while its second value is the number of 
%                neurons across the y-axis (vertical).
%
%
% DESCRIPTION 
%
% This function is used for basic visualization of the SOM. A spesific kind of SOM plane 
% can be shown: 
%
%  U-matrix (or distance matrix) which shows clustering structure of the SOM. 
%
% The u-matrices have colorbars showing the scale for the variable. The scale shows 
% by default the values that variables have in the map struct.
%
% By default the u-matrix - calculated using all variables - is shown.
%
%
% NOTICE
%
% It is obligatiry that the SOM's architecture must consist of a hexagonal lattice
% created with function HEXAGONALTOPOLOGY. The hexagonal lattice under consideration
% can only be one or two dimensional.
%
%
% EXAMPLE
%
%% Create a random weight matrix (map struct), and a 3x8 hexagonal lattice (map).
%% Afterwards do the basic visualization with SOMSHOW: u-matrix (distance matrix).
%
%   somShow(rand(24,4),[3,8]);
%
%
% See also HEXAGONALTOPOLOGY.

nx = gridsize(1,1);
ny = gridsize(1,2);

sMap = som_map_struct(size(IW,2),'name','Self-Organizing Map','msize',[ny,nx]);

index = 1;
for i = 1:nx,
    for j = 1:ny,
        sMap.codebook(index,:) = IW(nx*ny-j*nx+i,:);
        index = index+1;
    end
end

h = som_show(sMap,'umat','all');










































































































































































































































































































































































































































































































































































































































































































































































































































































































































































function sMap = som_map_struct(dim, varargin)

%SOM_MAP_STRUCT Create map struct. 
% 
% sMap = som_map_struct(dim, [[argID,] value, ...])
%
%  sMap = som_map_struct(4);
%  sMap = som_map_struct(4,'msize',[3 4],'hexa','sheet');
%  sMap = som_map_struct(4,'msize',[3 4 5],'rect','name','a 3D-SOM');
%  sMap = som_map_struct(4,'msize',[3 4],'bubble','mask',[1 1 1 0]);
%
%  Input and output arguments ([]'s are optional): 
%   dim      (scalar) input space dimension
%   [argID,  (string) See below. The values which are unambiguous can 
%    value]  (varies) be given without the preceeding argID.
%
%   sMap     (struct) self-organizing map struct
%
% Here are the valid argument IDs and corresponding values. The values
% which are unambiguous (marked with '*') can be given without the
% preceeding argID.
%   'mask'       (vector) BMU search mask, size dim x 1
%   'msize'      (vector) map grid size, default is [0]
%   'labels'     (string array / cellstr) labels for each map unit, 
%                 length=prod(msize)
%   'name'       (string) map name
%   'comp_names' (string array / cellstr) component names, size dim x 1
%   'comp_norm'  (cell array) normalization operations for each
%                 component, size dim x 1. Each cell is either empty, 
%                 or a cell array of normalization structs.
%   'topol'     *(struct) topology struct
%   'som_topol','sTopol' = 'topol'
%   'lattice'   *(string) map lattice, 'hexa' or 'rect'
%   'shape'     *(string) map shape, 'sheet', 'cyl' or 'toroid'
%   'neigh'     *(string) neighborhood function, 'gaussian', 'cutgauss',
%                 'ep' or 'bubble'
%
% For more help, try 'type som_map_struct' or check out online documentation.
% See also SOM_SET, SOM_INFO, SOM_DATA_STRUCT, SOM_TOPOL_STRUCT, SOM_MAKE.

%%%%%%%%%%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% som_map_struct
%
% PURPOSE
%
% Creates a self-organizing map structure. 
%
% SYNTAX
%
%  sM = som_map_struct(dim)
%  sM = som_map_struct(...,'argID',value,...);
%  sM = som_map_struct(...,value,...);
%
% DESCRIPTION
%
% Creates a self-organizing map struct. The struct contains the map
% codebook, labels, topology, information on normalization and training, 
% as well as component names and a name for the map. The obligatory
% parameter is the map dimension. Most of the other fields can be
% given values using optional arguments. If they are left unspecified,
% default values are used.
%
%  Field         Type         Size / default value (munits = prod(msize))
%  ------------------------------------------------------------------------
%   .type        (string)     'som_map'               
%   .name        (string)     'SOM date'
%   .codebook    (matrix)     rand(munits, dim)
%   .topol       (struct)     topology struct, with the following fields
%     .type         (string)  'som_topol'
%     .msize        (vector)  size k x 1, [0] 
%     .lattice      (string)  'hexa' 
%     .shape        (string)  'sheet'
%   .labels      (cellstr)    size munits x m, {''; ''; ... ''}
%   .neigh       (string)     'gaussian'
%   .mask        (vector)     size dim x 1, [1; 1; ...; 1]
%   .trainhist   (cell array) size tl x 1, []
%   .comp_names  (cellstr)    size dim x 1, {'Variable1', 'Variable2', ...}
%   .comp_norm   (cell array) size dim x 1, {[], [], ... []}
%
% '.type' field is the struct identifier. Do not change it.
% '.name' field is the identifier for the whole map struct
% '.codebook' field is the codebook matrix, each row corresponds to one unit
% '.topol' field is the topology of the map. This struct has three fields:
%   '.msize' field is the dimensions of the map grid. Note that the
%         matrix notation of indeces is used.
%   '.lattice' field is the map grid lattice
%   '.shape' field is the map grid shape
% '.labels' field contains the labels for each of the vectors. The ith row
%         of '.labels' contains the labels for ith map unit. Note that 
%         if some vectors have more labels than others, the others are
%         are given empty labels ('') to pad the '.labels' array up.
% '.neigh' field is the neighborhood function. 
% '.mask' field is the BMU search mask.
% '.trainhist' field contains information on the training. It is a cell
%         array of training structs. The first training struct contains
%         information on initialization, the others on actual trainings. 
%         If the map has not been initialized, '.trainhist' is empty ([]).
% '.comp_names' field contains the names of the vector components
% '.comp_norm' field contains normalization information for each
%         component. Each cell of '.comp_norm' is itself a cell array of
%         normalization structs. If no normalizations are performed for 
%         the particular component, the cell is empty ([]).
%
% REQUIRED INPUT ARGUMENTS
%
%  dim    (scalar) Input space dimension. 
%  
% OPTIONAL INPUT ARGUMENTS 
%
%  argID (string) Argument identifier string (see below).
%  value (varies) Value for the argument (see below).
%
%  The optional arguments are given as 'argID',value -pairs. If the
%  value is unambiguous (marked below with '*'), it can be given
%  without the preceeding argID. If an argument is given value
%  multiple times, the last one is used.
%
%   'mask'       (vector) BMU search mask, size dim x 1
%   'msize'      (vector) map grid size, default is [0]
%   'labels'     (string array / cellstr) labels for each map unit, 
%                 length=prod(msize)
%   'name'       (string) map name
%   'comp_names' (string array / cellstr) component names, size dim x 1
%   'comp_norm'  (cell array) normalization operations for each
%                 component, size dim x 1. Each cell is either empty, 
%                 or a cell array of normalization structs.
%   'lattice'   *(string) map lattice, 'hexa' or 'rect'
%   'shape'     *(string) map shape, 'sheet', 'cyl' or 'toroid'
%   'topol'     *(struct) topology struct, sets msize, lattice and shape
%   'som_topol','sTopol' = 'topol'
%   'neigh'     *(string) neighborhood function, 'gaussian', 'cutgauss',
%                 'ep' or 'bubble'
%
% OUTPUT ARGUMENTS
% 
%  sMap (struct) the map struct
%
% EXAMPLES
%
% Simplest case:
%  sMap = som_map_struct(3);
%  
% With optional arguments, the other fields can be given values:
%  sTo    = som_set('som_topol','msize',[10 5]);
%  labs   = cell(50, 1); labs{1, 1} = 'first_unit';
%  cnames = {'first'; 'second'; 'third'};
%  sN     = som_set('som_norm');
%  csN    = {sN; sN; sN};
%  
%  sMap = som_map_struct(3,'msize',[10 5],'rect');
%  sMap = som_map_struct(3,'msize',[10 5],'lattice','rect');
%  sMap = som_map_struct(3,sTo,'bubble','labels',labs);
%  sMap = som_map_struct(3,sTo,'comp_names',cnames);
%  sMap = som_map_struct(3,sTo,'name','a data struct');
%  sMap = som_map_struct(3,sTo,'comp_norm',csN,'mask',[1 0 0.5]);
%
% SEE ALSO
% 
%  som_set          Set values and create SOM Toolbox structs.
%  som_data_struct  Create a data struct.
%  som_make         Initialize and train self-organizing map.
%  som_topol_struct Default values for map topology.

% Copyright (c) 1997-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/

% Version 1.0beta ecco 100997
% Version 2.0beta juuso 101199 130300

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% default values
sTopol     = som_set('som_topol','lattice','hexa','shape','sheet');
neigh      = 'gaussian';
mask       = ones(dim,1);
name       = sprintf('SOM %s', datestr(now, 1));
labels     = cell(prod(sTopol.msize),1);
for i=1:length(labels), labels{i} = ''; end
comp_names = cell(dim,1); 
for i = 1:dim, comp_names{i} = sprintf('Variable%d', i); end
comp_norm  = cell(dim,1); 

% varargin
i=1; 
while i<=length(varargin), 
  argok = 1; 
  if ischar(varargin{i}), 
    switch varargin{i}, 
      % argument IDs
     case 'mask',       i=i+1; mask = varargin{i}; 
     case 'msize',      i=i+1; sTopol.msize = varargin{i}; 
     case 'labels',     i=i+1; labels = varargin{i};
     case 'name',       i=i+1; name = varargin{i};
     case 'comp_names', i=i+1; comp_names = varargin{i}; 
     case 'comp_norm',  i=i+1; comp_norm = varargin{i};
     case 'lattice',    i=i+1; sTopol.lattice = varargin{i};
     case 'shape',      i=i+1; sTopol.shape = varargin{i}; 
     case {'topol','som_topol','sTopol'}, i=i+1; sTopol = varargin{i};
     case 'neigh',      i=i+1; neigh = varargin{i};
      % unambiguous values
     case {'hexa','rect'}, sTopol.lattice = varargin{i};
     case {'sheet','cyl','toroid'}, sTopol.shape = varargin{i}; 
     case {'gaussian','cutgauss','ep','bubble'}, neigh = varargin{i};
     otherwise argok=0; 
    end
  elseif isstruct(varargin{i}) & isfield(varargin{i},'type'), 
    switch varargin{i}(1).type, 
     case 'som_topol', sTopol = varargin{i};
     otherwise argok=0; 
    end
  else
    argok = 0; 
  end
  if ~argok, 
    disp(['(som_map_struct) Ignoring invalid argument #' num2str(i+1)]); 
  end
  i = i+1; 
end

% create the SOM
codebook = rand(prod(sTopol.msize),dim); 
sTrain = som_set('som_train','time',datestr(now,0),'mask',mask);
sMap = som_set('som_map','codebook',codebook,'topol',sTopol,...
                         'neigh',neigh,'labels',labels,'mask',mask,...
                         'comp_names',comp_names,'name',name,...
                         'comp_norm',comp_norm,'trainhist',sTrain);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







function h=som_show(sMap, varargin)

% SOM_SHOW Basic SOM visualizations: component planes, u-matrix etc.
%
% h = som_show(sMap, ['argID', value, ...])
% 
%  som_show(sMap);
%  som_show(sMap,'bar','none');
%  som_show(sMap,'comp',[1:3],'umat','all');
%  som_show(sMap,'comp',[1 2],'umat',{[1 2],'1,2 only'},'comp',[3:6]);   
%  som_show(sMap,'size',m,'bar','vert','edge','off');
%
% Input and output arguments ([]'s are optional):
%  sMap        (struct) map struct
%  [argID,     (string) Additional parameters are given as argID, value
%    value]    (varies) pairs. See below for list of valid IDs and values.
%
%  h           (struct) struct with the following fields:
%   .plane     (vector) handles to the axes objecets (subplots)
%   .colorbar  (vector) handles to the colorbars. Colorbars for empty
%                       grids & RGB color planes do not exist: the
%                       value for them in the vector is -1.
%   .label     (vector) handles to the axis labels
%
% Here are the valid argument IDs and corresponding values. M is
% the number of map units
%  'comp'               Which component planes to draw, title is
%                       the name of the component (from sMap.comp_names) 
%              (vector) a vector of component indices
%              (string) 'all' (or '' or []) for all components
%  'compi'              as 'comp' but uses interpolated shading
%  'umat'               Show u-matrix calculated using specified 
%                       components 
%              (vector) a vector of component indeces
%              (string) 'all' (or '' or []) to use all components
%              (cell)   of form {v, str} uses v as the vector, and put
%                       str as title instead of the default 'U-matrix'
%  'umati'              as 'umat' but uses interpolated shading of colors 
%  'empty'     (string) Make an empty plane using given string as title
%  'color'              Set arbitrary unit colors explicitly  
%              (matrix) size Mx1 or Mx3, Mx1 matrix uses indexed
%                       coloring;  Mx3 matrix (RGB triples as rows)
%                       defines fixed unit colors
%              (cell)   of from {color, str}. 'color' is the Mx1
%                       or Mx3 RGB triple matrix and 'str' is title 
%                       string
%  'colori'             as 'color' but uses interpolated shading of colors 
%  'norm'      (string) 'n' or 'd': Whether to show normalized 'n' or 
%                       denormalized 'd' data values on the
%                       colorbar. By default denormalized values are used.
%  'bar'       (string) Colorbar direction: 'horiz', 'vert' (default)
%                       or 'none'
%  'size'               size of the units
%              (scalar) same size for each unit, default is 1
%              (vector) size Mx1, individual size for each unit
%  'edge'      (string) Unit edges on component planes 'on'
%                       (default) or 'off'
%  'footnote'  (string) Footnote string, sMap.name by default
%  'colormap'  (matrix) user defined colormap 
%  'subplots'  (vector) size 1 x 2, the number of subplots in y- and
%                       and x-directions (as in SUBPLOT command)
%
% If identifiers 'comp', 'compi', 'umat', 'umati', 'color', 'colori'
% or 'empty' are not specified at all, e.g. som_show(sMap) or
% som_show(sMap,'bar','none'), the U-matrix and all component planes
% are shown.
%
% For more help, try 'type som_show' or check out online documentation. 
% See also SOM_SHOW_ADD, SOM_SHOW_CLEAR, SOM_UMAT, SOM_CPLANE, SOM_GRID.

%%%%%%%%%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% som_show
%
% PURPOSE 
%
% Shows basic visualizations of SOM: component planes, unified distance
% matrices as well as empty planes and fixed color planes.
%
% SYNTAX
%
%  h = som_show(sMap)
%  h = som_show(sMap, 'argID', value, ...)
%
% DESCRIPTION 
%
% This function is used for basic visualization of the SOM. Four
% kinds of SOM planes can be shown: 
%
%  1. U-matrix (see SOM_UMAT) which shows clustering structure of
%     the SOM. Either all or just part of the components can 
%     be used in calculating the U-matrix.
%  2. component planes: each component plane shows the values of
%     one variable in each map unit
%  3. an empty plane which may be used as a base for, e.g., hit 
%     histogram visualization or labeling (see SOM_SHOW_ADD)
%  4. a fixed or indexed color representation for showing color coding or 
%     clustering
%
% The component planes and u-matrices may have colorbars showing the
% scale for the variable. The scale shows by default the values that
% variables have in the map struct. It may be changed to show the
% original data values (estimated by SOM_DENORMALIZE). In this case a
% small 'd' appears below the colorbar. The orientation of these
% colorbars may be changed, or they can be removed.
%
% By default the u-matrix - calculated using all variables - and all
% component planes are shown. This is achieved by giving command
% som_show(sMap) without any further arguments
%
% REQUIRED INPUT ARGUMENTS
%
% sMap  (struct) Map to be shown. If only this argument is
%                specified, the function draws first the u-matrix 
%                calculated using all the variables followed by all
%                the component planes.
%
% OPTIONAL INPUT ARGUMENTS
% 
% (M is the number of map units)
%
% Optional arguments must be given as 'argID',value -pairs
% 
% 'comp'      Defines the variabels to be shown as component planes.
%    (vector) 1xN or Nx1 vector with integer positive numbers ranging 
%             from 1 to the number of variables in the map codebook
%             (dim). This vector determines the variables to be show
%             as component planes and their order. Note that the same
%             component plane (the same variable index) is allowed to
%             occur several times.
%    (string) 'all' or '' or []. This uses all variables, that is, it's
%             the same that using value [1:dim] where dim is the
%             number of variables in the codebook.
%       
% 'compi'     Same as 'comp' but uses a Gouraud shaded color plane 
%             (made using SOM_GRID function) instead of the cell-like
%             visualization of 'comp' (made using SOM_CPLANE). Note
%             that the color interpolation doesn't work strictly
%             correctly on 'hexa' grid, as it uses rectangular grid
%             (see SURF).
% 
% 'umat'      Show U-matrix: value defines the variables to be used
%             for calculating a u-matrix.
%    (vector) as in 'comps'. However, multiple occurences of the
%             same variable (same variable index) are ignored. 
%    (string) 'all' or '' or []. This uses all variables, that is, 
%             is the same as using value [1:dim] where dim is the
%             number of variables in the codebook. 
%    (cell)   of form {v, str} where v is a valid index vector for 'umat' 
%             (see above) and str is a string that is used as a title 
%             for the u-matrix instead of the default title
%             'U-matrix'. This may be useful if several u-matrices
%             are shown in the same figure. 
% 
% 'umati'     Same as 'umat' but uses shaded color plane (see 'compi').
%
% 'empty'     Show an empty plane (patch edges only)
%    (string) value is used as title
% 
% 'color'     Define fixed RGB colors for the map units
%    (matrix) a Mx3 matrix of RGB triples as rows             
%    (vector) a Mx1 vector of any values: sets indexed coloring using
%             the current colormap (as SURF does)  
%    (matrix) a Mx3xN matrix of RGB triples as rows. This gives N
%             color planes.
%    (matrix) a Mx1xN matrix of any values: sets indexed coloring using
%             the current colormap (as SURF does). This gives N
%             color planes.
%    (cell)   of form {rgb, str} where rgb is a Mx3 (xN) matrix of RGB
%             triples as rows and str is a string that is used as
%             title(s).  
%    (cell)   of form {v, str} where v is a Mx1(xN) matrix of values
%             and str is a string that is used as title(s). 
%
% 'colori'    Same as 'color' but uses shaded color plane (see 'compi').
%
% 'norm'      Defines whether to use normalized or denormalized
%             values in the colorbar. If denormalized values are
%             used, they are acquired from SOM_DENORMALIZE function 
%             using sMap.comp_norm field.
%    (string) 'd' (default) for denormalized values and 'n' for
%             normalized values. The corresponding letter appears
%             below the colorbar.
%   
% 'bar'       Define the direction of the colorbars for component planes 
%             and U-matrices or turn them completely off.
%    (string) 'vert' (default), 'horiz' or 'none'. 'vert' gives
%             vertical and 'horiz' horizontal colorbars. 'none'
%             shows no colorbars at all. 
%
% 'size'      Define sizes of the units. 
%    (scalar) all units have the same size (1 by default)
%    (vector) size Mx1, each unit gets individual size scaling 
%             (as in SOM_CPLANE)
%
% 'edge'      Unit edges on component plane visualizations.
%    (string) 'on' or 'off' determines whether the unit edges on component 
%             planes ('comp') are shown or not. Default is 'off'. Note that
%             U-matrix and color planes are _always_ drawn without edges.
%
% 'footnote'  Text on the figure
%    (string) is printed as a movable text object on the figure
%             where it may be moved using mouse. Default value is the
%             string in the sMap.name field. Note: value [] gives the
%             string, but input value '' gives no footnote a all. 
%             See VIS_FOOTNOTE for more information on the text object 
%             and ways to change its font size.
% 
% 'colormap'  som_show ghages the colormap by default to a gray-level map
%    (matrix) This argument is used to set some other colormap. 
%
% 'subplots'  the number of subplots in y- and x-directions, as in 
%    (vector) command SUBPLOT
% 
% OUTPUT ARGUMENTS
%
% h (struct)
%    .plane         (vector) handles to the axes objects (subplots)
%    .colorbar      (vector) handles to the colorbars. Colorbars of empty
%                            & color planes do not exist: the corresponding
%                            value in the vector is -1
%    .label         (vector) handles to the axis labels
%
% OBJECT TAGS
%
% The property field 'Tag' of the axis objects created by this function 
% are set to contain string 'Cplane' if the axis contains component plane
% ('comp'), color plane ('color') or empty plane ('empty') and string
% 'Uplane' if it contains a u-matrix ('umat'). The tag is set to 
% 'CplaneI' for planes created using 'compi' and 'colori', and 
% 'UplaneI' for 'umati'.
%
% FEATURES
%
% Note that when interpolated shading is used in coloring ('compi' and
% 'colori') the standard built-in bilinear Gouraud interpolation for a 
% SURF object is used. If the lattice is hexagonal - or anything else than 
% rectangular in general - the result is not strictly what is looked 
% for, especially if the map is small. 
%
% EXAMPLES
%
%% Make random data, normalize it, and give component names
%% Make a map
%
%   data=som_data_struct(rand(1000,3),'comp_names',{'One','Two','Three'});
%   data=som_normalize(data,'var');
%   map=som_make(data);
%
%% Do the basic visualization with som_show: u-matrix and all
%% component planes
%
%   som_show(map);   
%
%% The values shown in the colorbar are denormalized codebook values 
%% (if denormalization is possible). To view the actual values, use
%% the ..., 'norm', 'n' argument pair.
%
%   som_show(map,'norm','n')
%
%% Something more complex:
%% Show 1-2. Component planes 1 and 2 (variables 'One' and 'Two')
%%        3. U-matrix that is calculated only using variables
%%           'One' and 'Two' 
%%           with title '1,2 only'
%%        4. U-matrix that is calculated using all variables with the 
%%           deafult title 'U-matrix'
%%        5. The color code (in c) with title 'Color code'
%%        6. Component plane 3 (variable 'Three')
%% and  use vertical colorbars and and the values      
%% But first: make a continuous color code (see som_colorcode)
%
% c=som_colorcode(map,'rgb1');
% 
% som_show(map,'comp',[1 2],'umat',{1:2,'1,2 only'},'umat','all', ...
%  'color',{c,'Color code'},'bar','vert','norm','n','comp',3)
%
%  SEE ALSO
%
% som_show_add   Show hits, labels and trajectories on SOM_SHOW visualization.
% som_show_clear Clear hit marks, labels or trajectories from current figure. 
% som_umat       Compute unified distance matrix of self-organizing map.
% som_grid       Visualization of a SOM grid.
% som_cplane     Visualization of component, u-matrix and color planes.

% Copyright (c) 1997-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 1.0beta johan 100298 
% Version 2.0beta johan 201099 juuso 181199 johan 011299-100200
%                 juuso 130300 190600

%% Check arguments %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(nargchk(1,Inf,nargin))     % check no. of input args

if isstruct(sMap),               % check map
  [tmp,ok,tmp]=som_set(sMap);
  if all(ok) & strcmp(sMap.type,'som_map') 
    ;
  else
    error('Map struct is invalid!');
  end
else
  error('Requires a map struct!')
end

munits=size(sMap.codebook,1); % numb. of map units
d=size(sMap.codebook,2);      % numb. of components
msize=sMap.topol.msize;       % size of the map
lattice=sMap.topol.lattice;   % lattice

if length(msize)>2 
  error('This visualizes only 2D maps!')
end

if rem(length(varargin),2)
  error('Mismatch in identifier-value  pairs.');
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  read in optional arguments
 
if isempty(varargin),
  varargin = { 'umat','all','comp','all'};
end

%% check the varargin and build visualization infostrcuts
% Vis:       what kind of planes, in which order, what are the values in
%            the units
% Vis_param: general properties
% see subfunction

% The try-catch construction is here just for avoiding the
% possible termination to happen in subfunction because an error
% message containing subfunction line numbers etc. might be confusing, as
% there probably is nothing wrong with the subfunction but with the 
% input. Ok, this isn't proper programming sytle... 

try       
  [Plane, General]= check_varargin(varargin, munits, d, sMap.name);
catch
  error(lasterr);
end

% Set default values for missing ones

% No planes at all (only general properties given in varargin):
% set default visualization

if isempty(Plane)
  varargin = [varargin, { 'umat','all','comp','all'}];
  % and again we go...
  try
    [Plane, General]= check_varargin(varargin, munits, d, sMap.name);
  catch
    error(lasterr);
  end
end

% set defaults for general properties

if isempty(General.colorbardir)
  General.colorbardir='vert';
end

if isempty(General.scale)
  General.scale='denormalized';
end

if isempty(General.size)
  General.size=1;
end

if isempty(General.edgecolor)
  General.edgecolor='none';
end

%% Action %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get rid of an annoying warning: "RGB color data not yet supported in
% Painter's mode."
%set(gcf, 'renderer','zbuffer'); 
%% -> a much more annoying thing results: the output to PostScript is 
%%    as bitmap, making the files over 6MB in size... 

n=length(Plane);                     % the number of subfigures

% get the unique component indices
c=General.comp(General.comp>0);
c=setdiff(unique(c),[0 -1]); 
c=c(~isnan(c));                   

% estimate the suitable dimension for
if isempty(General.subplots), 
  y=ceil(sqrt(n));                   % subplots
  x=ceil(n/y);
else
  y = General.subplots(2); 
  x = General.subplots(1); 
  if y*x<n, 
    error(['Given subplots grid size is too small: should be >=' num2str(n)]); 
  end    
end

clf;                               % clear figure

for i=1:n,                         % main loop
  h_axes(i,1)=subplot(x,y,i);      % open a new subplot
  
  % Main switch: select function according to the flags set in comps  

  switch Plane{i}.mode
  
  case 'comp'
    %%% Component plane

    tmp_h=som_cplane(lattice,msize, sMap.codebook(:,General.comp(i)), ...
		     General.size);
    set(tmp_h,'EdgeColor', General.edgecolor);
    set(h_axes(i),'Tag','Cplane');
    h_label(i,1)=xlabel(sMap.comp_names{General.comp(i)});
    

  case 'compi'
    %%% Component plane (interpolated shading)
    
    tmp_h=som_grid(lattice, msize, 'surf', sMap.codebook(:,Plane{i}.value), ...
	'Marker', 'none', 'Line', 'none');
    set(h_axes(i),'Tag','CplaneI');
    h_label(i,1)=xlabel(sMap.comp_names(Plane{i}.value));
    vis_PlaneAxisProperties(gca,lattice,msize,NaN);
  
  case 'color'
    %%% Color plane

    tmp_h=som_cplane(lattice,msize,Plane{i}.value,General.size);
    set(tmp_h,'EdgeColor','none');
    set(h_axes(i),'Tag','Cplane');
    h_label(i,1)=xlabel(Plane{i}.name);
    
      
  case 'colori'
    %%% Color plane (interpolated shading)
    
    tmp_h=som_grid(lattice, msize, 'surf', Plane{i}.value, 'Marker', 'none', ...
	'Line', 'none');
    set(h_axes(i),'Tag','CplaneI');
    h_label(i,1)=xlabel(Plane{i}.name);
    vis_PlaneAxisProperties(gca,lattice,msize,NaN);
  
  case 'empty'      
    %%% Empty plane
    
    tmp_h=som_cplane(lattice,msize,'none');
    h_label(i,1)=xlabel(Plane{i}.name);
    set(h_axes(i),'Tag','Cplane');
    
  case 'umat'
    %%% Umatrix  
    
    u=som_umat(sMap.codebook(:,Plane{i}.value),sMap.topol,'median',...
	'mask',sMap.mask(Plane{i}.value)); u=u(:);
    tmp_h=som_cplane([lattice 'U'],msize,u);
    set(tmp_h,'EdgeColor','none');
    set(h_axes(i),'Tag','Uplane');
    h_label(i,1)=xlabel(Plane{i}.name);

  case 'umati'
    %%% Umatrix (interpolated shading) 
    
    u=som_umat(sMap.codebook(:,Plane{i}.value),sMap.topol,'mean',...
	'mask',sMap.mask(Plane{i}.value)); u=u(1:2:end,1:2:end);
    u=u(:);
    tmp_h=som_grid('rect', msize, 'surf', u, ...
	'Marker', 'none', 'Line', 'none', ...
	'coord', som_vis_coords(lattice,msize));
    set(h_axes(i),'Tag','UplaneI');
    h_label(i,1)=xlabel(Plane{i}.name);
    vis_PlaneAxisProperties(gca,lattice,msize,NaN);
    
  otherwise
    error('INTERNAL ERROR: unknown visualization mode.');
  end

  %%% Adjust axis ratios to optimal (only 2D!) and put the
  %%% title as close to axis as possible

  set(h_label,'Visible','on','verticalalignment','top');
  set(gca,'plotboxaspectratio',[msize(2) msize(1) msize(1)]);
  
  %%% Draw colorbars if they are turned on and the plane is umat or c-plane

  if General.comp(i)> -1 & ~strcmp(General.colorbardir,'none'),
    h_colorbar(i,1)=colorbar(General.colorbardir);           % colorbars
  else
    h_colorbar(i,1)=-1;
    General.comp(i)=-1;
  end
end         %% main loop ends
  
% Set window name

set(gcf,'Name',[ 'Map name: ' sMap.name]);

%% Set axes handles to the UserData field (for som_addxxx functions
%% and som_recolorbar) 
%% set component indexes and normalization struct for som_recolorbar

SOM_SHOW.subplotorder=h_axes;
SOM_SHOW.msize=msize;
SOM_SHOW.lattice=lattice;
SOM_SHOW.dim=d;
SOM_SHOW.comps=General.comp;
SOM_SHOW.comp_norm=sMap.comp_norm; %(General.comp(find(General.comp>0)));

set(gcf,'UserData', SOM_SHOW);

% Set text property 'interp' to 'none' in title texts

set(h_label,'interpreter','none');

h_colorbar=som_recolorbar('all', 3, General.scale);   %refresh colorbars

% Set a movable text to lower corner pointsize 12.

vis_footnote(General.footnote);  vis_footnote(12);  

% set colormap
colormap(General.colormap);

%% Build output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

if nargout > 0
  h.plane=h_axes; h.colorbar=h_colorbar; h.label=h_label;
end


%%%%%% SUBFUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [Plane, General]=check_varargin(args, munits, dim, name)

% args: varargin of the main function
% munits: number of map units
% dim: map codebook dimension
% name: map name
% Define some variables (they must exist later)

Plane={};           % stores the visualization data for each subplot
General.comp=[];    % information stored on SOM_SHOW figure (which component)
General.size=[];            % unit size
General.scale=[];           % normalization
General.colorbardir=[];     % colorbar direction
General.edgecolor=[];       % edge colors
General.footnote=name;      % footnote text
General.colormap=colormap;  % default colormap (used to be gray(64).^.5;)
General.subplots=[];        % number of subplots in y- and x-directions

for i=1:2:length(args),
  %% Check that all argument types are strings
  
  if ~ischar(args{i}),
    error('Invalid input identifier names or input argument order.');
  end
  
  %% Lower/uppercase in identifier types doesn't matter: 
  
  identifier=lower(args{i});     % identifier (lowercase)
  value=args{i+1};
  
  %%% Check first the identifiers that define planes and get values
  %%% to the visualization data struct array Plane.
  %%% (comps,compi,umat,color,empty) Note that name, value and comp_
  %%% must be specified in these cases 
  %%% comp_ are collected to comp in order. This is stored to the
  %%% SOM_SHOW user property field to give information for SOM_RECOLROBAR
  %%% how to operate, i.e., which component is in which subplot:
  %%% comp(i)=0: draw colorbar, but no normalization (umat) 
  %%% comp(i)=1...N: a component plane of variable comp(i)
  %%% comp(i)=-1: no colorbar (color or empty plane)    
  
  switch identifier  
   case {'comp','compi'}
    %%% Component planes: check values & set defaults
    
    if ~vis_valuetype(value,{'nx1','1xn','string'}) & ~isempty(value),
      error([ 'A vector argument or string ''all'' expected for ''' ...
	      identifier '''.'])
    end
    if isempty(value) 
      value=1:dim;
    elseif ischar(value), 
      if ~strcmp(value,'all')
	error([ 'Only string value ''all'' is valid for ''' ...
		identifier '''.']);
      else
	value=1:dim;
      end
    else
      value=round(value);
      if min(value)<1 | max(value)>dim,
	error([ 'Component indices out of range in ''' identifier '''.']) 
      end
    end
    if size(value,1)==1, value=value';end
    comp_=value; 
    name=[]; % name is taken form sMap by index in main loop 
    
  case {'umat','umati'}
    %%% Check first the possible cell input
    
    if iscell(value),
      if ndims(value) ~= 2 | any(size(value) ~= [1 2]) | ...
	    ~vis_valuetype(value{2},{'string'}),
	error('Cell input for ''umat'' has to be of form {vector, string}.');
      else
	name=value{2}; value=value{1};
      end
    else 
      name='U-matrix'; % no cell: default title is set
    end
    if ~vis_valuetype(value,{'nx1','1xn','string'}) & ~isempty(value),
      error('Vector, string ''all'', or cell {vector, string} expected for ''umat''.')
    end
    if isempty(value)
      value=1:dim;
    elseif ischar(value), 
      if ~strcmp(value,'all')
	error('Only string value ''all'' is valid for ''umat''.')
      else
	value=1:dim;
      end
    else
      value=unique(round(value));
    end
    if min(value)<1 | max(value)>dim,
      error('Component indices out of range in ''umat''.') 
    end
    
    if size(value,1)==1, value=value';end
    comp_=0;
    
  case 'empty'
    %%% Empty plane: check values & set defaults
    
    if ~vis_valuetype(value,{'string'}), 
      error('A string value for title name expected for ''empty''.');
    end
    name=value;
    comp_=-1;
    
  case { 'color','colori'}
    %%% Color plane: check values & set defaults
    
    % Check first the possible cell input
    if iscell(value),
      if ndims(value)~=2 | any(size(value) ~= [1 2]) | ...
	    ~vis_valuetype(value{2},{'string'}),
	error([ 'Cell input for ''' identifier ...
	      ''' has to be of form {M, string}.']);
      else
	name=value{2}; value=value{1};
      end
    else 
      name='Color code'; % no cell: default title is set
    end
    if size(value,1)~=munits | ...
	  (~vis_valuetype(value,{'nx3rgb'}) & ... 
	   ~vis_valuetype(value,{'nx1'}) & ...
	   ~vis_valuetype(value,{'nx1xm'}) & ...
	   ~vis_valuetype(value,{'nx3xdimrgb'})),
      error(['Mx3 or Mx3xN RGBmatrix, Mx1 or Mx1xN matrix, cell '...
	     '{RGBmatrix, string},' ...
	     ' or {matrix, string} expected for ''' identifier '''.']);
    end

    % if colormap is fixed, we don't draw colorbar (comp_ flag is -1)
    % if colormap is indexed, we draw colorbar as in umat (comp_=0)

    if size(value,2)==3
      comp_=-1;
    else
      comp_=0;
    end
    
    %%%% The next things are general properties of the visualization---
    %%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
  case 'size'
    %%% Unit size: check & set
  
    if ~vis_valuetype(value,{'1x1',[munits 1]})
      error('A munits x 1 vector or a scalar expected for ''size''.')
    end
    if isempty(value),
      General.size=1;
    else
      General.size=value;
    end
    
   case 'bar'
    %%% Colorbar existence & direction: check & set
    
    if ~vis_valuetype(value,{'string'})
      error('String value expected for ''bar''.')
    elseif isempty(value)
      value='vert';
    end
    if any(strcmp(value,{'vert','horiz','none'})),
      General.colorbardir=value;
    else
      error('String ''vert'', ''horiz'' or ''none'' expected for ''bar''.');
    end
    
  case 'norm' 
    %%% Value normalization: check & set
    
    if ~vis_valuetype(value,{'string'})
      error('String ''n'' or ''d'' expected for ''norm''.');
    elseif isempty(value)
      value='n';
    end
    if strcmp(value(1),'n'), 
      General.scale='normalized';
    elseif strcmp(value(1),'d'),
      General.scale='denormalized';
    else
      error('String ''n(ormalized)'' or ''d(enormalized)'' expected for ''norm''.');
    end
    
  case 'edge'
    %%% Edge on or off : check % set 
    
    if ~vis_valuetype(value,{'string'}) & ~isempty(value),
      error('String value expected for ''edge''.')
    elseif ~isempty(value),
      switch value
      case 'on'
	General.edgecolor='k';
      case 'off' 
	General.edgecolor='none';
      otherwise
	error('String value ''on'' or ''off'' expected for ''edge''.')  
      end
    end
    
  case 'footnote'
    %%% Set the movable footnote text  
    
    if ~vis_valuetype(value,{'string'}) 
      if ~isempty(value),
	error('String value expected for ''footnote''.');
      else
	General.footnote=sMap.name;
      end
    else
      General.footnote=value;
    end

   case 'colormap'
    %%% Set the colormap
    if isempty(value)
      General.colormap=gray(64).^2;
    elseif ~vis_valuetype(value,{'nx3rgb'})
      error('Colormap is invalid!');
    else
      General.colormap=value;
    end
    
   case 'subplots'
    %%% set the number of subplots
    if ~vis_valuetype(value,{'1x2'}) & ~vis_valuetype(value,{'2x1'})
      error('Subplots grid size is invalid!');
    else
      General.subplots=value; 
    end
    
  otherwise
    %%% Unknown identifier
    
    error(['Invalid argument identifier ''' identifier '''!']);
  end
  
  %%% Set new entry to the Plane array if the indentifier means 
  %%% making a new plane/planes
  
  tail=length(Plane);
  switch identifier
  case {'comp','compi'}
    for i=1:length(value)
      Plane{tail+i}.mode=identifier;
      Plane{tail+i}.value=value(i);
      Plane{tail+i}.name=name; % not used actually
    end
    General.comp = [General.comp; comp_];
   case {'umat','umati','empty'}
    Plane{tail+1}.mode=identifier;
    Plane{tail+1}.value=value;
    Plane{tail+1}.name=name;
    General.comp = [General.comp; comp_];
   case {'color','colori'},
    for i=1:size(value,3),
      Plane{tail+i}.mode=identifier;
      Plane{tail+i}.name=[name '_' num2str(i)];
      Plane{tail+i}.value=value(:,:,i);
      General.comp = [General.comp; comp_];
    end
    if size(value,3)==1,
      Plane{tail+1}.name=name;
    end
  otherwise
    ; % do nothing
  end
end



function [sS, ok, msgs] = som_set(sS, varargin)

%SOM_SET Create and check SOM Toolbox structs, give values to their fields.
%
% [sS, ok, msgs] = som_set(sS, [field, contents, ...])
%
%   sM              = som_set(sM,'name','SOM#1.1');
%   [dummy,ok,msgs] = som_set(sData);   
%   sT              = som_set('som_topol','msize',[10 10],'lattice','hexa');
%   [sTrain,ok]     = som_set(sTrain,'algorithm','lininit');
%   [sN,ok,msgs]    = som_set('som_norm');
%
% Input and output arguments ([]'s are optional):
%  sS                   the target struct
%              (struct) a SOM Toolbox structure (not visualization struct)
%              (string) structure identifier (see below)
%                       the updated/created structure is returned
%  [field,     (string) field to be given value to (see below)
%   contents]  (varies) the contents for the field
%
%  ok          (vector)  status for each field-contents pair (1=ok)
%  msgs        (cellstr) status string for each field-contents pair (''=ok)
%
%  There can be arbitrarily many field-contents pairs. If there
%  are _no_ field-content pairs, and the first argument is a struct,
%  the fields of the struct are checked for validity.
% 
%  Valid struct and corresponding field identifiers: 
%  'som_map'   : 'codebook', 'labels', 'mask', 'neigh', 'name', 
%                'topol', 'msize, 'lattice', 'shape',
%                'trainhist', 'comp_names', 'comp_norm', 
%  'som_data'  : 'data', 'labels', 'name', 'comp_names', 'comp_norm', 
%                'label_names'
%  'som_topol' : 'msize', 'lattice', 'shape'
%  'som_norm'  : 'method', 'params', 'status'
%  'som_train' : 'algorithm', 'data_name', 'mask', 'neigh', 
%                'radius_ini', 'radius_fin', 'alpha_ini', 'alpha_type', 
%                'trainlen', 'time'
%  'som_grid'  : 'lattice', 'shape', 'msize', 'coord',
%                'line', 'linecolor', 'linewidth', 
%                'marker', 'markersize', 'markercolor', 'surf', 
%                'label', 'labelcolor', 'labelsize'
%                checking given values has not been implemented yet!
%                
% For more help, try 'type som_set' or check out online documentation.
% See also SOM_INFO, SOM_MAP_STRUCT, SOM_DATA_STRUCT, SOM_VS1TO2.

%%%%%%%%%%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% som_set
%
% PURPOSE
%
% Create and set values for fields of SOM Toolbox structs (except
% visualization struct). Can also be used to check the validity of structs.
%
% SYNTAX
%
%  sMap   = som_set('som_map');
%  sData  = som_set(sData);
%  sNorm  = som_set(...,'field',contents,...);
%  [sTopol,ok]      = som_set(sTopol,...);
%  [sTrain,ok,msgs] = som_set('som_train',...);
%
% DESCRIPTION
%
% The function is used to create and set values for fields of SOM
% Toolbox structs, except visualization structs. The given values are
% first checked for validity, and if they are not valid, an error
% message is returned. The function can also be used to check the
% validity of all the fields of the struct by supplying a struct as
% the first and only argument.
% 
% NOTE: Using SOM_SET to create structures does _not_ guarantee that the
% structs are valid (try e.g. sM = som_set('som_map'); som_set(sM)). The
% initial values that the function gives to the fields of the structs are
% typically invalid. It is recommended that when creating map or data 
% structs, the corresponding functions SOM_MAP_STRUCT and SOM_DATA_STRUCT 
% are used instead of SOM_SET. However, when giving values for the fields, 
% SOM_SET tries to guarantee that the values are valid.
%
% If a string is given as the first argument, the corresponding 
% structure is first created and the field-content pairs are then
% applied to it. 
%
% There can be arbitrarily many field-contents pairs. The pairs
% are processed sequentially one pair at a time. For each pair,
% the validity of the contents is checked and the corresponding 
% items in the returned 'ok'-vector and 'msgs'-cellstring are set.
% - if the contents is ok, the status is set to 1 and message to ''
% - if the contents is suspicious, status is set to 1, but a
%   message is produced
% - if the contents is invalid, status is set to 0 and an error
%   message is produced. The contents are _not_ given to the field.
% If there is only one output argument, the status and messages
% for each pair are printed to standard output.
%
% The different field-contents pairs have no effect on each other.
% If a field is given a value multiple times, the last valid one 
% stays in effect.
% 
% In some cases, the order of the given fields is significant.
% For example in the case of 'som_map', the validity of some fields, 
% like '.comp_names', depends on the input space dimension, which is
% checked from the '.data' field (dim = size(sD.data,2) to be specific).
% Therefore, the '.data' field (or '.codebook' field in case of map 
% struct) should always be given a value first. Below is a list of 
% this kind of dependancies:
% 
% som_map:   'comp_names', 'comp_norm', 'msize', 'topol.msize',
%            'labels' and 'mask' depend on 'codebook'
%            new value for 'codebook' should have equal size to the old
%            one (unless the old one was empty)
% som_data:  'comp_names' and 'comp_norm' depend on 'data'
%            new value for 'data' should have equal dimension (size(data,2))
%            as the old one (unless the old one was empty)
% 
% KNOWN BUGS
%
% Checking the values given to som_grid struct has not been
% implemented. Use SOM_GRID function to give the values.
%
% REQUIRED INPUT ARGUMENTS
%
%  sS          The struct.
%     (struct) A SOM Toolbox struct.
%     (string) Identifier of a SOM Toolbox struct: 'som_map', 
%              'som_data', 'som_topol', 'som_norm' or 'som_train'
%   
% OPTIONAL INPUT ARGUMENTS 
%
%  field     (string) Field identifier string (see below).
%  contents  (varies) Value for the field (see below).
%
%  Below is the list of valid field identifiers for the different 
%  SOM Toolbox structs. 
%
%  'som_map' (map struct)
%    'codebook'    : matrix, size [munits, dim] 
%    'labels'      : cell array of strings, 
%                    size [munits, maximum_number_of_labels]
%    'topol'       : topology struct (prod(topol.msize)=munits)
%    'mask'        : vector, size [dim, 1]
%    'neigh'       : string ('gaussian' or 'cutgauss' or 'bubble' or 'ep')
%    'trainhist'   : struct array of train structs
%    'name'        : string
%    'comp_names'  : cellstr, size [dim, 1], e.g. {'c1','c2','c3'}
%    'comp_norm'   : cell array, size [dim, 1], of cell arrays 
%                    of normalization structs
%    Also the following can be used (although they are fields
%    of the topology struct)
%    'msize'       : vector (prod(msize)=munits)
%    'lattice'     : string ('rect' or 'hexa')
%    'shape'       : string ('sheet' or 'cyl' or 'toroid')
%
%  'som_data' (data struct)
%    'data'        : matrix, size [dlen, dim]
%    'name'        : string
%    'labels'      : cell array of strings, 
%                    size [dlen, m]
%    'comp_names'  : cellstr, size [dim, 1], e.g. {'c1','c2','c3'}
%    'comp_norm'   : cell array, size [dim, 1], of cell arrays 
%                    of normalization structs
%    'label_names' : cellstr, size [m, 1]
%
% 'som_topol' (topology struct)
%    'msize'       : vector
%    'lattice'     : string ('rect' or 'hexa')
%    'shape'       : string ('sheet' or 'cyl' or 'toroid')
%
% 'som_norm' (normalization struct)
%    'method'      : string
%    'params'      : varies
%    'status'      : string ('done' or 'undone' or 'uninit')
%
% 'som_train' (train struct)
%    'algorithm'   : string ('seq' or 'batch' or 'lininit' or 'randinit')
%    'data_name'   : string
%    'mask'        : vector, size [dim, 1]
%    'neigh'       : string ('gaussian' or 'cutgauss' or 'bubble' or 'ep')
%    'radius_ini'  : scalar
%    'radius_fin'  : scalar
%    'alpha_ini'   : scalar
%    'alpha_type'  : string ('linear' or 'inv' or 'power')
%    'trainlen'    : scalar
%    'time'        : string
%
% 'som_grid' (grid struct) : checking the values has not been implemented yet!
%    'lattice'     : string ('rect' or 'hexa') or 
%                    (sparce) matrix, size munits x munits
%    'shape'       : string ('sheet' or 'cyl' or 'toroid')
%    'msize'       : vector, size 1x2
%    'coord'       : matrix, size munits x 2 or munits x 3
%    'line'        : string (linespec, e.g. '-', or 'none')
%    'linecolor'   : RGB triple or string (colorspec, e.g. 'k') or 
%                    munits x munits x 3 (sparce) matrix or cell
%                    array of RGB triples 
%    'linewidth'   : scalar or munits x munits (sparce) matrix
%    'marker'      : string (markerspec, e.g. 'o', or 'none') or 
%                    munits x 1 cell or char array of these
%    'markersize'  : scalar or munits x 1 vector
%    'markercolor' : RGB triple or string (colorspec, e.g. 'k')
%    'surf'        : [], munits x 1 or munits x 3 matrix of RGB triples
%    'label'       : [] or munits x 1 char array or 
%                    munits x l cell array of strings 
%    'labelcolor'  : RGB triple or string (colorspec, e.g. 'g' or 'none')
%    'labelsize'   : scalar
%
% OUTPUT ARGUMENTS
% 
%  sS    (struct)  the created / updated struct
%  ok    (vector)  length = number of field-contents pairs, gives
%                  validity status for each pair (0=invalid, 1 otherwise)
%  msgs  (cellstr) length = number of field-contents pairs, gives
%                  error/warning message for each pair ('' if ok)
%
% EXAMPLES
%
% To create a struct:
%  sM  = som_set('som_map');
%  sD  = som_set('som_data');
%  sTo = som_set('som_topol');
%  sTr = som_set('som_train');
%  sN  = som_set('som_norm');
%  sG  = som_set('som_grid');
%
% To check the the contents of a struct: 
%  som_set(sS);
%  [dummy,ok]      = som_set(sS);
%  [dummy,ok,msgs] = som_set(sS);
%
% To give values to fields: 
%  sTo = som_set(sTo,'msize',[10 10],'lattice','hexa','shape','toroid');
%  sM  = som_set('som_map','codebook',rand(100,4),'topol',sTo);
%   
% SEE ALSO
% 
%  som_info         Prints information the given struct.
%  som_map_struct   Create map struct.
%  som_data_struct  Create data struct.
%  som_topol_struct Create topology struct.
%  som_train_struct Create training struct.
%  som_grid         Create and visualize grid struct.
%  som_vs1to2       Conversion from version 1.0 structs to 2.0.
%  som_vs2to1       Conversion from version 2.0 structs to 1.0.

% Copyright (c) 1999-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/

% Version 2.0beta juuso 101199 130300

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create struct if necessary

if ischar(sS), 
  switch sS
   case 'som_map',
    sS=struct('type', 'som_map', ...
              'codebook', [], ...
              'topol', som_set('som_topol'), ...
              'labels', cell(1), ...
              'neigh', 'gaussian', ...
              'mask', [], ...
              'trainhist', cell(1), ...
              'name', '',...
              'comp_names', {''}, ...
              'comp_norm', cell(1));
   case 'som_data', 
    sS=struct('type', 'som_data', ...
              'data', [], ...
              'labels', cell(1), ...
              'name', '', ...
              'comp_names', {''}, ...
              'comp_norm', cell(1), ...
              'label_names', []);
   case 'som_topol',
    sS=struct('type', 'som_topol', ...
              'msize', 0, ...
              'lattice', 'hexa', ...
              'shape', 'sheet');
   case 'som_train',
    sS=struct('type', 'som_train', ...
              'algorithm', '', ...
              'data_name', '', ...
              'neigh', 'gaussian', ...
              'mask', [], ...
              'radius_ini', NaN, ...
              'radius_fin', NaN, ...
              'alpha_ini', NaN, ...
              'alpha_type', 'inv', ...
              'trainlen', NaN, ...
              'time', '');
   case 'som_norm',
    sS=struct('type', 'som_norm', ...
              'method', 'var', ...
              'params', [], ...
              'status', 'uninit');
   case 'som_grid', 
    sS=struct('type','som_grid',...
	      'lattice','hexa',...
	      'shape','sheet',...
	      'msize',[1 1],...
	      'coord',[],...
	      'line','-',...
	      'linecolor',[.9 .9 .9],...
	      'linewidth',0.5,...
	      'marker','o',...
	      'markersize',6,...
	      'markercolor','k',...
	      'surf',[],...
	      'label',[],...
	      'labelcolor','g',...
	      'labelsize',12);    
   otherwise
    ok=0; msgs = {['Unrecognized struct type: ' sS]}; sS = [];
    return;
  end  
  
elseif isstruct(sS) & length(varargin)==0, 
  
  % check all fields
  fields = fieldnames(sS);
  if ~any(strcmp('type',fields)), 
    error('The struct has no ''type'' field.');
  end
  k = 0;
  for i=1:length(fields), 
    contents = getfield(sS,fields{i});
    if ~strcmp(fields{i},'type'), 
      varargin{k+1} = fields{i};
      varargin{k+2} = contents;
      k = k + 2;
    else 
      if ~any(strcmp(contents, ...
        {'som_map','som_data','som_topol','som_train','som_norm'})), 
	error(['Unknown struct type: ' contents]);
      end
    end
  end
  
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set field values

p = ceil(length(varargin)/2);
ok = ones(p,1);
msgs = cell(p,1);

for i=1:p, 
  field = varargin{2*i-1}; 
  content = varargin{2*i};
  msg = '';
  isok = 0;
  
  si = size(content);
  isscalar = (prod(si)==1);
  isvector = (sum(si>1)==1);
  isrowvector = (isvector & si(1)==1);
  if isnumeric(content), 
    iscomplete = all(~isnan(content(:))); 
    ispositive = all(content(:)>0); 
    isinteger  = all(content(:)==ceil(content(:)));
    isrgb = all(content(:)>=0 & content(:)<=1) & size(content,2)==3;
  end
  
  switch sS.type, 
   case 'som_map',
    [munits dim] = size(sS.codebook);
    switch field, 
     case 'codebook', 
      if ~isnumeric(content), 
	msg = '''codebook'' should be a numeric matrix'; 
      elseif size(content) ~= size(sS.codebook) & ~isempty(sS.codebook), 
	msg = 'New ''codebook'' must be equal in size to the old one.'; 
      elseif ~iscomplete, 
	msg = 'Map codebook must not contain NaN''s.'; 
      else
	sS.codebook = content; isok=1;
      end
     case 'labels', 
      if isempty(content), 
	sS.labels = cell(munits,1); isok = 1;
      elseif size(content,1) ~= munits, 
	msg = 'Length of labels array must be equal to the number of map units.';
      elseif ~iscell(content) & ~ischar(content), 
	msg = '''labels'' must be a string array or a cell array/matrix.';
      else
	isok = 1;
	if ischar(content), content = cellstr(content); 
	elseif ~iscellstr(content), 
	  l = prod(size(content));
	  for j=1:l, 
	    if ischar(content{j}), 
	      if ~isempty(content{j}), 
		msg = 'Invalid ''labels'' array.';
		isok = 0; 
		break;
	      else
		content{j} = ''; 
	      end
	    end
	  end
	end
	if isok, sS.labels = content; end
      end
     case 'topol', 
      if ~isstruct(content), 
	msg = '''topol'' should be a topology struct.'; 
      elseif ~isfield(content,'msize') | ...
	    ~isfield(content,'lattice') | ...
	    ~isfield(content,'shape'), 
	msg = '''topol'' is not a valid topology struct.'; 
      elseif prod(content.msize) ~= munits, 
	msg = '''topol''.msize does not match the number of map units.'; 
      else
	sS.topol = content; isok = 1;
      end
     case 'msize', 
      if ~isnumeric(content) | ~isvector | ~ispositive | ~isinteger, 
	msg = '''msize'' should be a vector with positive integer elements.'; 
      elseif prod(content) ~= munits, 
	msg = '''msize'' does not match the map size.'; 
      else
	sS.topol.msize = content; isok = 1;
      end
     case 'lattice', 
      if ~ischar(content),
	msg = '''lattice'' should be a string'; 
      elseif ~strcmp(content,'rect') & ~strcmp(content,'hexa'),
	msg = ['Unknown lattice type: ' content];
	sS.topol.lattice = content; isok = 1;
      else
	sS.topol.lattice = content; isok = 1;
      end
     case 'shape', 
      if ~ischar(content),
	msg = '''shape'' should be a string';
      elseif ~strcmp(content,'sheet') & ~strcmp(content,'cyl') & ...
	    ~strcmp(content,'toroid'),
	msg = ['Unknown shape type:' content]; 
	sS.topol.shape = content; isok = 1;
      else
	sS.topol.shape = content; isok = 1;
      end
     case 'neigh', 
      if ~ischar(content),
	msg = '''neigh'' should be a string'; 
      elseif ~strcmp(content,'gaussian') & ~strcmp(content,'ep') & ...
	    ~strcmp(content,'cutgauss') & ~strcmp(content,'bubble'),
	msg = ['Unknown neighborhood function: ' content]; 
	sS.neigh = content; isok = 1;
      else
	sS.neigh = content; isok = 1;
      end
     case 'mask', 
      if size(content,1) == 1, content = content'; end
      if ~isnumeric(content) | size(content) ~= [dim 1], 
	msg = '''mask'' should be a column vector (size dim x 1).'; 
      else
	sS.mask = content; isok = 1;
      end
     case 'name', 
      if ~ischar(content), 
	msg = '''name'' should be a string.';
      else 
	sS.name = content; isok = 1;
      end
     case 'comp_names', 
      if ~iscell(content) & ~ischar(content), 
	msg = '''comp_names'' should be a cell string or a string array.'; 
      elseif length(content) ~= dim, 
	msg = 'Length of ''comp_names'' should be equal to dim.'; 
      else
	if ischar(content), content = cellstr(content); end
	if size(content,1)==1, content = content'; end
	sS.comp_names = content;
	isok = 1;
      end        
     case 'comp_norm', 
      if ~iscell(content) & length(content)>0, 
	msg = '''comp_norm'' should be a cell array.'; 
      elseif length(content) ~= dim, 
	msg = 'Length of ''comp_norm'' should be equal to dim.'; 
      else
	isok = 1;
	for j=1:length(content), 
	  if ~isempty(content{j}) & (~isfield(content{j}(1),'type') | ...
				     ~strcmp(content{j}(1).type,'som_norm')), 
	    msg = 'Each cell in ''comp_norm'' should be either empty or type ''som_norm''.';
	    isok = 0; 
	    break; 
	  end
	end
	if isok, sS.comp_norm = content; end
      end        
     case 'trainhist', 
      if ~isstruct(content) & ~isempty(content), 
	msg = '''trainhist'' should be a struct array or empty.';
      else
	isok = 1;
	for j=1:length(content), 
	  if ~isfield(content(j),'type') | ~strcmp(content(j).type,'som_train'), 
	    msg = 'Each cell in ''trainhist'' should be of type ''som_train''.';
	    isok = 0; 
	    break; 
	  end
	end
	if isok, sS.trainhist = content; end      
      end        
     otherwise, 
      msg = ['Invalid field for map struct: ' field]; 
    end
    
   case 'som_data',
    [dlen dim] = size(sS.data);
    switch field,      
     case 'data', 
      [dummy dim2] = size(content);
      if prod(si)==0, 
	msg = '''data'' is empty';
      elseif ~isnumeric(content), 
	msg = '''data'' should be numeric matrix.'; 
      elseif dim ~= dim2 & ~isempty(sS.data), 
	msg = 'New ''data'' must have the same dimension as old one.'; 
      else
	sS.data = content; isok = 1;
      end
     case 'labels', 
      if isempty(content), 
	sS.labels = cell(dlen,1); isok = 1;
      elseif size(content,1) ~= dlen, 
	msg = 'Length of ''labels'' must be equal to the number of data vectors.';
      elseif ~iscell(content) & ~ischar(content), 
	msg = '''labels'' must be a string array or a cell array/matrix.';
      else
	isok = 1;
	if ischar(content), content = cellstr(content); 
	elseif ~iscellstr(content), 
	  l = prod(size(content));
	  for j=1:l, 
	    if ~ischar(content{j}), 
	      if ~isempty(content{j}), 
		msg = 'Invalid ''labels'' array.';
		isok = 0; j
		break;
	      else
		content{j} = ''; 
	      end
	    end
	  end
	end
	if isok, sS.labels = content; end
      end
     case 'name', 
      if ~ischar(content), 
	msg = '''name'' should be a string.';
      else 
	sS.name = content; isok = 1;
      end
     case 'comp_names', 
      if ~iscell(content) & ~ischar(content), 
	msg = '''comp_names'' should be a cell string or a string array.'; 
      elseif length(content) ~= dim, 
	msg = 'Length of ''comp_names'' should be equal to dim.'; 
      else
	if ischar(content), content = cellstr(content); end
	if size(content,1)==1, content = content'; end
	sS.comp_names = content;
	isok = 1;
      end        
     case 'comp_norm', 
      if ~iscell(content) & length(content)>0, 
	msg = '''comp_norm'' should be a cell array.'; 
      elseif length(content) ~= dim, 
	msg = 'Length of ''comp_norm'' should be equal to dim.'; 
      else
	isok = 1;
	for j=1:length(content), 
	  if ~isempty(content{j}) & (~isfield(content{j}(1),'type') | ...
				     ~strcmp(content{j}(1).type,'som_norm')), 
	    msg = 'Each cell in ''comp_norm'' should be either empty or type ''som_norm''.';
	    isok = 0; 
	    break; 
	  end
	end
	if isok, sS.comp_norm = content; end
      end        
     case 'label_names', 
      if ~iscell(content) & ~ischar(content) & ~isempty(content), 
	msg = ['''label_names'' should be a cell string, a string array or' ...
	       ' empty.']; 
      else
	if ~isempty(content), 
	  if ischar(content), content = cellstr(content); end
	  if size(content,1)==1, content = content'; end
	end
	sS.label_names = content;
	isok = 1;
      end        
     otherwise, 
      msg = ['Invalid field for data struct: ' field]; 
    end
    
   case 'som_topol', 
    switch field,      
     case 'msize', 
      if ~isnumeric(content) | ~isvector | ~ispositive | ~isinteger, 
	msg = '''msize'' should be a vector with positive integer elements.'; 
      else
	sS.msize = content; isok=1;
      end
     case 'lattice', 
      if ~ischar(content),
	msg = '''lattice'' should be a string'; 
      elseif ~strcmp(content,'rect') & ~strcmp(content,'hexa'),
	msg = ['Unknown lattice type: ' content]; 
	sS.lattice = content; isok = 1;
      else
	sS.lattice = content; isok = 1;
      end
     case 'shape', 
      if ~ischar(content),
	msg = '''shape'' should be a string';
      elseif ~strcmp(content,'sheet') & ~strcmp(content,'cyl') & ...
	    ~strcmp(content,'toroid'),
	msg = ['Unknown shape type: ' content]; 
	sS.shape = content; isok = 1;
      else
	sS.shape = content; isok = 1;
      end
     otherwise, 
      msg = ['Invalid field for topology struct: ' field]; 
    end
    
   case 'som_train', 
    switch field,      
     case 'algorithm', 
      if ~ischar(content),
	msg = '''algorithm'' should be a string.'; 
      else
	sS.algorithm = content; isok = 1;
      end
     case 'data_name', 
      if ~ischar(content),
	msg = '''data_name'' should be a string'; 
      else
	sS.data_name = content; isok = 1;
      end
     case 'neigh', 
      if ~ischar(content),
	msg = '''neigh'' should be a string'; 
      elseif ~isempty(content) & ~strcmp(content,'gaussian') & ~strcmp(content,'ep') & ...
	    ~strcmp(content,'cutgauss') & ~strcmp(content,'bubble'),
	msg = ['Unknown neighborhood function: ' content]; 
	sS.neigh = content; isok = 1;
      else
	sS.neigh = content; isok = 1;
      end
     case 'mask', 
      if size(content,1) == 1, content = content'; end
      dim = size(content,1); %[munits dim] = size(sS.data); 
      if ~isnumeric(content) | size(content) ~= [dim 1], 
	msg = '''mask'' should be a column vector (size dim x 1).'; 
      else
	sS.mask = content; isok = 1;
      end
     case 'radius_ini', 
      if ~isnumeric(content) | ~isscalar, 
	msg = '''radius_ini'' should be a scalar.'; 
      else
	sS.radius_ini = content; isok = 1;
      end
     case 'radius_fin', 
      if ~isnumeric(content) | ~isscalar, 
	msg = '''radius_fin'' should be a scalar.'; 
      else
	sS.radius_fin = content; isok = 1;
      end
     case 'alpha_ini', 
      if ~isnumeric(content) | ~isscalar,
	msg = '''alpha_ini'' should be a scalar.'; 
      else
	sS.alpha_ini = content; isok = 1;
      end
     case 'alpha_type', 
      if ~ischar(content),
	msg = '''alpha_type'' should be a string'; 
      elseif ~strcmp(content,'linear') & ~strcmp(content,'inv') & ...
	    ~strcmp(content,'power') & ~strcmp(content,'constant') & ~strcmp(content,''),
	msg = ['Unknown alpha type: ' content]; 
	sS.alpha_type = content; isok = 1;
      else
	sS.alpha_type = content; isok = 1;
      end        
     case 'trainlen', 
      if ~isnumeric(content) | ~isscalar, 
	msg = '''trainlen'' should be a scalar.'; 
      else
	sS.trainlen = content; isok = 1;
      end
     case 'time', 
      if ~ischar(content),
	msg = '''time'' should be a string'; 
      else
	sS.time = content; isok = 1;
      end        
     otherwise, 
      msg = ['Invalid field for train struct: ' field]; 
    end
    
   case 'som_norm', 
    switch field,      
     case 'method', 
      if ~ischar(field), 
	msg = '''method'' should be a string.';
      else
	sS.method = content; isok = 1;
      end
     case 'params', 
      sS.params = content; isok = 1;
     case 'status', 
      if ~ischar(content),
	msg = '''status'' should be a string'; 
      elseif ~strcmp(content,'done') & ~strcmp(content,'undone') & ...
	    ~strcmp(content,'uninit'),
	msg = ['Unknown status type: ' content]; 
	sS.status = content; isok = 1;
      else
	sS.status = content; isok = 1;
      end        
     otherwise, 
      msg = ['Invalid field for normalization struct: ' field];
    end

   case 'som_grid', 
    if any(strcmp(field,{'lattice', 'shape', 'msize', 'coord',...
			 'line', 'linecolor', 'linewidth', ...
			 'marker', 'markersize', 'markercolor', 'surf', ... 
			 'label', 'labelcolor', 'labelsize'})), 
      warning('No checking done on field identifier or content.');
      sS = setfield(sS,field,content);       
      isok = 1;
    else
      msg = ['Invalid field for grid struct: ' field];
    end
    
   otherwise, 
    error('Unrecognized structure.'); 
    
  end
  
  msgs{i} = msg;
  ok(i) = isok;
  
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% return

if nargout < 2, 
  for i=1:p,     
    if ~isempty(msgs{i}), 
      if ~ok(i), fprintf(1,'[Error! '); 
      else fprintf(1,'[Notice ');
      end
      fprintf(1,'in setting %s] ',varargin{2*i-1});
      fprintf(1,'%s\n',msgs{i});
    end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



function flag=vis_valuetype(value, valid, str);

% VIS_VALUETYPE Used for type checks in SOM Toolbox visualization routines
%
%  flag = vis_valuetype(value, valid, str)
%
%  Input and output arguments:
%   value  (varies) variable to be checked
%   valid  (cell array) size 1xN, cells are strings or vectors (see below)
%   str    (string) 'all' or 'any' (default), determines whether
%                   all or just any of the types listed in argument 'valid' 
%                   should be true for 'value'
%
%   flag   (scalar) 1 or 0 (true or false) 
%
% This is an internal function of SOM Toolbox visualization.  It makes
% various type checks. For example:
%
%  % Return 1 if X is a numeric scalar otherwise 0:
%  f=vis_valuetype(X,{'1x1'});
%
%  % Return 1 if X is a ColorSpec, that is, a 1x3 vector presenting an RGB
%  % value or any of strings 'red','blue','green','yellow','magenta','cyan',
%  % 'white' or 'black' or their shortenings  'r','g','b','y','m','c','w','k': 
%  f=vis_valueype(X,{'1x3rgb','colorstyle'})
%
%  % Return 1 if X is _both_ 10x3 size numeric matrix and has RGB values as rows
%  f=vis_valuetype(X,{'nx3rgb',[10 3]},'all')
%
% Strings that may be used in argument valid: 
%  id             is true if value is 
% 
%  [n1 n2 ... nn] any n1 x n2 x ... x nn sized numeric matrix
%  '1x1'          scalar (numeric)
%  '1x2'          1x2 vector (numeric)
%  'nx1'          any nx1 numeric vector
%  'nx2'              nx2
%  'nx3'              nx3
%  'nxn'          any numeric square matrix
%  'nxn[0,1]'     numeric square matrix with values in interval [0,1]
%  'nxm'          any numeric matrix
%  '1xn'          any 1xn numeric vector
%  '1x3rgb'       1x3 vector v for which all(v>=0 & v<=1), e.g., a RGB code
%  'nx3rgb'       nx3 numeric matrix that contains n RGB values as rows
%  'nx3dimrgb'    nx3xdim numeric matrix that contains RGB values
%  'nxnx3rgb'     nxnx3 numeric matrix of nxn RGB triples
%  'none'         string 'none'
%  'xor'          string 'xor'
%  'indexed'      string 'indexed'
%  'colorstyle'   strings 'red','blue','green','yellow','magenta','cyan','white' 
%                 or 'black', or 'r','g','b','y','m','c','w','k'                 
%  'markerstyle'  any of Matlab's marker chars '.','o','x','+','*','s','d','v',
%                 '^','<','>','p'or 'h'
%  'linestyle'    any or Matlab's line style strings '-',':','--', or '-.'
%  'cellcolumn'   a nx1 cell array
%  'topol_cell'   {lattice, msize, shape} 
%  'topol_cell_no_shape' {lattice, msize}
%  'string'       any string (1xn array of char)  
%  'chararray'    any MxN char array

% Copyright (c) 1999-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 201099 juuso 280800

if nargin == 2
  str='any';
end

flag=0;  
sz=size(value);
dims=ndims(value);

% isnumeric
numeric=isnumeric(value);
character=ischar(value);

% main loop: go through all types in arg. 'valid'
for i=1:length(valid),
  if isnumeric(valid{i}), % numeric size for double matrix
    if numeric & length(valid{i}) == dims,
      flag(i)=all(sz == valid{i});
    else
      flag(i)=0; % not numeric or wrong dimension
    end
  else
    msg=''; % for a error message inside try 
    try 
      switch valid{i}
	
	% scalar
       case '1x1'
	flag(i)=numeric & dims == 2 & sz(1)==1 & sz(2) ==1;
	
	% 1x2 numeric vector
       case '1x2'
	flag(i)=numeric & dims == 2 & sz(1)==1 & sz(2) == 2;
	
	% 1xn numeric vector
       case '1xn'
	flag(i)=numeric & dims == 2 & sz(1) == 1;
	
	% any numeric matrix
       case 'nxm' 
	flag(i)=numeric & dims == 2;
	
	% nx3 numeric matrix 
       case 'nx3'
	flag(i)=numeric & dims == 2 & sz(2) == 3;
	
	% nx2 numeric matrix 
       case 'nx2'
	flag(i)=numeric & dims == 2 & sz(2) == 2;
	
	% nx1 numeric vector
       case 'nx1'
	flag(i)=numeric & dims == 2 & sz(2) == 1;
       
	% nx1xm numric matrix
       case 'nx1xm'
	flag(i)=numeric & dims == 3 & sz(2) == 1;
	
	% nx3 matrix of RGB triples
       case 'nx3rgb'  
	flag(i)=numeric & dims == 2 & sz(2) == 3 & in0_1(value);
	
	% RGB triple (ColorSpec vector)
       case '1x3rgb'
	flag(i) = numeric & dims == 2 & sz(1)==1 & sz(2) == 3 & in0_1(value);
	
	% any square matrix
       case 'nxn'
	flag(i)=numeric & dims == 2 & sz(1) == sz(2);
	
	% nx3xdim array of nxdim RGB triples
       case 'nx3xdimrgb'
	flag(i)=numeric & dims == 3 & sz(2) == 3 & in0_1(value);
	
	% nxnx3 array of nxn RGB triples
       case 'nxnx3rgb'
	flag(i)= numeric & dims == 3 & sz(1) == sz(2) & sz(3) == 3 ...
		 & in0_1(value);
	
	% nxn matrix of values between [0,1]
       case 'nxn[0,1]' 
	
	flag(i)=numeric & dims == 2 & sz(1) == sz(2) & in0_1(value);
	
	% string 'indexed'
       case 'indexed'
	flag(i) = ischar(value) & strcmp(value,'indexed');
	
	% string 'none'
       case 'none'
	flag(i) = character & strcmp(value,'none');
      
	% string 'xor'
       case 'xor'
	flag(i) = character & strcmp(value,'xor');
	
	% any string (1xn char array)
       case 'string'
	flag(i) = character & dims == 2 & sz(1)<=1;
	
	% any char array
       case 'chararray'
	flag(i) = character & dims == 2 & sz(1)>0;
	
	% ColorSpec string
       case 'colorstyle'
	flag(i)=(character &  sz(1) == 1 & sz(2) == 1 & ...
		 any(ismember('ymcrgbwk',value))) | ...
	(ischar(value) & any(strcmp(value,{'none','yellow','magenta',...
		    'cyan','red','green','blue','white','black'})));
	
	% any valid Matlab's Marker
       case 'markerstyle'
	flag(i)=character &  sz(1) == 1 & sz(2) == 1 & ...
		any(ismember('.ox+*sdv^<>ph',value));
	
	% any valid Matlab's LineStyle
       case 'linestyle'
	str=strrep(strrep(strrep(value,'z','1'),'--','z'),'-.','z');
	flag(i)=character & any(ismember(str,'z-:')) & sz(1)==1 & (sz(2)==1 | sz(2)==2);
	
	% any struct
       case 'struct'
	flag(i)=isstruct(value);
	
	% nx1 cell array of strings
       case 'cellcolumn_of_char'
	flag(i)=iscell(value) & dims == 2 & sz(2)==1;  
	try, char(value); catch, flag(i)=0; end
	
	% mxn cell array of strings
       case '2Dcellarray_of_char'  
	flag(i)=iscell(value) & dims == 2; 
	try, char(cat(2,value{:})); catch, flag(i)=0; end
	
	% valid {lattice, msize} 
       case 'topol_cell_no_shape'
	flag(i)=1;
	if ~iscell(value) | length(size(value)) ~= 2 | size(value,2)~=2
	  flag(i)=0;
	else
	  if vis_valuetype(value{1},{'string'}),
	    switch value{1}
	     case { 'hexa','rect'}
	      ;
	     otherwise
	      flag(i)=0;
	    end
	  end
	  if ~vis_valuetype(value{2},{'1xn'}),
	    flag(i)=0;
	  end
	end
	
	% valid {lattice, msize, shape} 
       case 'topol_cell'
	flag(i)=1;
	if ~iscell(value) | length(size(value)) ~= 2 | size(value,2) ~= 3,
	  flag(i)=0;
	else
	  if vis_valuetype(value{1},{'string'}),
	    switch value{1}
	     case { 'hexa','rect'}
	      ;
	     otherwise
	      flag(i)=0;
	    end
	  end
	  if ~vis_valuetype(value{2},{'1xn'})
	    flag(i)=0;
	  end
	  if ~vis_valuetype(value{3},{'string'})
	    flag(i)=0;
	  else
	    switch value{3}
	     case { 'sheet','cyl', 'toroid'}
	      ;
	     otherwise
	      flag(i)=0;
	    end
	  end
	end
       otherwise
	msg='Unknown valuetype!';
      end
    catch 
      % error during type check is due to wrong type of value: 
      % lets set flag(i) to 0
      flag(i)=0; 
    end
    % Unknown indetifier?
    error(msg);
  end
  % set flag according to 3rd parameter (all ~ AND, any ~ OR) 
  if strcmp(str,'all');
    flag=all(flag);
  else
    flag=any(flag);
  end
end


function f=in0_1(value)

f=all(value(:) >= 0 & value(:)<=1);


function U = som_umat(sMap, varargin)

%SOM_UMAT Compute unified distance matrix of self-organizing map.
%
% U = som_umat(sMap, [argID, value, ...])
%
%  U = som_umat(sMap);  
%  U = som_umat(M,sTopol,'median','mask',[1 1 0 1]);
%
%  Input and output arguments ([]'s are optional): 
%   sMap     (struct) map struct or
%            (matrix) the codebook matrix of the map
%   [argID,  (string) See below. The values which are unambiguous can 
%    value]  (varies) be given without the preceeding argID.
%
%   U        (matrix) u-matrix of the self-organizing map 
%
% Here are the valid argument IDs and corresponding values. The values which
% are unambiguous (marked with '*') can be given without the preceeding argID.
%   'mask'       (vector) size dim x 1, weighting factors for different 
%                         components (same as BMU search mask)
%   'msize'      (vector) map grid size
%   'topol'     *(struct) topology struct
%   'som_topol','sTopol' = 'topol'
%   'lattice'   *(string) map lattice, 'hexa' or 'rect'
%   'mode'      *(string) 'min','mean','median','max', default is 'median'
%
% NOTE! the U-matrix is always calculated for 'sheet'-shaped map and
% the map grid must be at most 2-dimensional.
% 
% For more help, try 'type som_umat' or check out online documentation.
% See also SOM_SHOW, SOM_CPLANE.

%%%%%%%%%%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% som_umat
%
% PURPOSE
%
% Computes the unified distance matrix of a SOM.
%
% SYNTAX
%
%  U = som_umat(sM)  
%  U = som_umat(...,'argID',value,...)
%  U = som_umat(...,value,...)
%
% DESCRIPTION
%
% Compute and return the unified distance matrix of a SOM. 
% For example a case of 5x1 -sized map:
%            m(1) m(2) m(3) m(4) m(5)
% where m(i) denotes one map unit. The u-matrix is a 9x1 vector:
%    u(1) u(1,2) u(2) u(2,3) u(3) u(3,4) u(4) u(4,5) u(5) 
% where u(i,j) is the distance between map units m(i) and m(j)
% and u(k) is the mean (or minimum, maximum or median) of the 
% surrounding values, e.g. u(3) = (u(2,3) + u(3,4))/2. 
%
% Note that the u-matrix is always calculated for 'sheet'-shaped map and
% the map grid must be at most 2-dimensional.
%
% REFERENCES
%
% Ultsch, A., Siemon, H.P., "Kohonen's Self-Organizing Feature Maps
%   for Exploratory Data Analysis", in Proc. of INNC'90,
%   International Neural Network Conference, Dordrecht,
%   Netherlands, 1990, pp. 305-308.
% Kohonen, T., "Self-Organizing Map", 2nd ed., Springer-Verlag, 
%    Berlin, 1995, pp. 117-119. 
% Iivarinen, J., Kohonen, T., Kangas, J., Kaski, S., "Visualizing 
%   the Clusters on the Self-Organizing Map", in proceedings of
%   Conference on Artificial Intelligence Research in Finland,
%   Helsinki, Finland, 1994, pp. 122-126.
% Kraaijveld, M.A., Mao, J., Jain, A.K., "A Nonlinear Projection
%   Method Based on Kohonen's Topology Preserving Maps", IEEE
%   Transactions on Neural Networks, vol. 6, no. 3, 1995, pp. 548-559.
% 
% REQUIRED INPUT ARGUMENTS
%
%  sM (struct) SOM Toolbox struct or the codebook matrix of the map.
%     (matrix) The matrix may be 3-dimensional in which case the first 
%              two dimensions are taken for the map grid dimensions (msize).
%
% OPTIONAL INPUT ARGUMENTS
%
%  argID (string) Argument identifier string (see below).
%  value (varies) Value for the argument (see below).
%
%  The optional arguments are given as 'argID',value -pairs. If the 
%  value is unambiguous, it can be given without the preceeding argID.
%  If an argument is given value multiple times, the last one is used. 
%
%  Below is the list of valid arguments: 
%   'mask'      (vector) mask to be used in calculating
%                        the interunit distances, size [dim  1]. Default is 
%                        the one in sM (field sM.mask) or a vector of
%                        ones if only a codebook matrix was given.
%   'topol'     (struct) topology of the map. Default is the one
%                        in sM (field sM.topol).
%   'sTopol','som_topol' (struct) = 'topol'
%   'msize'     (vector) map grid dimensions
%   'lattice'   (string) map lattice 'rect' or 'hexa'
%   'mode'      (string) 'min', 'mean', 'median' or 'max'
%                        Map unit value computation method. In fact, 
%                        eval-function is used to evaluate this, so 
%                        you can give other computation methods as well.
%                        Default is 'median'. 
%
% OUTPUT ARGUMENTS
%
%  U   (matrix) the unified distance matrix of the SOM 
%               size 2*n1-1 x 2*n2-1, where n1 = msize(1) and n2 = msize(2)
%
% EXAMPLES
%
%  U = som_umat(sM);  
%  U = som_umat(sM.codebook,sM.topol,'median','mask',[1 1 0 1]);
%  U = som_umat(rand(10,10,4),'hexa','rect'); 
% 
% SEE ALSO
%
%  som_show    show the selected component planes and the u-matrix
%  som_cplane  draw a 2D unified distance matrix

% Copyright (c) 1997-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/

% Version 1.0beta juuso 260997
% Version 2.0beta juuso 151199, 151299, 200900

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check arguments

error(nargchk(1, Inf, nargin));  % check no. of input arguments is correct

% sMap
if isstruct(sMap), 
  M = sMap.codebook;
  sTopol = sMap.topol; 
  mask = sMap.mask;
elseif isnumeric(sMap),
  M = sMap; 
  si = size(M);
  dim = si(end);
  if length(si)>2, msize = si(1:end-1);
  else msize = [si(1) 1];
  end
  munits = prod(msize);
  sTopol = som_set('som_topol','msize',msize,'lattice','rect','shape','sheet'); 
  mask = ones(dim,1);
  M = reshape(M,[munits,dim]);
end
mode = 'median';

% varargin
i=1; 
while i<=length(varargin), 
  argok = 1; 
  if ischar(varargin{i}), 
    switch varargin{i}, 
      % argument IDs
     case 'mask',       i=i+1; mask = varargin{i}; 
     case 'msize',      i=i+1; sTopol.msize = varargin{i}; 
     case 'lattice',    i=i+1; sTopol.lattice = varargin{i};
     case {'topol','som_topol','sTopol'}, i=i+1; sTopol = varargin{i};
     case 'mode',       i=i+1; mode = varargin{i};
      % unambiguous values
     case {'hexa','rect'}, sTopol.lattice = varargin{i};
     case {'min','mean','median','max'}, mode = varargin{i};
     otherwise argok=0; 
    end
  elseif isstruct(varargin{i}) & isfield(varargin{i},'type'), 
    switch varargin{i}(1).type, 
     case 'som_topol', sTopol = varargin{i};
     case 'som_map',   sTopol = varargin{i}.topol;
     otherwise argok=0; 
    end
  else
    argok = 0; 
  end
  if ~argok, 
    disp(['(som_umat) Ignoring invalid argument #' num2str(i+1)]); 
  end
  i = i+1; 
end

% check
[munits dim] = size(M);
if prod(sTopol.msize)~=munits, 
  error('Map grid size does not match the number of map units.')
end
if length(sTopol.msize)>2, 
  error('Can only handle 1- and 2-dimensional map grids.')
end
if prod(sTopol.msize)==1,
  warning('Only one codebook vector.'); U = []; return;
end
if ~strcmp(sTopol.shape,'sheet'), 
  disp(['The ' sTopol.shape ' shape of the map ignored. Using sheet instead.']);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% initialize variables

y = sTopol.msize(1);
x = sTopol.msize(2);
lattice = sTopol.lattice;
shape = sTopol.shape;
M = reshape(M,[y x dim]);

ux = 2 * x - 1; 
uy = 2 * y - 1;
U  = zeros(uy, ux);

calc = sprintf('%s(a)',mode);

if size(mask,2)>1, mask = mask'; end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% u-matrix computation

% distances between map units

if strcmp(lattice, 'rect'), % rectangular lattice
  
  for j=1:y, for i=1:x,
      if i<x, 
	dx = (M(j,i,:) - M(j,i+1,:)).^2; % horizontal
	U(2*j-1,2*i) = sqrt(mask'*dx(:));
      end 
      if j<y, 
	dy = (M(j,i,:) - M(j+1,i,:)).^2; % vertical
	U(2*j,2*i-1) = sqrt(mask'*dy(:));
      end
      if j<y & i<x,	
	dz1 = (M(j,i,:) - M(j+1,i+1,:)).^2; % diagonals
	dz2 = (M(j+1,i,:) - M(j,i+1,:)).^2;
	U(2*j,2*i) = (sqrt(mask'*dz1(:))+sqrt(mask'*dz2(:)))/(2 * sqrt(2));
      end
    end
  end

elseif strcmp(lattice, 'hexa') % hexagonal lattice

  for j=1:y, 
    for i=1:x,
      if i<x,
	dx = (M(j,i,:) - M(j,i+1,:)).^2; % horizontal
	U(2*j-1,2*i) = sqrt(mask'*dx(:));
      end
      
      if j<y, % diagonals
	dy = (M(j,i,:) - M(j+1,i,:)).^2;
	U(2*j,2*i-1) = sqrt(mask'*dy(:));	
	
	if rem(j,2)==0 & i<x,
	  dz= (M(j,i,:) - M(j+1,i+1,:)).^2; 
	  U(2*j,2*i) = sqrt(mask'*dz(:));
	elseif rem(j,2)==1 & i>1,
	  dz = (M(j,i,:) - M(j+1,i-1,:)).^2; 
	  U(2*j,2*i-2) = sqrt(mask'*dz(:));
	end
      end
    end
  end
  
end

% values on the units

if (uy == 1 | ux == 1),
  % in 1-D case, mean is equal to median 

  ma = max([ux uy]);
  for i = 1:2:ma,
    if i>1 & i<ma, 
      a = [U(i-1) U(i+1)]; 
      U(i) = eval(calc);
    elseif i==1, U(i) = U(i+1); 
    else U(i) = U(i-1); % i==ma
    end
  end    

elseif strcmp(lattice, 'rect')

  for j=1:2:uy, 
    for i=1:2:ux,
      if i>1 & j>1 & i<ux & j<uy,    % middle part of the map
	a = [U(j,i-1) U(j,i+1) U(j-1,i) U(j+1,i)];        
      elseif j==1 & i>1 & i<ux,        % upper edge
	a = [U(j,i-1) U(j,i+1) U(j+1,i)];
      elseif j==uy & i>1 & i<ux,       % lower edge
	a = [U(j,i-1) U(j,i+1) U(j-1,i)];
      elseif i==1 & j>1 & j<uy,        % left edge
	a = [U(j,i+1) U(j-1,i) U(j+1,i)];
      elseif i==ux & j>1 & j<uy,       % right edge
	a = [U(j,i-1) U(j-1,i) U(j+1,i)];
      elseif i==1 & j==1,              % top left corner
	a = [U(j,i+1) U(j+1,i)];
      elseif i==ux & j==1,             % top right corner
	a = [U(j,i-1) U(j+1,i)];
      elseif i==1 & j==uy,             % bottom left corner
	a = [U(j,i+1) U(j-1,i)];
      elseif i==ux & j==uy,            % bottom right corner
	a = [U(j,i-1) U(j-1,i)];
      else
	a = 0;
      end
      U(j,i) = eval(calc);
    end
  end

elseif strcmp(lattice, 'hexa')
  
  for j=1:2:uy, 
    for i=1:2:ux,
      if i>1 & j>1 & i<ux & j<uy,      % middle part of the map
	a = [U(j,i-1) U(j,i+1)];
	if rem(j-1,4)==0, a = [a, U(j-1,i-1) U(j-1,i) U(j+1,i-1) U(j+1,i)];
	else a = [a, U(j-1,i) U(j-1,i+1) U(j+1,i) U(j+1,i+1)]; end       
      elseif j==1 & i>1 & i<ux,        % upper edge
	a = [U(j,i-1) U(j,i+1) U(j+1,i-1) U(j+1,i)];
      elseif j==uy & i>1 & i<ux,       % lower edge
	a = [U(j,i-1) U(j,i+1)];
	if rem(j-1,4)==0, a = [a, U(j-1,i-1) U(j-1,i)];
	else a = [a, U(j-1,i) U(j-1,i+1)]; end
      elseif i==1 & j>1 & j<uy,        % left edge
	a = U(j,i+1);
	if rem(j-1,4)==0, a = [a, U(j-1,i) U(j+1,i)];
	else a = [a, U(j-1,i) U(j-1,i+1) U(j+1,i) U(j+1,i+1)]; end
      elseif i==ux & j>1 & j<uy,       % right edge
	a = U(j,i-1);
	if rem(j-1,4)==0, a=[a, U(j-1,i) U(j-1,i-1) U(j+1,i) U(j+1,i-1)];
	else a = [a, U(j-1,i) U(j+1,i)]; end
      elseif i==1 & j==1,              % top left corner
	a = [U(j,i+1) U(j+1,i)];
      elseif i==ux & j==1,             % top right corner
	a = [U(j,i-1) U(j+1,i-1) U(j+1,i)];
      elseif i==1 & j==uy,             % bottom left corner
	if rem(j-1,4)==0, a = [U(j,i+1) U(j-1,i)];
	else a = [U(j,i+1) U(j-1,i) U(j-1,i+1)]; end
      elseif i==ux & j==uy,            % bottom right corner
	if rem(j-1,4)==0, a = [U(j,i-1) U(j-1,i) U(j-1,i-1)];
	else a = [U(j,i-1) U(j-1,i)]; end
      else
	a=0;
      end
      U(j,i) = eval(calc);
    end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% normalization between [0,1]

% U = U - min(min(U)); 
% ma = max(max(U)); if ma > 0, U = U / ma; end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



function h=som_cplane(varargin) 

%SOM_CPLANE Visualize one 2D component plane, U-matrix or color plane.
%
% h=som_cplane(lattice, msize, color, [s], [pos]) 
% h=som_cplane(topol, color, [s], [pos]) 
%
%  som_cplane('hexa', [10 5], 'none');
%  som_cplane('rect', [10 5], 'r');
%  som_cplane(sM.topol, sM.codebook(:,1));
%  U = som_umat(sM); som_cplane('hexaU',sM.topol.msize,U(:));
%
%  Input and output arguments ([]'s are optional): 
%   lattice   (string) 'hexa', 'rect' (component planes)
%                      'hexaU', 'rectU' (corresponding U-matrices)
%             (matrix) defines the patch (see function VIS_PATCH).
%   msize     (vector) 1x2 vector defines grid size (M=prod(msize))
%             (matrix) Mx2 matrix gives explicit coordinates for each node
%   topol     (struct) map or topology struct
%   color              color for the nodes
%             (matrix) Mx1 matrix gives indexed colors for the units
%                      Mx3 matrix of RGB triples gives explicit
%                      color for each unit
%                      (Note: in case of U-matrix, the number of color
%                      values is 4*prod(msize)-2*sum(msize)+1, not prod(msize))
%             (string) ColorSpec gives the same color for each node
%                      'none' draws black edges only.              
%   [s]       (matrix) size Mx1, gives individual size scaling for each node 
%             (scalar) gives the same size for each node, default=1.
%                      Additional features: see 'type som_cplane' 
%                      This argument is ignored if the lattice is 'rectU' or 'hexaU'.
%   [pos]     (vector) a 1x2 vector that determines position of origin, 
%                      default is [1 1].
%
%   h         (scalar) the object handle for the PATCH object
%
% Axis are set to the 'ij' mode with equal spacing and turned off if
% 'pos' is not given. If 'lattice' is 'rect', 'hexa', 'rectU' or
% 'hexaU' the node (a,b) has coordinates (a,b) (+pos), except on the
% even numbered rows on the 'hexa' and 'hexaU' grids where the
% coordinates are (a,b+0.5) (+pos).
%
% For more help, try 'type som_cplane' or check out online documentation.
% See also SOM_PIEPLANE, SOM_PLOTPLANE, SOM_BARPLANE, VIS_PATCH,
%          SOM_VIS_COORDS

%%%%%%%%%%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% som_cplane
%
% PURPOSE
% 
% Visualizes a 2D component plane or u-matrix
%
% SYNTAX
%
%  h = som_cplane(topol, color)
%  h = som_cplane(lattice, msize, color)
%  h = som_cplane(lattice, msize, color)
%  h = som_cplane(..., size) 
%  h = som_cplane(..., size, pos) 
%
% DESCRIPTION
%
% Creates some basic visualizations of the SOM grid: the component plane and
% the unified distance matrix. The routine draws the SOM grid as a patch
% object according to the specifications given in the input arguments and
% returns its object handle.
% 
% Each unit of the map is presented by a polygon whose color, size, shape
% and location can be specified in various ways. The usual procedure 
% is to choose the lattice and map size used in the map training. Then
% the function creates the standard sheet shaped topological 
% representation of the map grid with hexagonal or rectangular units.
% When the values from a map codebook component (or from SOM_UMAT) 
% are given to the function it produces an indexed coloring for the 
% units (as in SURF command). Another possibility is to give a fixed 
% RGB color for each unit explicitly.
% 
% Special effects (variable unit size, location or shape) can be produced
% giving different types of input variables.
%
% KNOWN BUGS
%
% Using 1x3 or 3x1 grids causes problem, as the MATLAB will treat the color 
% information vector 1x3 or 3x1 as a single RGB triple. So, using indexed 
% colors is not possible for this particular map size.
%
% It is not possible to specify explicit coordinates for map
% consistig of just one unit as then the msize is interpreted as
% map size.
%
% REQUIRED INPUT ARGUMENTS
% 
% Note: M is the number of map units
%
% lattice  The basic shape of the map units 
%
%   (string) 'hexa' or 'rect' creates standard component plane; 
%            'hexaU' or 'rectU' creates standard u-matrix.
%   (matrix) Lx2 matrix defines the cornes of an arbitary polygon to be used
%            as the unit marker. (L is the number of patch vertex: L=6 for 
%            'hexa' and L=4 for 'rect') 
%
% msize    The size of the map grid     
%         
%   (vector) [n1 n2] vector defines the map size (height n1 units, width 
%            n2 units, total M=n1 x n2 units). The units will be placed to their 
%            topological locations to form a uniform hexagonal or rectangular grid.
%   (matrix) Mx2 matrix defines arbitrary coordinates for the M units
%            In this case the argument 'lattice' defines the unit form only. 
%
% topol    Topology of the map grid
%
%   (struct) map or topology struct from which the topology is taken
% 
% color    Unit colors
%           
%   (string) (ColorSpec) gives the same color for each unit, 'none'
%            draws black unit edges only.
%   (vector) Mx1 column vector gives indexed color for each unit using the 
%            current colormap (see help colormap).   
%   (matrix) Mx3 matrix of RGB triples as rows gives each unit a fixed color.
%
% OPTIONAL INPUT ARGUMENTS
%
% Note: M is the number of map units. 
% Note: if unspecified or given empty values ('' or []) default
% values are used for optional input arguments.
% 
% s        The size scaling factors for the units
% 
%   (scalar) scalar gives each unit the same size scaling: 
%            0   unit disappears (edges can be seen as a dot).
%            1   by default unit has its normal size (ie. no scaling)
%            >1  unit overlaps others      
%   (matrix) Mx1 double: each unit gets individual size scaling
%
% pos      Position of origin          
% 
%   (vector) This argument exists to be able drawing component planes
%            in arbitrary locations in a figure. Note the operation:
%            if this argument is given, the axis limits setting
%            part in the routine is skipped and the limits setting
%            will be left to be done by MATLAB's default
%            operation. 
%
% OUTPUT ARGUMENTS
%
% h (scalar) handle to the created patch object
% 
% OBJECT TAGS     
%
% One object handle is returned: field Tag is set to
%  'planeC'  for component plane     
%  'planeU'  for U-matrix
%
% FEATURES
%
% There are some extra features in following arguments
%
% size
%  - MxL matrix: radial scaling: the distance between 
%    the center of node m and its kth vertex is scaled by
%    s(m,k).
%  - Mx1x2 matrix: the uniform scaling is done separately for
%    x- and y-directions
%  - MxLx2 matrix: the scaling is done separately to x- and y-
%    directions for each vertex.
%
% color
%    Each vertex may be given individual color. 
%    The PATCH object interpolates the colors on the 
%    face if shading is turned to interp. 
%  - 1xMxL matrix: colormap index for each vertex
%  - LxMx3 matrix: RGB color for each vertex
%
% Note: In both cases (size and color) the ordering of the patch
% vertices in the "built-in" patches is the following
%
%          'rect'      'hexa'
%            1 3          1 
%            2 4         5 2
%                        6 3
%                         4
%
% The color interpolation result seem to depend on the order 
% in which the patch vertices are defined. Anyway, it gives 
% unfavourable results in our case especially with hexa grid: 
% this is a MATLAB feature.
%
% EXAMPLES
%
% m=som_make(rand(100,4),'msize',[6 5])         % make a map
% 
% % show the first variable plane using indexed color coding
%          
% som_cplane(m.topol.lattice,m.topol.msize,m.codebook(:,1));  
% or som_cplane(m.topol,m.codebook(:,1));  
% or som_cplane(m,m.codebook(:,1));  
%
% % show the first variable using different sized black units
%  
% som_cplane(m,'k',m.codebook(:,1));
% 
% % Show the u-matrix. First we have to calculate it. 
% % Note: som_umat returns a matrix therefore we write u(:) to get 
% % a vector which contains the values in the proper order.
% 
% u=som_umat(m); 
% som_cplane('hexaU', m.topol.msize, u(:)); 
%
% % Show three first variables coded as RGB colors
% % and turn the unit edges off
% 
% h=som_cplane(m, m.codebook(:,1:3),1)
% set(h,'edgecolor','none');
%
% % Try this! (see section FEATURES)
% 
% som_cplane('rect',[5 5],'none',rand(25,4));
% som_cplane('rect',[5 5],rand(1,25,4));
%
% SEE ALSO
%
% som_barplane   Visualize the map prototype vectors as bar diagrams
% som_plotplane  Visualize the map prototype vectors as line graphs
% som_pieplane   Visualize the map prototype vectors as pie charts
% som_umat       Compute unified distance matrix of self-organizing map
% vis_patch      Define the basic patches used in som_cplane
% som_vis_coords The default 'hexa' and 'rect' coordinates in visualizations

% Copyright (c) 1999-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 061099 juuso 151199 juuso 070600

%%% Check & Init  arguments %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[nargin, lattice, msize, color, s, pos]=vis_planeGetArgs(varargin{:});
error(nargchk(3, 5, nargin));  % check no. of input args is correct

%% Translation?

if nargin < 5 | isempty(pos)
  pos=NaN;              % "no translation" flag
elseif ~vis_valuetype(pos,{'1x2'}),
  error('Position of origin has to be given as an 1x2 vector.');
end

%% Patchform %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

switch class(lattice)
case 'char'             % built-in patchforms
  pos=pos-1;
  switch lattice
  case {'hexa', 'hexaU'}
    patchform=vis_patch('hexa');
  case {'rect', 'rectU'}
    patchform=vis_patch('rect');
  otherwise
    error([ 'Lattice ' lattice ' not implemented!']);
  end
case { 'double', 'sparse'}
  if vis_valuetype(lattice,{'nx2'}),
    patchform=lattice; % users patchform
    lattice='rect';    
  else
    error('Patchform matrix has wrong size');
  end
otherwise
  error('String or matrix expected for lattice.');
end

l=size(patchform,1);     % number of vertices    
planeType=lattice(end);  % 'U' if umatrix otherwise something else

if ~vis_valuetype(msize,{ '1x2', 'nx2'}),
  error('msize has to be given as 1x2 or nx2 vectors.');
end

%% msize or coordinates %%%%%%%%%%%%%%%%%%%%%%%

if size(msize,1)>1 
  % msize is coordinate matrix Nx2?
  
  if planeType == 'U',  % don't accept u-matrix
    error('U-matrix visualization doesn''t work with free coordinates.');
  end
  
  % set number of map unit and unit coordinates 
  munits=size(msize,1);
  unit_coords=msize; msize=[munits 1];
  
  if isnan(pos),         % no translation is done here 
    pos=[0 0];           % using [0 0] in order to prevent 
  end	                 % axis tightening in
                         % vis_PlaneAxisProperties (arbitary coords!) 
else
  % msize is built-in lattice
  
  unit_coords=som_vis_coords(lattice,msize);
  
  % Calculate matrices x and y which 'moves' nodes 
  % to the correct positions:
  % For U-matrix, the size has to be recalculated
  if planeType == 'U',
    xdim=2*msize(1)-1;ydim=2*msize(2)-1;
  else
    xdim=msize(1);ydim=msize(2);
  end
  munits=xdim*ydim;
  
  % Feature warning
  if munits == 3  
    warning('Problems with 1x3 and 3x1 maps. See documentation.');
  end
end

%% Color matrix %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~isnumeric(color) & ~ischar(color),
  error('Color matrix is invalid.');
else
  d=size(color);           
  switch length(d)       
  case 2   %% Flat colors
    if ischar(color) % Check for string 'none'
      if strcmp(color,'none'), 
	color=NaN;
      end
    else               
      if ~(d(1)== 1 & d(2) == 3) & ...
	    ~(d(1) == munits & (d(2)==1 | d(2)==3))
	error('Color data matrix has wrong size.');
      elseif d(1)~=1 & d(2)==3 
	if any(color>1 | color<0)
	  error('Color data matrix has invalid RGB values.');
	end
	color=reshape(color,[1 munits 3]);  % RGB colors
      elseif d(2)==1
	color=color';                       % indexed
      end
    end
  case 3   %% Interpolated colors
    if d(1) == 1 & d(2) == munits & d(3) == l,  
      color=reshape(color, l, munits);
    elseif ~(d(1) == l & d(2) == munits & d(3) == 3) 
      error('Color data matrix has wrong size.');
    end
  otherwise
    error('Color data matrix has too many dimensions.');
  end
end

%% Size matrix? %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if nargin < 4 | isempty(s),  
   s=1;      % default value for s (no scaling)
elseif ~isnumeric(s)
  error('Size matrix is not numeric.');
end

%%Determine the type of size matrix
d=size(s);                  
switch length(d)
case 2  
  if (d(1)==1 & d(2)==1),
    % Each node gets the same, uniform scaling.
    s=s'; sx=s; sy=s;	
  elseif (d(1)==munits & d(2)==l),
    % Each vertex is scaled radially respetc to the 
    % node center.
    s=s'; sx=s; sy=s;          
  elseif d(1)==munits & d(2)==1  
    % Each node gets an individual uniform scaling.
    sx=repmat(s',l,1); sy=sx;
  else
    error('Size matrix has wrong size.');
  end
case 3  
  if d(1)==munits & d(2)==1 & d(3)==2,     
    % Each node is individually and uniformly 
    % scaled separately to x- and y-directions.
    sx=repmat(shiftdim(s(:,:,1))',l,1);   
    sy=repmat(shiftdim(s(:,:,2))',l,1);   
  elseif d(1)==munits & d(2)==l & d(3)==2,
    % Each vertex is scaled separately to x- and y-directions
    % with respect to the node center.
    sx=shiftdim(s(:,:,1))';                
    sy=shiftdim(s(:,:,2))';              
  else
    error('Size matrix has wrong size.');
  end
otherwise 
  error('Size matrix has too many dimensions.');
end

% Size zero would cause division by zero. eps is as good (node disappears)
% I tried first NaN, it works well otherwise, but the node is 
% then not on the axis and some commands may the work oddly. 
% The edge may be visible, though.

sx(sx==0)=eps;                    
sy(sy==0)=eps;                

% Rescale sizes for u-matrix
if planeType=='U', 
   sx=sx/2;sy=sy/2; 
end

%%%% Action %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Making grid. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Translation for each patch

x=repmat(unit_coords(:,1)',l,1);
y=repmat(unit_coords(:,2)',l,1);

% patch vertex coordiantes 

nx=repmat(patchform(:,1),1,munits); 
ny=repmat(patchform(:,2),1,munits); 

% NB: The hexagons are not uniform in order to get even  
% y-coordinates for the nodes. This is handled by setting _axis scaling_ 
% so that the hexa-nodes look like uniform hexagonals. See 
% vis_PlaneAxisProperties

%% Make and scale the final input for PATCH:

% 1: combine translation and scaling of each patch 
x=(x./sx+nx).*sx; y=(y./sy+ny).*sy;  

%% 2: translation of origin (pos)
if ~isnan(pos)
   x=x+pos(1);y=y+pos(2);    % move upper left corner 
end                         % to pos

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set axes properties  
%% Command view([0 90]) shows the map in 2D properly oriented

ax=newplot;                               % set new plot
vis_PlaneAxisProperties(ax,lattice,msize,pos);

%% Draw the map! 

if ~isnan(color)
   h_=patch(x,y,color);         
else
   h_=patch(x,y,'k');                     % empty grid 
   set(h_,'FaceColor','none');
end

%% Set object tag

if planeType=='U'
   set(h_,'Tag','planeU');                      
else
   set(h_,'Tag','planeC');
end	

%%% Build output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if nargout>0, h=h_; end                   % Set h only, 
                                          % if there really is output


                                          
function [nargin_,varargout]=vis_planeGetArgs(varargin)

% VIS_PLANEGETARGS Subfunction for som_*plane: extracts topolopy 
%                  information from the first arguments.
%
% [nargin,varargout]=vis_planeGetArgs(varargin)
%
%  Input and output arguments: 
%   varargin   (varies) arguments given to som_*plane function
%   nargin_    (scalar) number of arguments that nargchk of som_*plane "should see"
%                       +number_of_varargins if varargin{1} is not map/topol struct
%                       +number_of_varargins+1 if varargin{2} is a map/topol struct
%   varargout  (varies) the arguments that som_*plane "should see"
%
% Basically, this function allows topology information to be given 
% in various ways: either as a map/topology struct, or as a argument pair:
% lattice, msize. The topology is always converted into the (lattice, msize)
% argument pair.
%  - if first input argument (varargin{1}) is a map or topol struct 
%    the function extracts lattice and msize fields to two first 
%    output variables after 'nargin_'. 
%  - otherwise it copies the input arguments to the output arguments 
%    after 'nargin_'. 
% If there are too many inputs (as compared to number of outputs), the 
% last ones are ignored. If too few, they are replaced by empty values 
% in outputs.
%
% Example of usage: 
%   function definition: h = som_cplane(varargin)
%   first code line:     [nargin,lattice,msize,color,size,pos]=vis_planeGetArgs(varargin);
%
% See also SOM_CPLANE, SOM_BARPLANE, SOM_PLOTPLANE, SOM_PIEPLANE.

% Copyright (c) 2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 240300

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nout=nargout-1;

% Set first all varargins to contain empty (==default values in som_*plane)

for i=1:nout, varargout{i}=[]; end

nargin_ = nargin;
% Struct: might be map or topol

if isstruct(varargin{1}),

  % Get topol from topol field
  if isfield(varargin{1},'topol'), topol=varargin{1}.topol;
  else topol=varargin{1}; % assume that this is topol struct 
  end

  if ~isstruct(topol),
    % topol not a struct !?
    warning('Field ''topol'' is not a struct.');
    varargout{1}=varargin{1};
    varargoutC=2;
    nargin_ = nargin;
  elseif ~isfield(topol,'msize') | ~isfield(topol,'lattice'),
    % Field missing?!
    warning('Invalid topology struct.');
    varargout{1}=topol;
    varargoutC=2;
    nargin_ = nargin;
  else
    varargout{1}=topol.lattice;
    varargout{2}=topol.msize;
    % increment input arg. counter
    varargoutC=3;
    nargin_ = nargin+1;    
  end

elseif iscell(varargin{1}), 

  c = varargin{1}; 
  lattice = 'hexa'; shape = 'sheet'; msize = [1 1]; 
  for i=1:length(c), 
    if ischar(c{i}), 
      switch c{i}, 
      case {'hexa','hexaU','rect','rectU'}, lattice = c{i}; 
      case {'sheet','cyl','toroid'}, shape = c{i}; 
      end
    else
      msize = c{i}; 
    end 
  end
  varargout{1} = lattice;
  varargout{2} = msize;
  varargoutC=3;
  nargin_ = nargin+1;    

else

  % should be a lattice (string) 
  varargout{1}=varargin{1};
  varargoutC=2;
  nargin_=nargin;

end

for i=2:nargin, varargout{varargoutC+i-2}=varargin{i}; end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



function p=vis_patch(lattice)

% VIS_PATCH Defines the basic patches (hexa and rect) used in SOM_CPLANE
%
%  p = vis_patch(lattice)
%
%  Input and output arguments: 
%   lattice   (string) 'rect', 'hexa' or 'hexagon'
%
%   p         (matrix) size Lx2, defines the vertices of the patch
%
% This function produces vertex coordinates for a patch presenting
% a map unit in hexagonal or rectangular lattice with its centre in (0,0). 
%
% For more help, try 'type vis_patch' or check out online documentation.
% See also SOM_CPLANE, PATCH.

%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% vis_patch
%
% SYNTAX
%  
%  p = vis_patch(lattice)
% 
% DESCRIPTION
% 
% Forms a map unit patch for SOM_CPLANE function. Mainly a subroutine
% of SOM_CPLANE, although can be used for on its own as well.
%
% REQUIRED INPUT ARGUMENTS
%
% lattice (string)
% 
%    'hexa'  produces vertex coordiantes for a hexagoanl patch which 
%            has its center on (0,0), unit width, and a height of
%            1.3334 units. This is not a regular hexagon but such that
%            the node which has topological coordinates (a,b) has its
%            center in the visualization at coordinates (a,b) for odd
%            a and at (a,b+.5) for even a. The non-regular look of the
%            patch is taken care simply by changing the axis ratio.
%
%    'rect'  produces vertex coordinates for a uniform rectangular patch.
%            having its center on (0,0) and unit sidelength. Used as a
%            subfunction in SOM_CPLANE.
%
%    'hexagon' produces vertex coordinates for a regular hexagonal patch.
%            It may be used in som_cplane if, for some reason, truly
%            regular hexagons are needed instead of the default unit
%            markers which are not uniform, but have integer
%            y-coordinates in the lattice.
%
% OUTPUT ARGUMENTS
%
% p (matrix) The 2-dimensional vertex coordinates: 
%
%   case 'rect'        case 'hexa'                case 'hexagon'
%    p=[[-.5 -.5];...     p=[[0     0.6667];...    p=[[0     0.5774];...
%       [-.5  .5];...        [0.5   0.3333];...       [0.5   0.2887];...
%       [ .5  .5];...        [0.5  -0.3333];...       [0.5  -0.2887];...
%       [ .5 -.5]];          [0    -0.6667];...       [0    -0.5774];...
%                            [-0.5 -0.3333];...       [-0.5 -0.2887];...
%                            [-0.5  0.3333]];         [-0.5  0.2887]]; 
%
% EXAMPLES
%
%  som_cplane(vis_patch('rect'),[6 5],'none');
%  % this produces the same result as som_cplane('rect',[6 5], 'none') 
%  
%  som_cplane(vis_patch('hexa'), vis_unit_coord('hexa',[6 5]), 'none');
%  % produces in principle the same result as 
%  % som_cplane(vis_patch('hexa'),[6 5],'none'), 
%  % _but_ in this case the axis are not rescaled and the non-regular 
%  % shape of hexagons can be seen.
%
%  som_cplane(vis_patch('hexagon'), som_unit_coords([6 5],'hexa'), 'none');
%  % produces a truly regular hexa lattice 
%
% SEE ALSO 
%
%  vis_unit_coord   The default 'hexa' and 'rect' coordinates in visualizations
%  som_unit_coords  Locations of units on the SOM grid.

% Copyright (c) 1999-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 041099

if ~ischar(lattice)
  error('Input argument should be a string')
else
  switch lattice
  case 'rect'
    p=[[-.5 -.5]; ...
      [-.5 .5];...
      [.5 .5];...
      [.5 -.5]];
  case 'hexagon'
    p=[[0 0.5774];...
      [0.5 0.2887];...
      [0.5 -0.2887];...
      [0 -0.5774];...
      [-0.5 -0.2887];...
      [-0.5 0.2887]];
  case 'hexa'
    p=[[0 0.6667];...
      [0.5 0.3333];...
      [0.5 -0.3333];...
      [0 -0.6667];...
      [-0.5 -0.3333];...
      [-0.5 0.3333]];
  otherwise
    error('Unknown lattice');
  end
end



function unit_coord=som_vis_coords(lattice, msize)

%SOM_VIS_COORDS Unit coordinates used in visualizations.
% 
% Co = som_vis_coords(lattice, msize)
%
%  Co = som_vis_coords('hexa',[10 7])
%  Co = som_vis_coords('rectU',[10 7])
%
%  Input and output arguments: 
%   lattice   (string) 'hexa', 'rect', 'hexaU' or 'rectU'
%   msize     (vector) grid size in a 1x2 vector    
%
%   Co        (matrix) Mx2 matrix of unit coordinates, where 
%               M=prod(msize) for 'hexa' and 'rect', and 
%               M=(2*msize(1)-1)*(2*msize(2)-1) for 'hexaU' and 'rectU'
%
% This function calculates the coordinates of map units on a 'sheet'
% shaped map with either 'hexa' or 'rect' lattice as used in the
% visualizations. Note that this slightly different from the
% coordinates provided by SOM_UNIT_COORDS function. 
%
% 'rectU' and 'hexaU' gives the coordinates of both units and the
% connections for u-matrix visualizations.
%
% For more help, try 'type som_vis_coords' or check out online documentation.
% See also SOM_UNIT_COORDS, SOM_UMAT, SOM_CPLANE, SOM_GRID.

%%%%%%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PURPOSE 
% 
% Returns coordinates of the map units for map visualization
%
% SYNTAX
%
%  Co = som_vis_coords(lattice, msize)
%
% DESCRIPTION
%
% This function calculates the coordinates of map units in 'hexa' and
% 'rect' lattices in 'sheet' shaped map for visualization purposes. It
% differs from SOM_UNIT_COORDS in the sense that hexagonal lattice is
% calculated in a "wrong" way in order to get integer coordinates for
% the units. Another difference is that it may be used to calculate
% the coordinates of units _and_ the center points of the lines
% connecting them (edges) by using 'hexaU' or 'rectU' for lattice. 
% This property may be used for drawing u-matrices.
%
% The unit number 1 is set to (ij) coordinates (1,1)+shift
%                 2                            (2,1)+shift
%
%  ... columnwise
% 
%             n-1th                        (n1-1,n2)+shift
%             nth                            (n1,n2)+shift
%
% where grid size = [n1 n2] and shift is zero, except for 
% the even lines of 'hexa' lattice, for which it is +0.5.
%
% For 'rectU' and 'hexaU' the unit coordinates are the same and the
% coordinates for connections are set according to these. In this case
% the ordering of the coordinates is the following:
%   let
%     U  = som_umat(sMap); U=U(:); % make U a column vector
%     Uc = som_vis_coords(sMap.topol.lattice, sMap.topol.msize); 
%   now the kth row of matrix Uc, i.e. Uc(k,:), contains the coordinates 
%   for value U(k). 
%
% REQUIRED INPUT ARGUMENTS 
%
%  lattice  (string) The local topology of the units: 
%                    'hexa', 'rect', 'hexaU' or 'rectU'
%  msize    (vector) size 1x2, defining the map grid size. 
%                    Notice that only 2-dimensional grids
%                    are allowed.
%
% OUTPUT ARGUMENTS
% 
%  Co       (matrix) size Mx2, giving the coordinates for each unit.
%                    M=prod(msize) for 'hexa' and 'rect', and 
%                    M=(2*msize(1)-1)*(2*msize(2)-1) for 'hexaU' and 'rectU'
%
% FEATURES
% 
% Only 'sheet' shaped maps are considered. If coordinates for 'toroid'
% or 'cyl' topologies are required, you must use SOM_UNIT_COORDS
% instead.
%
% EXAMPLES
%
% Though this is mainly a subroutine for visualizations it may be
% used, e.g., in the following manner:
%
% % This makes a hexagonal lattice, where the units are rectangular
% % instead of hexagons.
%    som_cplane('rect',som_vis_coords('hexa',[10 7]),'none');
%
% % Let's make a map and calculate a u-matrix: 
%    sM=som_make(data,'msize',[10 7],'lattice','hexa');
%    u=som_umat(sM); u=u(:);
% % Now, these produce equivalent results:
%    som_cplane('hexaU',[10 7],u);
%    som_cplane(vis_patch('hexa')/2,som_vis_coords('hexaU',[10 7]),u);
%
% SEE ALSO
%
% som_grid         Visualization of a SOM grid
% som_cplane       Visualize a 2D component plane, u-matrix or color plane
% som_barplane     Visualize the map prototype vectors as bar diagrams
% som_plotplane    Visualize the map prototype vectors as line graphs
% som_pieplane     Visualize the map prototype vectors as pie charts
% som_unit_coords  Locations of units on the SOM grid

% Copyright (c) 1999-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 201099 juuso 261199

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~vis_valuetype(msize,{'1x2'}),
  error('msize must be a 1x2 vector.')
end

if vis_valuetype(lattice,{'string'})
  switch lattice
  case {'hexa', 'rect'}
    munits=prod(msize);
    unit_coord(:,1)=reshape(repmat([1:msize(2)],msize(1),1),1,munits)';
    unit_coord(:,2)=repmat([1:msize(1)]',msize(2),1);
    if strcmp(lattice,'hexa')
      % Move even rows by .5
      d=rem(unit_coord(:,2),2) == 0;   
      unit_coord(d,1)=unit_coord(d,1)+.5;
    end
  case {'hexaU','rectU'}
    msize=2*msize-1; munits=prod(msize);
    unit_coord(:,1)=reshape(repmat([1:msize(2)],msize(1),1),1,munits)';
    unit_coord(:,2)=repmat([1:msize(1)]',msize(2),1);
    if strcmp(lattice,'hexaU')
      d=rem(unit_coord(:,2),2) == 0;   
      unit_coord(d,1)=unit_coord(d,1)+.5;
      d=rem(unit_coord(:,2)+1,4) == 0; 
      unit_coord(d,1)=unit_coord(d,1)+1;
    end
    unit_coord=unit_coord/2+.5;
  otherwise
    error([ 'Unknown lattice ''' lattice '''.']);
  end
else
  error('Lattice must be a string.');
end



function vis_PlaneAxisProperties(ax,lattice,msize,pos)

% VIS_PLANEAXISPROPERTIES Set axis properties for SOM_CPLANE, 
%                         SOM_PIEPLANE, SOM_BARPLANE and SOM_PLOTPLANE.
%
% vis_PlaneAxisProperties(ax,lattice,msize,pos)
%
%  Input arguments: 
%   ax        (scalar) axis handle     
%   lattice   (string) 'hexa', 'rect', 'hexaU' or 'rectU'
%             (matrix) defines the patch, see e.g. help vis_patch
%   msize     (vector) a 1x2 vector defining the grid size
%   pos       (vector) a 1x2 vector that determines position of
%                      origin or NaN which means default operation: 
%                      origin to [1 1] and tighten axis limits 
%                      according to the grid size.
% 
% This is a subfunction for SOM_CPLANE, SOM_PIEPLANE, SOM_BARPLANE and
% SOM_PLOTPLANE. This subfunction sets the proper values for axis.

% Copyright (c) 1999-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 060799

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xdim=msize(1);ydim=msize(2);
set(ax,'Visible','off');
set(get(ax,'Title'),'Visible','on');
set(ax,'XaxisLocation','Top');            % axis orientation
set(ax,'xdir','normal');                  % = axis ij = matrix mode
set(ax,'ydir','reverse'); 

switch lattice
case {'rect', 'rectU'}
  lelim=-.51; rilim=.51; uplim=-.51; lolim=.51;  % axis limits
  set(ax,'DataAspectRatio', [1 1 1]);            % =axis equal
case {'hexa','hexaU'}
  lelim=-.51; rilim=1.01; uplim=-.67; lolim=.67; % axis limits
  set(ax,'DataAspectRatio',[0.9015 1 1]);        % this corrects hexagons
end

% Nan: default origin [1 1] & tighten the axis
if isnan(pos)
  set(ax,'XLim',[1+lelim ydim+rilim],'YLim',[1+uplim xdim+lolim], ...
      'XLimMode','manual','YLimMode','manual'); % tighten the axis
end



function h=som_recolorbar(p, ticks, scale, labels)

%SOM_RECOLORBAR Refresh and  rescale colorbars in the current SOM_SHOW fig.
%
% h = som_recolorbar([p], [ticks], [scaling], [labels])
%
%   colormap(jet); som_recolorbar   
%
% Input and output arguments ([]'s are optional) 
%  [p]      (vector) subplot number vector 
%           (string) 'all' (the default), 'comp' to process only
%                    component planes        
%  [ticks]  (string) 'auto' or 'border', default: 'auto'
%           (cell array) p x 1 cell array of p row vectors
%           (vector) the same ticks are applied to all given subplots
%           (scalar) value is at least 2: the number of ticks to show, 
%                    evenly spaced between and including minimum and maximum 
%  [scale]  (string) 'denormalized' or 'normalized' (the default)
%  [labels] (cell array) p x 1 cell array of cells containing strings
%
%  h        (vector) handles to the colorbar objects.
%
% This function refreshes the colorbars in the figure created by SOM_SHOW.
% Refreshing  is necessary if you have changed the colormap.
% Each colorbar has letter 'd' or 'n' and possibly 'u' as label. Letter 'd' means
% that the scale is denormalized, letter 'n' that the scale is
% normalized, and 'u' is for user specified labels.
%
% For more help, try 'type som_recolorbar' or check out online documentation.
% See also SOM_SHOW
 
%%%%%%%%%%%%% DETAILED DESCRIPTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% som_recolorbar
%
% PURPOSE
% 
% Refreshes the the colorbars in the figure.
%
% SYNTAX
%
%  h = som_recolorbar
%  h = som_recolorbar(p)
%  h = som_recolorbar(p, ticks)
%  h = som_recolorbar(p, ticks, scaling)
%  h = som_recolorbar(p, ticks, scaling, labels)
%
% DESCRIPTION
%
% This function refreshes the colorbars in the figure created by SOM_SHOW.
% Refreshing is necessary if you have changed the colormap.  Each colorbar
% has letter 'd' or 'n' and possibly 'u' as label. Letter 'd' means that the
% scale is denormalized, letter 'n' that the scale is normalized, and 'u' is
% for user specified labels.
%
% Different argument combinations:
%
% 1. Argument 'ticks' has string values:
%  - 'auto' for input argument ticks sets the automatic tick
%     marking on (factory default). 
%  - 'border' sets the tick marks to the color borders. This is 
%     convenient if there are only few colors in use. 
%
%  Argument scale controls the scaling of the tick mark label values. 
%  'normalized' means that the tick mark labels are directly the values 
%  of the ticks, that is, they refer to the map codebook values. 
%  Value 'denormalized' scales the tick mark label values back to the original
%  data scaling. This is made using som_denormalize_data.
%
% 2. Argument 'ticks' is a cell array of vectors:
%  The values are set to be the tick marks to the colorbar specified by p.
%  - if arg. scale is 'normalized' the ticks are set directly to the colorbar.
%  - if arg. scale is 'denormalized' the tick values are first normalized 
%    in the same way as the data.
%
% 3. Argument 'ticks' is a vector
%  As above, but the same values are used for all (given) subplots.
%  
% 4. Argument 'ticks' is a scalar
%  The ticks are set to equally spaced values between (and including)
%  minimum and maximum.
%     
% Argument 'labels' specify user defined labels to the tick marks
%
% NOTE: ticks are rounded to contain three significant digits.
%
% OPTIONAL INPUT ARGUMENTS
% 
%  p        (vector) subplot number vector 
%           (string) 'all' (the default), 'comp' to effect only 
%                    component planes
%
%  ticks    (string) 'auto' or 'border', default: 'auto'
%           (cell array) p x 1 cell array of p row vectors
%           (vector) as the cell array, but the same vector is 
%                    applied to all given subplots
%           (scalar) the number of ticks to show: these are 
%                    evenly space between minimum and maximum
%
%  scale    (string) 'denormalized' or 'normalized' (the default)
%
%  labels   (cell array) p x 1 cell array of cells containing strings
%
% OUTPUT ARGUMENTS
%
%  h        (vector) handles to the colorbar objects.
%
% EXAMPLE
%
%  colormap(jet(5)); som_recolorbar('all','border','denormalized')
%      % Uses five colors and sets the ticks on the color borders.
%      % Tick label values are denormalized back to the original data scaling
%
%  colormap(copper(64));som_recolorbar
%      % changes to colormap copper and resets default ticking and labeling
%
%  som_recolorbar('all',3)
%      % To put 3 ticks to each colorbar so that minimum, mean and
%      % maximum values on the colorbar are shown.
% 
%  som_recolorbar([1 3],{[0.1 0.2 0.3];[0.2 0.4]},'denormalized')
%      % Ticks colorbar 1 by first normalizing values 0.1, 0.2, 0.3 and
%      % then setting the ticks to the colorbar. Labels are of course 
%      % 0.1, 0.2 and 0.3. Ticks colorbar 3 in the same way using values
%      % 0.2 and 0.4.
%
%  som_recolorbar([2 4],{[0.1 0.2];[-1.2 3]},'normalized',{{'1' '2'};{'a' 'b'}})
%      % Ticks colorbar 2 and 4 directly to the specified values. Sets labels
%      % '1' '2' and 'a' 'b' to the ticks.
%
%  som_recolorbar([2 4],{[0.1 0.2];[-1.2 3]},'normalized',{{'1' '2'};{'a' 'b'}})
%      % as previous one, but normalizes tick values first
%
% SEE ALSO
% 
%  som_show        Basic SOM visualization.
%  som_normalize   Normalization operations.
%  som_denormalize Denormalization operations.

% Copyright (c) 1997-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 1.0beta Johan 061197 
% Version 2.0beta juuso 151199 130300 160600 181101

%% Init & check %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(nargchk(0, 4, nargin))    % check no. of input args

% Check the subplot vector p and  get the handles, exit if error
% Default subplot vector is 'all'

if nargin < 1 | isempty(p)                       % default p
  p= 'all';
end

% check SOM_SHOW and get the figure data. Exit, if error

[handles, msg, lattice, msize, dim, normalization, comps]= ...
    vis_som_show_data(p, gcf);
error(msg);                                       

if nargin < 2 | isempty(ticks)                   % default tick mode is 'auto'
  ticks = 'auto';
elseif isa(ticks,'cell')                         % check for cell
  tickValues = ticks; 
  ticks= 'explicit';
elseif isa(ticks,'double') & length(ticks)>1,
  tickValues = {ticks}; 
  ticks = 'explicit'; 
elseif isa(ticks,'double') & length(ticks)==1,
  tickValues = max(2,round(ticks)); 
  ticks = 'evenspace'; 
end
if ~ischar(ticks)                                % invalid argument
  error('The second argument should be a string or a cell array of vectors.');
end

switch ticks                                     % check ticks
 case {'auto','border'}, % nill
 case 'evenspace', 
  tickValues_tmp = cell(length(handles),1); 
  for i=1:length(handles), tickValues_tmp{i} = tickValues; end
  tickValues = tickValues_tmp; 
 case 'explicit', 
  if length(tickValues)==1 & length(handles)>1, 
    tickValues_tmp = cell(length(handles),1); 
    for i=1:length(handles), tickValues_tmp{i} = tickValues{1}; end
    tickValues = tickValues_tmp; 
  end
  if length(tickValues) ~= length(handles), 
    error('Cell containing the ticks has wrong size.')
  end
otherwise
  error('''auto'' or ''border'' expected for the second argument.');
end

if nargin < 3 | isempty(scale)                   % default mode is normalized
  scale= 'normalized';
end
if ~ischar(scale)                                % check scale type
  error('The third argument should be a string.'); 
end
switch scale                                     % check the string
 case { 'normalized', 'denormalized'} % ok
 case 'n', scale = 'normalized'; 
 case 'd', scale = 'denormalized'; 
 otherwise   
  error('''normalized'' or ''denormalized'' expected for the third argument.')
end

if nargin < 4 | isempty(labels)                  % default is autolabeling
  labels = 'auto';
elseif ~isa(labels,'cell')                       % check type
  error('The fourth argument should be a cell array of cells containing strings.')
else
  labelValues=labels;                            % set labels
  labels = 'explicit';
  if length(labelValues) == length(handles)      % check size
    ;
  else
    error('Cell containing the labels has wrong size')
  end
end

%% Action %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n = size(colormap,1)+1;                      % number of colors+1
h_ = zeros(length(handles),1);  

for i=1:length(handles),                   % MAIN LOOP BEGINS
  axes(handles(i));                        % set axes, refres colorbar and  
  if comps(i)>=0,   
    h_(i)=colorbar;                          % get colorbar handles

    colorbardir=get(h_(i),'YaxisLocation');
    switch colorbardir                     % get colorbar direction &
     case 'left'                            % set some strings
      Tick='Xtick'; Lim='Xlim'; LabelMode='XTickLabelMode'; Label='XtickLabel';
     case 'right'
      Tick='Ytick'; Lim='Ylim'; LabelMode='YTickLabelMode'; Label='YtickLabel';
     otherwise
      error('Internal error: unknown value for YaxisLocation'); % fatal
    end                                                         
    
    switch ticks                         
     case 'auto'
      set(h_(i),LabelMode,'auto');        % factory default ticking
      tickValues{i}=get(h_(i),Tick);       % get tick values
     case 'border' 
      limit=caxis;                        
      t=linspace(limit(1),limit(2),n);    % set n ticks between min and max 
      t([1 length(t)])=get(h_(i),Lim); % <- caxis is not necerraily the same 
      tickValues{i}=t;                    % as the colorbar min & max values
     case 'evenspace'
      limit = caxis; 
      t = linspace(limit(1),limit(2),tickValues{i}); 
      t([1 length(t)])=get(h_(i),Lim);
      tickValues{i}=t; 
     case 'explicit'
      if comps(i)>0, 
	if strcmp(scale,'normalized')     % normalize tick values
	  tickValues{i} = som_normalize(tickValues{i},normalization{comps(i)});
	end
      end
      
     otherwise 
      error('Internal error: unknown tick type')   % this shouldn't happen
    end

    %tickValues{i} = epsto0(tickValues{i});

    switch labels
     case 'auto'
      switch scale                         
       case 'normalized'
	labelValues{i} = round2(tickValues{i});     % use the raw ones 
       case 'denormalized'                 % denormalize tick values
	if comps(i)>0, 
	  labelValues{i} = som_denormalize(tickValues{i},normalization{comps(i)});
	  labelValues{i} = round2(labelValues{i});     % round the scale
	else
	  labelValues{i} = round2(tickValues{i});
	end
       otherwise
	error('Internal error: unknown scale type'); % this shouldn't happen
      end
     case 'explicit'
      ;                                            % they are there already
     otherwise
      error('Internal error: unknown label type'); % this shouldn't happen
    end

    set(h_(i),Tick,tickValues{i});                 % set ticks and labels
    set(h_(i),Label,labelValues{i});            
    
    if comps(i)>0, 
      % Label the colorbar with letter 'n' if normalized, with letter 'd' 
      % if denormalized and 'u' if the labels are user specified  
      mem_axes=gca; axes(h_(i));
      ch='  ';
      if strcmp(scale,'normalized'),   ch(1)='n'; end
      if strcmp(scale,'denormalized'), ch(1)='d'; end
      if strcmp(labels,'explicit'),    ch(2)='u'; end
      xlabel(ch); 
      axes(mem_axes);
    end
  end
end                                              % MAIN LOOP ENDS 


%% Build output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if nargout>0
  h=h_;
end

return; 

%% Subfunction: ROUND2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROUND2 rounds the labels to tol significant digits

function r=round2(d)

tol=3;

zero=(d==0);
d(zero)=1;
k=floor(log10(abs(d)))-(tol-1);
r=round(d./10.^k).*10.^k;
r(zero)=0;
r=epsto0(r);

%% Subfunction: ISVECTOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function t=isvector(v)
% ISVECTOR checks if a matrix is a vector or not

t=(ndims(v) == 2 & min(size(v)) == 1) & isnumeric(v);

%% Subfunction: EPSTO0

function t=epsto0(t)
% EPSTO0 checks whether first tick value is *very* close to zero, 
% if so sets it to zero.

if (t(end)-t(1))/t(end) > 1-0.005 & abs(t(1))<1, t(1) = 0; end




function [handles,msg,lattice,msize,dim,normalization,comps]=vis_som_show_data(p,f)

% VIS_SOM_SHOW_DATA Checks and returns UserData and subplot handles stored by SOM_SHOW
%
% [handles,msg,lattice,msize,dim,normalization,comps] = vis_som_show_data(p, f)
%
%  Input and output arguments ([]'s are optional): 
%   [p]           (vector) subplot numbers 
%                 (string) 'all' to process all subplots, this is default
%                          'comp' to process only subplots which have
%                          component planes
%   [f]           (double) figure handle, default is current figure
%
%   handles       (vector) handles of requested subplots
%   msg           (string) error message or empty string (no error)
%   lattice       (string) map lattice: 'hexa' or 'rect'
%   msize         (vector) map grid size in figure
%   dim           (scalar) map data dimension in figure
%   normalization (struct) normalization struct used in the map in figure
%   comps         (vector) the component indexes in figure
%
% This function gets the handles of component planes and u-matrices in
% subplots p from figure f. SOM_SHOW writes the handles into the
% UserData field of the figure where their order won't be mixed
% up. This function reads the data according to the vector p. If the
% figure has been manipulated (original planes are missing) the function
% warns user or returns error string.
% 
% The main purpose for this is to be a subfuncion for SOM_SHOW_ADD,
% SOM_SHOW_CLEAR and SOM_RECOLORBAR functions, but it may be used on
% command line in the followong manner:
%
%  % plots text on the fifth plane
%  axes(vis_som_show_data(5)); hold on; text(1,3,'I am here');
%    
% See also SOM_SHOW, SOM_SHOW_ADD.

% Copyright (c) 1997-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 201099 juuso 160600

%% Check input args %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(nargchk(0, 2, nargin))  % check no. of input args 

%% Init %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handles=[];                                     % initialize output
normalize=[];
comps=[];
dim=[];
msize=[];
lattice=[];
msg=[];      

cr=sprintf('\n');                               % carriage return            

if nargin < 2 | isempty(f)
  f=gcf;                                        % default figure
end

if nargin < 1 | isempty(p)                      % default p 
  p= 'all';
end

% Find component planes and u-matrices from the figure and get the 
% UserData field where the handles for the components are 
% in the original order. 
% If the fields are corrupted, return an error message.

h_real = [findobj(f, 'Tag', 'Cplane'); ...
    findobj(f, 'Tag', 'Uplane'); ...
    findobj(f,'Tag','CplaneI'); ...
    findobj(f,'Tag','UplaneI')];
eval( 'h_stored=getfield(get(f,''UserData''),''subplotorder'');' , ...
    'msg=[ msg cr '' Missing SOM_SHOW.subplotorder''];');  
eval( 'normalization=getfield(get(f,''UserData''),''comp_norm'');' , ...
    'msg=[msg cr '' Missing SOM_SHOW.comp_norm''];');
eval( 'comps=getfield(get(f,''UserData''),''comps'');' , ...
    'msg=[msg cr '' Missing SOM_SHOW.comps''];');    
eval( 'msize=getfield(get(f,''UserData''),''msize'');' , ...
    'msg=[msg cr '' Missing SOM_SHOW.msize''];');    
eval( 'dim=getfield(get(f,''UserData''),''dim'');' , ...
    'msg=[msg cr '' Missing SOM_SHOW.dim''];');    
eval( 'lattice=getfield(get(f,''UserData''),''lattice'');' , ...
    'msg=[msg cr '' Missing SOM_SHOW.lattice''];');    
if ~isempty(msg), 
  msg=['The figure does not contain SOM_SHOW visualization or is corrupted.'...
	cr msg cr cr ...
	'This command may be applied only to a SOM_SHOW visualization.']; 
  return; 
end

%% Check arguments %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

index=ismember(h_stored, h_real);  % the original order for plot axes 

if ~prod(double(index))                    % missing planes?!
                                           % double added by kr 1.10.02
  l1= 'Some of the original planes seems to be missing.';
  l2= 'Subplot numbers now refer to the existing ones.';
  warning([l1 cr l2]);
end

if ~prod(double(ismember(h_real, h_stored))) % extra planes?! 
                                             % double added by kr 5.9.02
  warning('There seems to be new planes. Subplot numbers refer to the old ones.');
end

h_stored=h_stored(index);          % existing original plots in original order

if ischar(p)                       % check if p is 'all'
  switch(p)
   case 'all'                                   
    p=1:size(h_stored,1);          % all original subplots
   case 'comp'
    p=find(comps>0); 
   otherwise
    msg= 'String value for subplot number vector has to be ''all''!';
    return;
  end
end

if ~vis_valuetype(p,{ '1xn','nx1'}) % check the size
  msg= 'Subplot numbers (argument p in help text) have to be in a vector!';
  return
end

if min(p) < 1                      % check for invalid values
  msg= 'Subplot numbers (argument p in help text) must be at least 1!';
  return
end

%% p is too large

if max(p) > size(h_stored,1)
  l1= 'There are not so many existing subplots created by SOM_SHOW in the';
  l2= 'figure as you are trying to refer with subplot numbers.';
  l3= 'This is probably caused by having a too large subplot number.';
  l4= 'However, the reason may be invalid manipulation of';
  l5= 'this figure object or a program failure, too.';
  msg=([l1 cr l2 cr l3 cr cr l4 cr l5]);
  return;
end

%% Action and building output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handles=h_stored(p);
comps=comps(p);



function h=vis_footnote(txt)

% VIS_FOOTNOTE Adds a movable text to the current figure
%
%  h = vis_footnote(T)
%
%  Input and output arguments ([]'s are optional)
%   [T]  (string) text to be written
%        (scalar) font size to use in all strings 
%
%   h    (vector) handles to axis objects created by this function 
%
% This function sets a text to the current figure. If T is a string,
% it's written as it is to the same place. If T is a scalar, the font
% size of all text objects created by this function are changed to the
% pointsize T. If no input argument is given the function only returns
% the handles to all objects created by this function. The texts may
% be dragged to a new location at any time using mouse.  Note that the
% current axis will be the parent of the text object after dragging.
%
% String 'Info' is set to the Tag property field of the objects. 
% 
% EXAMPLES
%
% % add movable texts to the current figure and change their
% % fontsize to 20 points
% vis_footnote('Faa'); vis_footnote('Foo'); vis_footnote(20);
% 
% % delete all objects created by this function from the current figure
% delete(vis_footnote);
% 
% See also SOM_SHOW.

% Copyright (c) 1997-2000 by the SOM toolbox programming team.
% http://www.cis.hut.fi/projects/somtoolbox/             

% Version 2.0beta Johan 080698

%% Check arguments %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(nargchk(0, 1, nargin))  % check no. of input args

%% Init %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Get the handles to the existing Info-axes objects

h_infotxt=findobj(gcf,'tag','Info','type','text');
h_infoax=findobj(gcf,'tag','Info','type','axes');

%% Action %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If no arguments are given, return the old axes handles

if nargin == 0 | isempty(txt),
  ;  
elseif ischar(txt)                    % text: set new text
  [t,h_]=movetext(txt);
  h_infoax=[h_; h_infoax];
elseif vis_valuetype(txt,{'1x1'})      % scalar: change font size  
  set(h_infotxt,'fontunits','points');
  set(h_infotxt,'fontsize',txt);
else
  error('Input argument should be a string or a scalar.');
end

%% Build output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if nargout>0     % output only if necessary
  h=h_infoax;
end

%%% SUBFUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

function [t,h]=movetext(txt)
% Moves the text. See also VIS_FOOTNOTEBUTTONDOWNFCN
%
%
initpos=[0.05 0.05 0.01 0.01];   

memaxes = gca;                   % Memorize the gca

%% Create new axis on the lower left corner.
%% This will be the parent for the text object

h = axes('position',initpos,'units','normalized');
set(h,'visible','off');          % hide axis

t = text(0,0,txt);               % write text 
set(t,'tag','Info');             % set tag
set(h,'tag','Info');             % set tag

set(t,'verticalalignment','bottom');  % set text alignment
set(t,'horizontalalignment','left');

%% Set ButtonDownFcn

set(t,'buttondownfcn','vis_footnoteButtonDownFcn') 

axes(memaxes);                   % Reset original gca


