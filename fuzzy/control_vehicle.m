global map;
global map_size;

map_size = 400;

max_iterations = 1000;
fis_file = 'my_fuzzy_system.fis';

plans;

create_map(plan_4);

imshow(map);

fis = readfis(fis_file);

approach_goal(fis, max_iterations);

