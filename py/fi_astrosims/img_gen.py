import numpy as np
import scipy as s
import fi_astrosims

def make_gas_image(simulation, half_thickness, coarsefac, xxx):

    # radius over which each star is smeared, units of its volume-based radius
    distfac = 4.5

    # the width of the gaussian profile of each star, units of the radius of
    # the "smoothing circle" (i.e.:
    # distfac times its volume-based radius (||max_allowed_dist if applies)).
    # important to keep it <0.5, otherwise the circles have sharp boundaries
    gausswidthfac = 0.35;

    # arbitrary but for saving time
    max_allowed_dist = xxx(end)/2;

    lb = xxx(1) - max_allowed_dist;
    ub = xxx(end) + max_allowed_dist;

    q = fi_astrosims.client.Query(simulation, Pos_x=(lb, ub), Pos_y=(lb, ub),
                                  z=(-max_allowed_dist, max_allowed_dist))

