import numpy
import scipy
import fi_astrosims


"""
:param: simulation: (Simulation) object taken from fi_astrosims to generate
        the query within the established bounds 
:param: half_thickness: 
:param: coarsefac: (int) optimization factor
:param: pix_pos: (array) 3-item array of startpoint, step, and endpoint for 
        the pixels (for ex. [-100 0.5 100] will generate 400 pixels between
        [-100 -99.5 -99.0 ... 99.5 100])
"""
def make_gas_image(simulation, half_thickness, coarsefac, pix_pos):

    # radius over which each star is smeared, units of its volume-based radius
    distfac = 4.5

    # the width of the gaussian profile of each star, units of the radius of
    # the "smoothing circle" (i.e.:
    # distfac times its volume-based radius (||max_allowed_dist if applies)).
    # important to keep it <0.5, otherwise the circles have sharp boundaries
    gausswidthfac = 0.35;

    # create the 1D array of pixel positions
    xxx = numpy.arrange(pix_pos[0], pix_pos[2], pix_pos[1])
    # distance between the two pixels is the size of the bin
    binsize = xxx[1] - xxx[0];
    # number of pixels in one dimension
    npix = len(pix_pos)

    # arbitrary but for saving time
    max_allowed_dist = xxx[-1]/2;

    cur_dist = numpy.zeros((1, 2))

    lb = xxx[0] - max_allowed_dist;
    ub = xxx[-1] + max_allowed_dist;

    q = fi_astrosims.client.Query(simulation, Pos_x=(lb, ub), Pos_y=(lb, ub),
                                  z=(-max_allowed_dist, max_allowed_dist))

    masses = 1
    densities = 1
    posMat = [1,1]

    dists = distfac * ((4 / (3 * numpy.pi)) * masses / densities)**(1/3)
    dists[dists > max_allowed_dist] = max_allowed_dist
    maxdist = numpy.max(dists)

    scores = numpy.zeros((1, 1+numpy.ceil(maxdist / binsize)))
    scores_coarse = numpy.zeros((1, 1+numpy.ceil(maxdist / binsize / coarsefac)))

    for c in range(0, len(scores)):
        dist = c-1
        i = [-dist, dist]
        x_i = i * numpy.ones((len(i), 1))
        y_i = i * numpy.ones((1, len(i)))
        kernalmat = x_i**2 + y_i**2
        kernalmat = kernalmat[kernalmat**(1/2) <= dist]
        gausswidth = gausswidthfac * numpy.max(dist, 0.5)
        weights = numpy.exp(-(kernalmat+0.25) / 2 / gausswidth**2)
        scores[c] = sum(weights)

    imageMatrix = numpy.zeros((npix, npix))
    imageMatrix_Coarse = numpy.zeros(npix / coarsefac, npix / coarsefac)

    for i in range(0, len(masses)):
        if dists(i) > 50:
            cucroarsefac = coarsefac
        else:
            curcoarsefac = -1
        binsize = binsize * curcoarsefac
        dists[i] = numpy.floor((dists[i] / curcoarsefac))
        curwidth = gausswidthfac*numpy.max((dists[i], 0.5))
        pix_pos = numpy.floor((posMat[:, 1] - xxx[1]) / binsize) + 1
