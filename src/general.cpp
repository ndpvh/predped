#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
std::unordered_set<std::string> unique(CharacterVector x) {
    auto X = Rcpp::as<std::vector<std::string>>(x);
    return std::unordered_set<std::string>(X.begin(), X.end());
}

//' Rcpp version of line_line_intersection. Computes the  between several segments. Is a vectorized 
//' function with minimal loss of time when the number of segments to test 
//' increases.
//'
//' @param segments_1 Matrix with four columns denoting the x- and y-coordinates
//' that make up the line segment. Should be in order x_1, y_1, x_2, y_2.
//' @param segments_2 Matrix of line segments that `segments_1` should be tested
//' with. Should have the same structure as `segments_1`
//'
//' @return Returns a logical denoting whether any of the segments in 
//' 
//' @export
// [[Rcpp::export]]
LogicalVector line_line_intersection_rcpp(NumericMatrix segments_1, 
                                          NumericMatrix segments_2) {

    // Predefine some variables
    double t = 0.;
    double u = 0.;
    double t_max = 0.;
    double u_max = 0.;
    LogicalVector result(segments_1.nrow() * segments_2.nrow());

    // Loop over the instances of both segments
    int n = segments_2.nrow();
    for(int i = 0; i < segments_1.nrow(); i++) {
        for(int j = 0; j < segments_2.nrow(); j++) {
            // Compute the values of t, u and the values they can maximally take 
            // t_max, u_max of the Bézier parametrization of the line segments. 
            // If 0 <= t <= t_max and 0 <= u <= u_max, then the intersection 
            // between two lines lies within the boundaries that make up that 
            // line. 
            t = (segments_1(i, 0) - segments_2(j, 0)) * 
                (segments_2(j, 1) - segments_2(j, 3)) -
                (segments_1(i, 1) - segments_2(j, 1)) * 
                (segments_2(j, 0) - segments_2(j, 2));

            u = (segments_1(i, 0) - segments_1(i, 2)) * 
                (segments_1(i, 1) - segments_2(j, 1)) -
                (segments_1(i, 1) - segments_1(i, 3)) * 
                (segments_1(i, 0) - segments_2(j, 0));

            t_max = (segments_1(i, 0) - segments_1(i, 2)) * 
                (segments_2(j, 1) - segments_2(j, 3)) -
                (segments_1(i, 1) - segments_1(i, 3)) * 
                (segments_2(j, 0) - segments_2(j, 2));
            u_max = -t_max;

            // Do the test itself:
            //
            // Important limitation: End points are not regarded as intersecting.
            // This is done because of some weird bugs with parallel lines that are 
            // regarded as intersecting while not intersecting at all.
            if(t_max < 0.) {
                t = -t;
            }

            if(u_max < 0.) {
                u = -u;
            }

            result[i * n + j] = (0 < t) && (t <= abs(t_max)) && (0 < u) && (u <= abs(u_max)); 
        }
    }
    
    return result;
}