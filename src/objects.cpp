#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include "m4ma.h"
#include <math.h>

using namespace Rcpp;

//' Add Nodes on the Circumference of an Object
//' 
//' Rcpp alternative of \code{\link[predped]{nodes_on_circumference}}.
//' 
//' Used in the \code{\link[predped]{overlap_with_objects}} function for creating 
//' nodes of which their presence within an agent can be checked in an efficient 
//' way (see \code{\link[predped]{moving_options-method}} and 
//' \code{\link[predped]{in_object-method}}). Currently works for all 
//' instances of \code{\link[predped]{object-class}}, but only returns 
//' \code{NULL} for the \code{\link[predped]{segment-class}}.
//' 
//' @details 
//' Related to the \code{\link[predped]{add_nodes-method}} with the main difference
//' being that the \code{\link[predped]{add_nodes-method}} adds nodes around or 
//' within an object, while \code{nodes_on_circumference} adds nodes directly on
//' the circumference of an object.
//' 
//' Note that while \code{\link[predped]{rectangle-class}} is not explicitly 
//' mentioned here, this method does work for this class of objects.
//'
//' @param object Object of \code{\link[predped]{object-class}}.
//' @param space_between Numeric denoting the space to leave between the 
//' circumference of the object and the nodes to create. When \code{outside = TRUE},
//' \code{space_between} distance is created to the outside of the object, while
//' if \code{outside = FALSE} this same distance is created towards the inside 
//' of the object. Defaults to \code{5e-2}.
//'
//' @return Numerical matrix containing the nodes that were created around/within
//' the provided object.
//' 
//' @examples 
//' # Create an object
//' my_circle <- circle(center = c(0, 0), radius = 1)
//' 
//' # Generate nodes that fall around this circle with a distance of 1 around 
//' # the circle
//' nodes_on_circumference(my_circle, space_between = pi / 2, cpp = TRUE)
//' 
//' @seealso 
//' \code{\link[predped]{circle-class}}, 
//' \code{\link[predped]{polygon-class}},  
//' \code{\link[predped]{rectangle-class}},
//' \code{\link[predped]{segment-class}},
//' \code{\link[predped]{add_nodes}},
//' \code{\link[predped]{in_object}}, 
//' \code{\link[predped]{moving_options}}
//' 
//' @docType method
//' 
//' @rdname nodes_on_circumference_rcpp
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix nodes_on_circumference_rcpp(S4 object,
                                          double space_between) {

    // Check the class of the object. Depending on this, the computation 
    // is different
    std::string object_class = object.attr("class");

    // For polygons and rectangles, we base ourselves on the linearity of the 
    // lines on which we can add nodes. 
    if((object_class == "polygon") || (object_class == "rectangle")) {
        // Extract the points of the object and mend them into segments
        NumericMatrix points = object.slot("points"); 
        NumericVector x = points(_, 0);
        NumericVector y = points(_, 1);
        x.push_back(x[0]);
        y.push_back(y[0]);

        int n = points.nrow();

        // Loop over each of the points
        NumericVector len(n);
        for(int i = 0; i < n; i++) {
            // Get the number of nodes that you would have to create in the x 
            // and y direction to cover the whole thing
            double squared = std::sqrt((x(i + 1) - x(i)) * (x(i + 1) - x(i)) + (y(i + 1) - y(i)) * (y(i + 1) - y(i)));
            len[i] = std::ceil(squared / space_between);
        }

        // // Loop over each of the points again, but now with initialization of 
        // // x and y
        int m = sum(len) + n;
        NumericVector points_x(m);
        NumericVector points_y(m);
        NumericVector diff_x(m);
        NumericVector diff_y(m);
        NumericVector rel_loc(m);

        int k = 0;
        for(int i = 0; i < n; i++) {
            // Get an index that tells you where to assign to
            Rcpp::Range idx = seq(k, k + len[i]);

            // Assign repetitions of the first point to their vectors
            points_x[idx] = rep(x[i], len[i] + 1);
            points_y[idx] = rep(y[i], len[i] + 1);

            // Assign the difference between two consecutive points to their 
            // respective vectors
            diff_x[idx] = rep(x[i + 1] - x[i], len[i] + 1);
            diff_y[idx] = rep(y[i + 1] - y[i], len[i] + 1);

            // Assign a vector of factors with which to multiply the differences
            // in x and y
            NumericVector sequence(len[i] + 1);
            std::iota(sequence.begin(), sequence.end(), 0);
            rel_loc[idx] = sequence / len[i];

            k += len[i] + 1;
        }

        NumericVector res_x = points_x + rel_loc * diff_x;
        NumericVector res_y = points_y + rel_loc * diff_y;
        return cbind(res_x, res_y);

    // For circles, we use the angles of a circle to achieve the placement of 
    // nodes.
    } else if((object_class == "circle") || (object_class == "agent")) {

        // Compute the circumference of the circle and, based on that, compute
        // the number of nodes that you need to output
        double radius = object.slot("radius");
        int len = std::ceil(2 * M_PI * radius / space_between);

        // Define the angles at which the nodes should be placed if we want to 
        // have len nodes on the circumference. Note that in using .begin() and 
        // .end(), we automatically don't compute the value for angle 2\pi, 
        // which is what we want as this value would be the same as the one 
        // for 0.
        NumericVector sequence(len);
        std::iota(sequence.begin(), sequence.end(), 0);
        NumericVector angles = 2 * M_PI * sequence / len;

        // Create the points based on trigometry.
        NumericVector position = object.slot("center");
        NumericVector x = position[0] + radius * cos(angles);
        NumericVector y = position[1] + radius * sin(angles);

        return cbind(x, y);
    }

    return NumericMatrix(0, 2);
}

//' Check Whether a Point Lies Within an Object
//' 
//' Currently works for all classes inside of the \code{\link[predped]{object-class}}.
//'
//' @param object Object of the \code{\link[predped]{object-class}}.
//' @param x Numeric vector or matrix containing x- and y-coordinates to be 
//' checked.
//'
//' @return Logical whether the point is inside of the object (\code{TRUE}) or 
//' outside of the object (\code{FALSE}).
//' 
//' @examples 
//' # Create an object
//' my_circle <- circle(center = c(0, 0), radius = 1)
//' 
//' # Let's create a matrix of different coordinates of which the first is 
//' # inside of the object, the second on its circumference, and the third  
//' # outside of the object
//' coords <- rbind(c(0, 0), 
//'                 c(1, 0), 
//'                 c(2, 0))
//' 
//' # Let's do the test
//' in_object_rcpp(my_circle, coords)
//' 
//' @seealso 
//' \code{\link[predped]{circle-class}}, 
//' \code{\link[predped]{polygon-class}},  
//' \code{\link[predped]{rectangle-class}},
//' \code{\link[predped]{segment-class}},
//' \code{\link[predped]{out_object}}, 
//' \code{\link[predped]{moving_options}}
//' 
//' @docType method
//' 
//' @rdname in_object_rcpp
//' 
//' @export
// [[Rcpp::export]]
LogicalVector in_object_rcpp(S4 object,
                             NumericMatrix x) {

    // Check the class of the object. Depending on this, the computation 
    // is different
    std::string object_class = object.attr("class");
    LogicalVector result(x.nrow());

    // For polygons and rectangles, we will use the raycasting algorithm to find
    // out whether a point lies within the object
    if((object_class == "polygon") || (object_class == "rectangle")) {
        // Extract the points of the object and make them into edges or segments
        NumericMatrix points = object.slot("points");
        NumericVector xp = points(_, 0);
        NumericVector yp = points(_, 1);
        xp.push_back(xp[0]);
        yp.push_back(yp[0]); 

        // Define the number of edges to loop over, as well as the number of 
        // points to check.
        for(int i = 0; i < x.nrow(); i++) {
            // Initialize a counter that will tell us how many segments the point
            // crosses through raycasting.
            int counter = 0;

            // Loop over the different segments and check whether a line starting 
            // from the point and driving on to infinity leads to an intersection 
            // with the segments. If so, increase the counter
            for(int j = 0; j < xp.length() - 1; j++) {
                // First check: The y-coordinate of the point is above or below 
                // the y-coordinates that make up the segment. Can only intersect 
                // if at least one is above and one is below, or vice-versa.
                bool check_1 = (yp[j] > x(i, 1)) != (yp[j + 1] > x(i, 1));

                // Second check: Use a derived formula to find out whether 
                // drawing a horizontal line from the starting point to infinity 
                // will lead to an intersection with this segment.
                double slope = (xp[j + 1] - xp[j]) / (yp[j + 1] - yp[j]);
                double x_intersection = xp[j] + slope * (x(i, 1) - yp[j]);

                bool check_2 = x(i, 0) < x_intersection;

                // Update the counter if both checks are TRUE.
                if(check_1 & check_2) {
                    counter++;
                }
            }

            // After checking per segment, we can use the counter to check whether
            // the point lies inside or outside of the object. Inside when uneven,
            // outside when even.
            result[i] = (counter % 2 == 1);
        }

    // For circles, will use the distance from the point to the center of the 
    // circle to find out whether a point lies within this circle
    } else if((object_class == "circle") || (object_class == "agent")) {

        // Compute the distance between the coordinates and the center of the 
        // of the circle. Then check whether this distance is smaller thna the 
        // radius of the circle.
        NumericVector y = object.slot("center");
        double radius = object.slot("radius");
        radius = radius * radius;

        for(int i = 0; i < x.nrow(); i++) {
            double dist = (x(i, 0) - y[0]) * (x(i, 0) - y[0]) + (x(i, 1) - y[1]) * (x(i, 1) - y[1]);
            result[i] = dist < radius;
        }
    }

    return result;
}