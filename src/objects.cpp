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
    NumericMatrix nodes(0, 2);
    if((object_class == "polygon") || (object_class == "rectangle")) {
        // Extract the points of the object and mend them into segments
        NumericMatrix points = object.slot("points"); 
        int n = points.nrow();
    
        IntegerVector idx = seq(1, n - 1);
        idx.push_back(0);

        NumericMatrix matched_points(points.nrow(), points.ncol());
        for(int i = 0; i < points.nrow(); i++) {
            matched_points(i, _) = points(idx[i], _);
        }

        NumericVector points_x = points(_, 0);
        NumericVector matched_x = matched_points(_, 0);
        NumericVector points_y = points(_, 1);
        NumericVector matched_y = matched_points(_, 1);

        NumericMatrix x_changes = cbind(points_x, matched_x);
        NumericMatrix y_changes = cbind(points_y, matched_y);

        NumericVector x(0);
        NumericVector y(0);
        for(int i = 0; i < x_changes.nrow(); i++) {
            // Get the number of nodes that you would have to create in the x 
            // and y direction to cover the whole thing
            double len_x = (x_changes(i, 1) - x_changes(i, 0)) / space_between;
            double len_y = (y_changes(i, 1) - y_changes(i, 0)) / space_between;

            // Based on these two lengths, find out what the length should be 
            // of the tilted line between the two points using Pythagoras. Then
            // transform to the upper integer.
            double a = std::pow(len_x, 2);
            double b = std::pow(len_y, 2);
            double c = a + b;

            double len = std::sqrt(c);
            NumericVector len_vec(1);
            len_vec[0] = len;

            int len_int = ceiling(len_vec)[0];
            double denominator = len_int;

            // Create the x and y nodes with the seq function
            for(int j = 0; j <= len_int; j++) {
                // Define how far along you are on the line. You have to convert
                // the numerator and denominator to doubles in order for the 
                // relative location to be computed correctly (as a double 
                // itself)
                double numerator = j;
                double rel_loc = numerator / denominator;

                // Define coordinates to show how far along the line you are
                double xij = x_changes(i, 0) + rel_loc * (x_changes(i, 1) - x_changes(i, 0));
                double yij = y_changes(i, 0) + rel_loc * (y_changes(i, 1) - y_changes(i, 0));

                // Add to the x and y vectors
                x.push_back(xij);
                y.push_back(yij);
            }
        }
        
        // Bind the two vectors together
        nodes = cbind(x, y);

    // For circles, we use the angles of a circle to achieve the placement of 
    // nodes.
    } else if(object_class == "circle") {
        // Compute the circumference of the circle and, based on that, compute
        // the number of nodes that you need to output
        double radius = object.slot("radius");
        double circumference = 2 * M_PI * radius;
        double len_float = circumference / space_between;

        NumericVector len_vec(1);
        len_vec[0] = len_float;
        int len = ceiling(len_vec)[0];

        // Define the angles at which the nodes should be placed if we want to 
        // have len nodes on the circumference. Also delete the one angle that 
        // is the same, it being 0 and 2 \pi
        NumericVector angles(len);
        double denominator = len;
        for(int i = 0; i < len; i++) {
            double numerator = i;

            angles[i] = numerator / denominator;
            angles[i] = angles[i] * 2 * M_PI;
        }

        // Create the points based on trigometry.
        NumericVector x = radius * cos(angles);
        NumericVector y = radius * sin(angles);


        // Add the position of the object to it as well.
        NumericVector position = object.slot("center");
        x = x + position[0];
        y = y + position[1];

        // Bind together
        nodes = cbind(x, y);
    }

    return nodes;
}