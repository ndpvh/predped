// EKF + RTS smoother for constant-velocity model.
// State: [x, y, v, theta]  (position, speed m/s, heading radians)
// Process model (nonlinear):
//   x_{k+1} = x_k + v_k * cos(theta_k) * dt
//   y_{k+1} = y_k + v_k * sin(theta_k) * dt
//   v_{k+1} = v_k,  theta_{k+1} = theta_k
// Two measurement modes:
//   optimal = false: z = [x, y]                   (H is 2x4 selecting x,y)
//   optimal = true:  z = [x, y, v, theta]          (H = I4)

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// Wrap angle to (-pi, pi]
static inline double wrap_angle(double a) {
    while (a >  M_PI) a -= 2.0 * M_PI;
    while (a <= -M_PI) a += 2.0 * M_PI;
    return a;
}

//' Extended Kalman Filter + Rauch-Tung-Striebel smoother (Rcpp/Armadillo)
//'
//' Fits a constant-velocity model [x, y, v, theta] to a noisy trajectory.
//' Called from the R wrapper \code{.ekalman_smooth()} when \code{use_rcpp=TRUE}.
//'
//' @param x_obs   Numeric vector of observed x positions (length n).
//' @param y_obs   Numeric vector of observed y positions (length n).
//' @param t_obs   Numeric vector of observation times (length n).
//' @param speed_obs   Numeric vector of speed observations (length n); used only
//'   when \code{optimal=TRUE}.
//' @param theta_obs   Numeric vector of heading observations in radians (length n);
//'   used only when \code{optimal=TRUE}.
//' @param R_mat   2x2 (optimal=FALSE) or 4x4 (optimal=TRUE) measurement noise matrix.
//' @param P0_mat  4x4 initial state covariance.
//' @param x0      Length-4 initial state vector [x, y, v, theta].
//' @param q_pos_x Process noise for x position (scalar).
//' @param q_pos_y Process noise for y position (scalar).
//' @param q_v     Process noise for speed (scalar).
//' @param q_th    Process noise for heading (scalar).
//' @param mean_dt Mean time step (used to scale process noise).
//' @param optimal Logical; when TRUE uses 4D measurement model.
//'
//' @return List with smoothed vectors: x, y, speed, orientation (radians).
//'
//' @concept noiser
//'
// [[Rcpp::export]]
List ekalman_smooth_rcpp(NumericVector x_obs,
                         NumericVector y_obs,
                         NumericVector t_obs,
                         NumericVector speed_obs,
                         NumericVector theta_obs,
                         arma::mat     R_mat,
                         arma::mat     P0_mat,
                         arma::vec     x0,
                         double        q_pos_x,
                         double        q_pos_y,
                         double        q_v,
                         double        q_th,
                         double        mean_dt,
                         bool          optimal) {

    int n = x_obs.size();

    // Storage for forward pass
    arma::mat xf(n, 4, fill::zeros);   // filtered means
    arma::mat xp(n, 4, fill::zeros);   // predicted means
    arma::cube Pf(4, 4, n, fill::zeros);  // filtered covars
    arma::cube Pp(4, 4, n, fill::zeros);  // predicted covars
    arma::cube Fa(4, 4, n - 1, fill::zeros); // Jacobians

    const arma::mat I4 = eye(4, 4);
    const arma::mat eps4 = 1e-12 * I4;

    // Measurement matrix H and regularisation
    arma::mat H;
    arma::mat R_use = R_mat;
    if (optimal) {
        H = I4;
    } else {
        H = join_horiz(eye(2, 2), zeros(2, 2));
    }
    arma::mat eps_S = 1e-12 * eye(R_use.n_rows, R_use.n_rows);

    // Process-noise scale (divided by mean_dt once)
    double qsx = q_pos_x / mean_dt;
    double qsy = q_pos_y / mean_dt;
    double qsv = q_v     / mean_dt;
    double qst = q_th    / mean_dt;

    // Initialise prior
    xp.row(0) = x0.t();
    Pp.slice(0) = P0_mat;

    // ── FORWARD EKF PASS ───────────────────────────────────────────────────────
    for (int i = 0; i < n; ++i) {
        arma::vec xc = xp.row(i).t();
        arma::mat Pc = Pp.slice(i);

        // Innovation
        arma::vec z, inn;
        if (optimal) {
            z   = { x_obs[i], y_obs[i], speed_obs[i], theta_obs[i] };
            inn = z - xc;
            inn(3) = wrap_angle(inn(3));
        } else {
            z   = { x_obs[i], y_obs[i] };
            inn = z - H * xc;
        }

        // Innovation covariance and Kalman gain
        arma::mat S    = H * Pc * H.t() + R_use;
        arma::mat Sinv = inv_sympd(S + eps_S);
        arma::mat K    = Pc * H.t() * Sinv;

        arma::vec xu = xc + K * inn;
        xu(3) = wrap_angle(xu(3));
        arma::mat Pu = (I4 - K * H) * Pc;
        Pu = 0.5 * (Pu + Pu.t());

        xf.row(i) = xu.t();
        Pf.slice(i) = Pu;

        // ── Predict to i+1 ──────────────────────────────────────────────────
        if (i < n - 1) {
            double dti = t_obs[i + 1] - t_obs[i];
            double vi  = xu(2);
            double thi = xu(3);
            double cth = std::cos(thi);
            double sth = std::sin(thi);

            arma::mat Fi = {
                { 1, 0, cth * dti, -vi * sth * dti },
                { 0, 1, sth * dti,  vi * cth * dti },
                { 0, 0, 1,          0               },
                { 0, 0, 0,          1               }
            };

            arma::vec xn = {
                xu(0) + vi * cth * dti,
                xu(1) + vi * sth * dti,
                vi,
                wrap_angle(thi)
            };

            arma::mat Qi = diagmat(arma::vec({
                qsx * dti, qsy * dti, qsv * dti, qst * dti
            }));

            arma::mat Pn = Fi * Pu * Fi.t() + Qi;
            Pn = 0.5 * (Pn + Pn.t());

            xp.row(i + 1) = xn.t();
            Pp.slice(i + 1) = Pn;
            Fa.slice(i) = Fi;
        }
    }

    // ── BACKWARD RTS SMOOTHER ─────────────────────────────────────────────────
    arma::mat xs = xf;

    for (int i = n - 2; i >= 0; --i) {
        arma::mat Pp_next = Pp.slice(i + 1) + eps4;
        arma::mat G = Pf.slice(i) * Fa.slice(i).t() * inv_sympd(Pp_next);
        arma::vec diff_x = xs.row(i + 1).t() - xp.row(i + 1).t();
        xs.row(i) = xf.row(i) + (G * diff_x).t();
        xs(i, 3) = wrap_angle(xs(i, 3));
    }

    return List::create(
        Named("x")           = arma::vec(xs.col(0)),
        Named("y")           = arma::vec(xs.col(1)),
        Named("speed")       = arma::vec(arma::clamp(xs.col(2), 0.0, 1e9)),
        Named("orientation") = arma::vec(xs.col(3))   // radians; R wrapper converts to degrees
    );
}
