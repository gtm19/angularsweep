#' Calculate angle between PQ and the X Axis for arbitrary but distinct points P and Q
#'
#' This angle is called `A` in
#' [this GeeksForGeeks post](https://www.geeksforgeeks.org/angular-sweep-maximum-points-can-enclosed-circle-given-radius/)
#'
#' @param px the x-coordinate of point P
#' @param py the y-coordinate of point P
#' @param qx the x-coordinate of point Q
#' @param qy the y-coordinate of point Q
#'
#' @return a numeric angle A between \eqn{[-\pi, \pi]}.
big_a <- function(px, py, qx, qy) {

  if(identical(px, qx) && identical(py, qy)) {
    stop("Points P and Q must be distinct")
  }

  diff_y <- qy - py
  diff_x <- qx - px

  angle <- atan(
    diff_y / diff_x
  )

  angle[diff_y >= 0 & diff_x < 0] <- angle[diff_y >= 0 & diff_x < 0] + pi
  angle[diff_y < 0 & diff_x < 0] <- angle[diff_y < 0 & diff_x < 0] - pi

  return(angle)

}

#' Calculate angle between PC and PQ where C is the centre of the circle on which both P and Q lie
#'
#' This angle is called `B` in
#' [this GeeksForGeeks post](https://www.geeksforgeeks.org/angular-sweep-maximum-points-can-enclosed-circle-given-radius/)
#'
#' @param r the radius of the circle
#' @param d the Euclidean distance between points P and Q
#'
#' @return a numeric angle B between \eqn{[0,\pi/2]}.
big_b <- function(r, d) {

  if(any(d == 0)) {
    stop("The distance between P and Q is zero. Points P and Q must be distinct")
  } else if (any(d < 0)) {
    stop("The distance between P and Q cannot be less than zero")
  } else if (any(2*r < d)) {
    stop("A circle with radius less than half the distance between P and Q cannot contain both points")
  }

  acos(
    d / (2*r)
  )
}

#' Moves angles less than \eqn{-\pi} or greater than \eqn{\pi} to within this range
#'
#' @param thetas a numeric vector of angles
#'
#' @return a numeric vector of angles within the range \eqn{[-\pi, \pi]}
normalise_angle <- function(thetas) {

  i <- which((thetas < -pi) | (thetas > pi) & !is.na(thetas))

  if(length(i) > 0) {
    thetas[i] <- ((thetas[i] + pi) %% (2*pi)) - pi
  }

  return(thetas)

}

#' Converts polar coordinates to cartesian
#'
#' @param r vector of radial coordinate(s), the distance from the origin
#' @param theta vector of polar angle(s), the counterclockwise angle from the x-axis
#' @param origin a named vector (with `x` and `y` as names) being the location of the origin
#'
#' @return a 2 column matrix with names `x` and `y` with the cartesian coordinates therein
pol2cart <- function(r, theta, origin = c(x = 0, y = 0)) {

  if(!identical(sort(names(origin)), c("x", "y"))) {
    stop("Names of origin must be `x` and `y`")
  }

  x <- r*cos(theta)
  y <- r*sin(theta)

  matrix(
    c(x = origin["x"] + x, y = origin["y"] + y),
    ncol = 2,
    dimnames = list(NULL, c("x", "y"))
  )
}
