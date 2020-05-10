#' Perform the angular sweep algorithm on a data frame of points
#'
#' @inheritParams sweep_
#'
#' @return a data frame
#' @export
sweep_points <- function(df,
                         xcol,
                         ycol,
                         weight = NULL,
                         radius,
                         inc_data = FALSE) {

  fun <- sweep_(df, xcol, ycol, weight, radius, inc_data)

  # for cartesian points, the distance function does not need to be geodesic, and no reprojection required
  fun(dist_fun = dist_matrix, crs_transform = FALSE)

}


#' Perform the angular sweep algorithm on a data frame of coordinates
#'
#' @param lon the name of the column containing the longitude coordinate
#' @param lat the name of the column containing the latitude coordinate
#'
#' @inheritParams sweep_
#'
#' @importFrom geosphere distm
#'
#' @return a data frame
#' @export
sweep_latlons <- function(df,
                          lon,
                          lat,
                          weight = NULL,
                          radius,
                          inc_data = FALSE) {

  warning(
    paste("This function is experimental! Proceed with caution.",
          "I am still trying to determine the best way to reproject",
          "the points for accurate measures of distance between them", sep = "\n")
  )

  xcol <- lon
  ycol <- lat

  fun <- sweep_(df, xcol, ycol, weight, radius, inc_data)

  # for latlons, the distance function needs to be geodesic, and coordinates need to be re-projected
  fun(dist_fun = geosphere::distm, crs_transform = TRUE)

}

#' Create a basic sweep function
#'
#' @param df the data frame containing coordinates
#' @param xcol the name of the column containing the x coordinate
#' @param ycol the name of the column containing the y coordinate
#' @param weight name of column containing data with which to weight the points
#' @param radius the radius of the circle to sweep with (in metres)
#' @param inc_data logical. Whether or not to include the original data alongside
#'     the indices of the points included in each circle
#'
#' @importFrom stats setNames
#' @importFrom utils flush.console setTxtProgressBar txtProgressBar
#'
#' @return a function with two arguments
#' \describe{
#'   \item{dist_fun}{The function to use to calculate distance between points}
#'   \item{crs_transform}{Whether or not xcol and ycol are latlons, and need to be transformed to coordinate projection}
#' }
sweep_ <- function(df,
                   xcol,
                   ycol,
                   weight = NULL,
                   radius,
                   inc_data = FALSE) {

  pre_sweep_check(df, xcol, ycol, weight, radius, inc_data)

  function(dist_fun, crs_transform){

    # Add weight in if null
    if(is.null(weight)){
      weight <- "weight"
      df[[weight]] <- rep(1, length.out = nrow(df))
    }

    # add ID to df and capture raw DF before manipulating further
    df$id <- seq_along(df[[1]])
    df_raw <- df

    # keep only columns we'll need from df
    df <- df[c("id", xcol, ycol, weight)]

    # add suffix to df_raw xcol and ycol
    names(df_raw)[names(df_raw) %in% c(xcol, ycol)] <-
      paste0(names(df_raw)[names(df_raw) %in% c(xcol, ycol)], "_loc")

    # Create Distance Matrix --------------------------------------------------

    message("Computing distance matrix")
    flush.console()

    dist <-
      dist_fun(df[c(xcol, ycol)])

    message("Distance matrix...done")
    flush.console()

    # Separate lonely points: those with no other points within 1 diameter
    over_1_index <- rowSums(dist <= 2*radius) > 1

    if(sum(over_1_index) == 0) {
      stop("No circles of specified radius contain more than 1 point -- nothing to aggregate")
    }

    lonely_points <- df[!over_1_index, ]

    names(lonely_points)[names(lonely_points) == "id"] <- "in_circle"
    names(lonely_points)[names(lonely_points) == weight] <- "total"
    class(lonely_points[["in_circle"]]) <- "list"

    df <- df[over_1_index, ]

    # Prep for point by point sweeping
    # Progress bar initialisation
    total_points_to_check <- cumsum(rowSums(dist <= 2*radius))
    pb <- txtProgressBar(min = 0, max = max(total_points_to_check), style = 3)

    # Create empty results list to populate
    results <- vector(mode = "list", length = length(df$id))

    for(i in df$id) {

      # create index of nearby points
      nearby_index <-
        which(dist[i, ] <= 2*radius & dist[i, ] > 0)

      # create df of nearby points
      nearby_data <-
        df[df$id %in% nearby_index, ]

      # create df of specific point
      same_point_index <-
        which(dist[i, ] == 0)

      point_data <-
        df[df$id %in% same_point_index, ]

      # change CRS projection
      if(crs_transform) {
        utm_crs <- lonlat_to_utm(unlist(point_data[point_data$id == i, ][c(xcol, ycol)]))

        point_data <- change_projection(point_data, xcol, ycol, 4326, utm_crs)
        nearby_data <- change_projection(nearby_data, xcol, ycol, 4326, utm_crs)
      }

      # add thetas in and out of nearby dataset
      A <- big_a(point_data[[xcol]][1], point_data[[ycol]][1], nearby_data[[xcol]], nearby_data[[ycol]])
      B <- big_b(radius, dist[i, nearby_index])

      nearby_data$theta_in <- normalise_angle(A - B)
      nearby_data$theta_out <- normalise_angle(A + B)

      # creat df of all thetas
      all_thetas <-
        data.frame(theta = unlist(nearby_data[c("theta_in", "theta_out")], use.names = FALSE),
                   id = rep(nearby_data$id, 2),
                   in_ = c(rep(TRUE, nrow(nearby_data)), rep(FALSE, nrow(nearby_data))))

      in_circle <- vector(mode = "list", length = 2*nrow(nearby_data))

      all_thetas <- all_thetas[order(all_thetas$theta), ]

      # initial circle
      # identify ids which entered earlier and have not exited yet
      in_circle[[1]] <-
        sort( c(nearby_data$id[
          which(normalise_angle(all_thetas$theta[1] - nearby_data$theta_in) >= 0 &
                  normalise_angle(all_thetas$theta[1] - nearby_data$theta_out) <= 0)
          ], point_data$id) ) # includes all points in point_data

      # if first theta is a theta_out remove it from the initial
      if(!all_thetas$in_[1]) {
        in_circle[[1]] <-
          sort( setdiff(in_circle[[1]], all_thetas$id[1]) )
      }

      # for the rest of the rows, simply add the theta_in dots and remove
      # the theta_out ones
      for(j in 2:nrow(all_thetas)) {
        if(all_thetas$in_[j]) {
          in_circle[[j]] <-
            sort( c(in_circle[[j-1]], all_thetas$id[j]) )
        } else {
          in_circle[[j]] <-
            sort( setdiff(in_circle[[j-1]], all_thetas$id[j]) )
        }
      }

      # assemble data frame from the in_circle indexes
      origin <- setNames(unlist(point_data[point_data$id == i,][c(xcol, ycol)]), c("x", "y"))

      matrix <- pol2cart(radius, all_thetas$theta, origin)

      dimnames(matrix)[[2]] <- c(xcol, ycol)

      circles <-
        as.data.frame(matrix)

      circles$in_circle <-
        in_circle

      circles$total <-
        sapply(circles$in_circle, function(ids) {
          sum(df[[weight]][df$id %in% ids])
        })

      if(crs_transform) {
        circles <- change_projection(circles, xcol, ycol, utm_crs, 4326)
      }

      setTxtProgressBar(pb, total_points_to_check[match(i, df$id)])

      results[[match(i, df$id)]] <- circles
    }

    results <-
      do.call(rbind, results)

    if(exists("lonely_points")) {
      results <-
        rbind(results, lonely_points)
    }

    results <- results[order(-results$total), ]

    if(inc_data) {
      results$data <- lapply(results$in_circle, function(ids){
        df_raw[df_raw$id %in% ids, ]
      })
    }

    unique_results_index <-
      (!duplicated(results$in_circle)) & results$total > 0

    results <-
      results[unique_results_index, ]

    rownames(results) <- NULL

    return(results)
  }
}
