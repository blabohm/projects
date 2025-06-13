#' Snap Points to Nearest Line
#'
#' This function snaps each point to the nearest position on the nearest line within a specified distance.
#'
#' @param points `sf` object. Points to be snapped (geometry type: POINT).
#' @param lines `sf` object. Line geometries to which points should be snapped (geometry type: LINESTRING).
#' @param maxDist Numeric. Maximum snapping distance in meters. Points farther than this will be excluded. Default is 1000.
#'
#' @return An `sf` object of snapped points within `maxDist`, preserving the original point attributes.
#' @export
st_snap_points <- function(points, lines, maxDist = 1000) {
  point_data <- st_drop_geometry(points)
  
  d <- st_distance(points, lines)
  dist_to_line <- apply(d, 1, min, na.rm = TRUE)
  valid_points <- dist_to_line <= maxDist

  dist_to_point <- apply(d, 2, min, na.rm = TRUE)
  valid_lines <- dist_to_point <= maxDist

  points <- points[valid_points, ]
  lines <- lines[valid_lines, ]
  d <- d[valid_points, valid_lines, drop = FALSE]
  dist_to_line <- dist_to_line[valid_points]

  if (!any(valid_points)) {
    message("No points closer than threshold!")
    return(invisible(NULL))
  }

  nearest_line_index <- apply(d, 1, which.min)
  coords_lines <- st_coordinates(lines)
  coords_points <- st_coordinates(points)

  new_coords <- vapply(
    seq_len(nrow(points)),
    function(i) {
      line_coords <- coords_lines[coords_lines[, 3] == nearest_line_index[i], ]
      nearestPointOnLine(line_coords, coords_points[i, ])
    },
    FUN.VALUE = c(0, 0)
  )

  snapped_points <- t(new_coords) %>%
    as_tibble() %>%
    st_as_sf(coords = c("X", "Y"), crs = 3035) %>%
    bind_cols(point_data[valid_points, ])

  return(snapped_points)
}

#' Get Nearest Point on Line
#'
#' Computes the point on a multiline geometry that is closest to a target point.
#'
#' @param coordsLine Matrix. Coordinates of a single line as returned by `st_coordinates()` (with 2+ rows).
#' @param coordsPoint Numeric vector of length 2. Coordinates (X, Y) of the point to be snapped.
#'
#' @return A numeric vector of length 2 representing the snapped coordinates (X, Y).
#' @export
nearestPointOnLine <- function(coordsLine, coordsPoint) {
  nearest_points <- vapply(
    2:nrow(coordsLine),
    function(i) {
      nearestPointOnSegment(coordsLine[(i - 1):i, ], coordsPoint)
    },
    FUN.VALUE = c(0, 0, 0)
  )

  # Return the coordinates (X, Y) with the minimum distance
  nearest_points[1:2, which.min(nearest_points[3, ])]
}

#' Get Nearest Point on a Line Segment
#'
#' Computes the closest point on a 2-point segment to a given point.
#'
#' @param s Matrix (2 x 2). Coordinates of the segment endpoints (row 1 and 2: [X, Y]).
#' @param p Numeric vector of length 2. Coordinates of the point to be projected.
#'
#' @return A named numeric vector of length 3: `X`, `Y`, and `distance`.
#' @export
nearestPointOnSegment <- function(s, p) {
  ap <- c(p[1] - s[1, 1], p[2] - s[1, 2])
  ab <- c(s[2, 1] - s[1, 1], s[2, 2] - s[1, 2])

  t <- sum(ap * ab) / sum(ab * ab)
  t <- ifelse(t < 0, 0, ifelse(t > 1, 1, t))
  t <- ifelse(is.na(t), 0, t)

  x <- s[1, 1] + ab[1] * t
  y <- s[1, 2] + ab[2] * t
  distance <- sqrt((x - p[1])^2 + (y - p[2])^2)

  result <- c(x, y, distance)
  names(result) <- c("X", "Y", "distance")
  return(result)
}
