#' Compute Shortest Paths for Building Entries (Method 1)
#'
#' Adds a column `sPath` to `build_entries` with shortest paths from each
#' `gs_entries` geometry to associated building geometries within the network.
#'
#' @param build_entries `sf` object. Building entries with `ne_id` and `geom` columns.
#' @param gs_entries `sf` object. Nodes/points in the network with `geom` column.
#' @param sf_network `sfnetwork` object. Spatial network over which paths are calculated.
#'
#' @return `build_entries` with new list-column `sPath` containing edge paths.
#' @export
s_path1 <- function(build_entries, gs_entries, sf_network) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  build_entries %>%
    mutate(sPath = unlist(
      list(
        sapply(unique(ne_id), function(i) {
          pull(
            st_network_paths(
              x = sf_network,
              from = gs_entries$geom[i],
              to = .$geom[.$ne_id == i]
            ),
            edge_paths
          )
        })
      ), recursive = FALSE
    ))
}

#' Compute Shortest Paths for Building Entries (Method 2)
#'
#' Similar to `s_path1` but returns `sPath` as a flattened list-column directly.
#'
#' @inheritParams s_path1
#' @return `build_entries` with new list-column `sPath` containing edge paths.
#' @export
s_path2 <- function(build_entries, gs_entries, sf_network) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  build_entries %>%
    mutate(sPath = unlist(
      sapply(unique(ne_id), function(i) {
        pull(
          st_network_paths(
            x = sf_network,
            from = gs_entries$geom[i],
            to = .$geom[.$ne_id == i]
          ),
          edge_paths
        )
      }), recursive = FALSE
    ))
}

#' Compute Shortest Paths for Building Entries (Method 3)
#'
#' Similar to previous methods but returns `sPath` as a vector (not a list).
#'
#' @inheritParams s_path1
#' @return `build_entries` with new column `sPath` containing edge paths.
#' @export
s_path3 <- function(build_entries, gs_entries, sf_network) {
  require(dplyr, quietly = TRUE)
  require(sf, quietly = TRUE)
  require(sfnetworks, quietly = TRUE)

  build_entries %>%
    mutate(sPath = sapply(unique(ne_id), function(i) {
      pull(
        st_network_paths(
          x = sf_network,
          from = gs_entries$geom[i],
          to = .$geom[.$ne_id == i]
        ),
        edge_paths
      )
    }))
}
