#' Write Distance Index Data to CSV
#'
#' Filters and writes node distance indices (di) to a CSV file,
#' excluding infinite values and rounding distances.
#'
#' @param out `data.frame` or `tibble`. Output data containing `ID` and `di` columns.
#' @param folder Character. Folder path where the CSV file will be saved.
#' @param ID Character or numeric. Identifier used to name the output file.
#'
#' @return Invisibly returns `NULL`. Side effect is writing a CSV file.
#' @export
write_di <- function(out, folder, ID) {
  node_out <- file.path(folder, paste0("nodes_", ID, ".csv"))

  out %>%
    select(ID, di) %>%
    filter(!is.infinite(di)) %>%
    mutate(di = round(di, 2)) %>%
    write.csv(node_out, row.names = FALSE)
}

#' Write Link Scores to CSV
#'
#' Processes and summarizes link score data for edges,
#' formats the path sequences, and writes to CSV.
#'
#' @param out `data.frame` or `tibble`. Data containing `sPath` (list-columns) and `ls` columns.
#' @param network `data.frame` or `tibble`. Network data containing edge information including `edge_id`.
#' @param folder Character. Folder path to save the CSV file.
#' @param ID Character or numeric. Identifier used to name the output file.
#'
#' @return Invisibly returns `NULL`. Side effect is writing a CSV file.
#' @export
write_ls <- function(out, network, folder, ID) {
  edge_out <- file.path(folder, paste0("edges_", ID, ".csv"))

  out %>%
    select(sPath, ls) %>%
    mutate(
      sPath = sapply(sPath, function(x) paste(unlist(x), collapse = " "))
    ) %>%
    tidyr::separate_rows(sPath, sep = " ") %>%
    group_by(sPath) %>%
    summarise(ls = round(sum(ls, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      sPath = as.numeric(sPath),
      edge_id = network$edge_id[sPath]
    ) %>%
    na.omit() %>%
    select(-sPath) %>%
    write.csv(edge_out, row.names = FALSE)
}
