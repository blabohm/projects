#' Select an Available Overpass API Endpoint
#'
#' This function checks a list of Overpass API endpoints and selects the one
#' with the most available slots. It waits and retries if none are currently available.
#'
#' @param api_list A tibble containing Overpass API endpoint URLs in a column named `interpreter`.
#'   Defaults to a pre-defined list of public Overpass endpoints.
#'
#' @return A character string with the selected API URL.
#' @export
#'
#' @examples
#' \dontrun{
#' selected_api <- APIselect()
#' }

APIselect <- function(
  api_list = dplyr::tibble(
    interpreter = c(
      "http://overpass-api.de/api/interpreter",
      "https://lz4.overpass-api.de/api/interpreter",
      "https://z.overpass-api.de/api/interpreter"
    )
  )
) {
  # Load required package
  requireNamespace("dplyr", quietly = TRUE)

  # Evaluate available slots for each API
  api_list <- api_list %>%
    dplyr::mutate(slots = sapply(interpreter, getFreeSlots)) %>%
    dplyr::arrange(dplyr::desc(slots))

  # Retry until at least one API has a free slot
  while (sum(api_list$slots, na.rm = TRUE) < 1) {
    message("Waiting for API slot...")
    Sys.sleep(30)

    api_list <- api_list %>%
      dplyr::mutate(slots = sapply(interpreter, getFreeSlots)) %>%
      dplyr::arrange(dplyr::desc(slots))
  }

  return(api_list$interpreter[1])
}
