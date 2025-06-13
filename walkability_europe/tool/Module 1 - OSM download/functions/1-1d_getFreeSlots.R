#' Get the Number of Available Slots from an API
#'
#' Queries an API endpoint (typically a GPT or model-serving instance) to retrieve 
#' the number of currently available slots from its status page.
#'
#' @param APIlink Character. The base API link (e.g., a URL pointing to a model interpreter).
#'
#' @return Integer. The number of available slots. Returns 0 if no slots are available 
#' or if the API call fails.
#' @export
#'
#' @examples
#' \dontrun{
#' getFreeSlots("http://localhost:11434/interpreter")
#' }

get_free_slots <- function(APIlink) {
  # Initialize default value
  n_slots <- 0

  try({
    suppressWarnings({
      n_slots <- APIlink %>%
        sub("interpreter", "status", .) %>%
        httr::GET() %>%
        httr::content() %>%
        strsplit("\n") %>%
        unlist() %>%
        grep("available", ., value = TRUE) %>%
        substr(1, 1) %>%
        as.numeric() %>%
        .[1]
    })
  }, silent = TRUE)

  if (!is.null(n_slots) && !is.na(n_slots)) {
    return(n_slots)
  } else {
    return(0)
  }
}
