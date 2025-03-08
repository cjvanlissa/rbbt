
#' Update bibliography file
#'
#' This function takes an RMarkdown file location and a bibliography file
#' as inputs and updates the bibliography file.
#'
#' @param path_rmd The path to the RMarkdown file(s).
#' @param path_bib Optionally, the path to the bibliography file.
#' The default value `NULL`uses `bbt_guess_bib_file()` to detect the path from
#' the 'YAML' front matter of `path_rmd`.
#' @param encoding Passed to [rmarkdown::yaml_front_matter()]
#' @param quiet Use `TRUE` to suppress message on successful write.
#' @inheritParams bbt_write_bib
#'
#' @return `path_bib`, invisibly
#' @export
bbt_update_bib <- function(path_rmd,
                           path_bib = NULL,
                           ignore = character(0),
                           translator = NULL,
                           library_id = getOption("rbbt.default.library_id", 1),
                           overwrite = TRUE, filter = identity, quiet = FALSE) {
  # Extract citations from all rmds
  keys_list <- lapply(path_rmd, bbt_detect_citations)
  # If no bib path given, guess from files. Otherwise, repeat given bib path
  if(is.null(path_bib)){
    path_bib <- sapply(path_rmd, bbt_guess_bib_file)
  } else {
    path_bib <- rep(path_bib, length(keys_list))
  }
  # Preallocate list and name it
  keys_per_bib <- vector("list", length = length(unique(path_bib)))
  names(keys_per_bib) <- unique(path_bib)
  # Get keys from all documents per unique bib file
  for(thisb in names(keys_per_bib)){
    keys_per_bib[[thisb]] <- unique(do.call(c, keys_list[path_bib == thisb]))
  }
  # If no translator given, guess from files. Otherwise, repeat given translator
  if(is.null(translator)){
    translator <- sapply(names(keys_per_bib), bbt_guess_translator)
  } else {
    translator <- rep(translator, length(keys_list))
  }
  # Write files and report status messages
  out_file <- sapply(seq_along(keys_per_bib), function(i){
    fl <- bbt_write_bib(
      path = names(keys_per_bib)[i],
      keys = keys_per_bib[[i]],
      ignore = ignore,
      translator = translator[i],
      overwrite = overwrite,
      filter = filter
    )
    if (!quiet) {
      message(sprintf("Wrote %d %s to '%s'", length(keys_per_bib[[1]]), c("reference", "references")[(length(keys_per_bib[[1]]) > 1)+1L], names(keys_per_bib)[1]))
    }
    fl
  })
  return(invisible(out_file))
}

#' @rdname bbt_update_bib
#' @export
bbt_guess_bib_file <- function(path_rmd, encoding = "UTF-8") {
  front_matter <- rmarkdown::yaml_front_matter(path_rmd, encoding = encoding)
  if (length(front_matter$bibliography) != 1) {
    stop(
      sprintf("Can't guess bibliography file from '%s' front matter", path_rmd),
      call. = FALSE
    )
  }

  bib_file <- front_matter$bibliography
  if (!fs::is_absolute_path(bib_file)) {
    file.path(dirname(path_rmd), bib_file)
  } else {
    bib_file
  }
}
