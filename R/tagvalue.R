#
# TagValues are modeled as named character vectors:
# c(tag_1="value_1", ...)
#
# Can be packed in a single string as: "tag_1=value_1;tag_2=value_2;"
#
# or unfolded in a character vector: c("tag_1", "value_1", "tag_2", "value_2", ...)

pack_tagvalue <- function(tv, mode=c("string", "unfold"))
{
  mode <- match.arg(mode)

  stopifnot(is.character(tv))

  if(length(tv) == 0L)
    return(switch(mode,
                  string= "",
                  unfold= character()))

  tags <- names(tv)

  stopifnot(!is.null(tags),
            nzchar(tags, keepNA=TRUE))

  if(mode == "string")
    paste0(tags, "=", tv, ";", collapse="")

  else {
    res <- character(2L * length(tv))

    idx <- 2L * seq_along(tv)

    res[idx] <- unname(tv)
    res[idx - 1L] <- tags

    res
  }
}

#
# Convert c("tag_1", "value_1", "tag_2", "value_2", ...)
# to      c(tag_1="value_1", tag_2="value_2"...)
#
fold_tagvalue <- function(v)
{
  stopifnot(is.character(v))

  n <- length(v)

  stopifnot(n %% 2L == 0L)

  idx <- 2L * seq_len(n %/% 2L)

  structure(v[idx], names=v[idx - 1L])
}
