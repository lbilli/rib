splatpb <- function(pb)
{
  stopifnot(inherits(pb, "Message"),
            pb$descriptor()$field_count() == length(pb))

  lapply(pb, function(val) if(inherits(val, "Message")) mapfrompb(val)
                           else if(is.list(val)) lapply(val, mapfrompb)
                           else val)
}


mapfrompb <- function(pb)
{
  stopifnot(inherits(pb, "Message"))

  res <- get(pb$descriptor()$name(), mode="list")

  for(n in names(pb)) {

    if(!pb$has(n))
      next

    val <- pb[[n]]

    if(inherits(val, "Message"))
      val <- mapfrompb(val)

    else if(is.list(val))
      val <- lapply(val, mapfrompb)

    res[[n]] <- val
  }

  res
}


maptopb <- function(x, desc)
{
  stopifnot(inherits(desc, "Descriptor"))

  deft <- get(desc$name(), mode="list")

  stopifnot(names(deft) == names(x))

  pb = RProtoBuf::new(desc)

  for(n in names(x)) {

    val <- x[[n]]

    if(identical(val, deft[[n]]))
      next

    if(is.list(val))
      val <- if(is.null(names(val))) lapply(val, maptopb, desc[[n]]$message_type())
             else                    maptopb(val, desc[[n]]$message_type())

    else
      stopifnot(!is.na(val))

    pb[[n]] <- val
  }

  pb
}
