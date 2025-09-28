splat <- function(pb, ...)
{
  stopifnot(inherits(pb, "Message"))

  init <- list(...)

#  if(length(pb) < pb$descriptor()$field_count())
#    warning("default needed: ",
#            paste(Filter(function(n) !pb$has(n), names(pb)), collapse=" "))

  sapply(names(pb), function(n) {

                      if(pb$has(n)) {

                        val <- pb[[n]]

                        if(inherits(val, "Message")) mapfrompb(val)
                        else if(is.list(val)) lapply(val, mapfrompb)
                        else val
                      }
                      else if(utils::hasName(init, n))
                        init[[n]]

                      else {
                        warning("default not provided: ", n)
                        NA
                      }
                    },
                  simplify=FALSE)
}


mapfrompb <- function(pb)
{
  stopifnot(inherits(pb, "Message"))

  desc <- pb$descriptor()

  res <- if(length(pb) == desc$field_count()) list()
         else get0(desc$name(), parent.env(environment()), mode="list", inherits=FALSE)

  if(is.null(res)) {
    warning(desc$name(), " not found. Using as.list()")
    return(as.list(pb))
  }

  for(n in names(pb)) {

    if(!pb$has(n))
      next

    val <- pb[[n]]

    if(inherits(val, "Message"))
      val <- mapfrompb(val)

    else if(is.list(val))
      val <- if(desc[[n]]$message_type()$name() == "TagValue")
               tagvaluefrompb(val)

             else if(desc[[n]]$message_type()$name() == "OrderCondition")
               lapply(val, conditionfrompb)

             else
               lapply(val, mapfrompb)

    res[[n]] <- val
  }

  res
}


maptopb <- function(x, desc, exclude=character())
{
  pb <- RProtoBuf::new(desc)

  for(n in names(x)) {

    if(n %in% exclude)
      next

    val <- x[[n]]

    if(length(val) == 0L)
      next

    fld <- desc[[n]]

    if(is.list(val)) {
      if(is.null(names(val))) {
        stopifnot(fld$is_repeated(),
                  fld$message_type()$name() %in% c("OrderCondition", "ComboLeg"))

        val <- if(fld$message_type()$name() == "OrderCondition")
                 lapply(val, conditiontopb)
               else
                 lapply(val, maptopb, desc=fld$message_type())
      }
      else {
        stopifnot(!fld$is_repeated(),
                  fld$message_type()$name() %in% c("Contract", "Order",
                                                   "SoftDollarTier", "DeltaNeutralContract",
                                                   "ExecutionFilter"))

        val <- maptopb(val, fld$message_type())

        if(length(val) == 0L)
          next
      }
    }
    else if(fld$is_repeated()) {

      stopifnot(!is.na(val))

      if(fld$type() == RProtoBuf::TYPE_MESSAGE) {

        stopifnot(fld$message_type()$name() == "TagValue")

        val <- tagvaluetopb(val)
      }

      # Repeated scalar are unchanged
    }
    else if(is.na(val) ||         # Optional scalar types
            is.character(val) && !nzchar(val, keepNA=TRUE) ||
            isFALSE(val))
      next

    pb[[n]] <- val
  }

  pb
}


tagvaluetopb <- function(tv)
{
  stopifnot(is.character(tv),
            !is.null(names(tv)))

  mapply(function(k, v) RProtoBuf::new(IBProto.TagValue, key=k, value=v), names(tv), tv)
}


tagvaluefrompb <- function(pb)
{
  n <- sapply(pb, function(kv) kv$key)
  v <- sapply(pb, function(kv) kv$value)

  structure(v, names=n)
}


conditiontopb <- function(cond)
{
  cond$type <- map_enum2int("Condition", cond$type)
  cond$isConjunction <- cond$conjunction == "a"
  cond$conjunction <- NULL

  maptopb(cond, IBProto.OrderCondition)
}


conditionfrompb <- function(pb)
{
  stopifnot(pb$descriptor()$name() == "OrderCondition",
            pb$has("type"),
            pb$has("isConjunction"))

  cond <- fCondition(map_int2enum("Condition", pb$type))
  if(pb$isConjunction)
    cond$conjunction <- "a"

  for(n in names(pb)) {

    if(n %in% c("type", "isConjunction") || !pb$has(n))
      next

    if(utils::hasName(cond, n))
      cond[[n]] <- pb[[n]]

    else
      warning("unknown name: ", n)
  }

  cond
}
