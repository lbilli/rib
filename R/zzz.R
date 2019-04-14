.onLoad <- function(libname, pkgname) {

  load_data <- function(file)
                 utils::read.table(file= system.file("data-raw/", file, package=pkgname),
                                   header= TRUE,
                                   colClasses= "character")

  load_map <- function(file) {

                codes <- load_data(file)

                function(k) codes[match(k, codes$name), , drop=TRUE]
              }

  ns <- environment(sys.function())

  assign("map_error", load_map("codes_error.txt"), ns, inherits=FALSE)

  assign("map_outbound", load_map("codes_outbound.txt"), ns, inherits=FALSE)

  assign("signature_wrap", load_data("codes_signature.txt"), ns, inherits=FALSE)

  assign("signature_struct", load_data("codes_struct.txt"), ns, inherits=FALSE)

  assign("Validator", generate_validator(), ns, inherits=FALSE)
}
