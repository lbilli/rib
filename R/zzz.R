.onLoad <- function(libname, pkgname) {

  load_data <- function(file)
                 utils::read.table(file= system.file("data-raw/", file, package=pkgname),
                                   header= TRUE,
                                   colClasses= "character")

  ns <- environment(sys.function())

  assign("signature_wrap", load_data("codes_signature.txt"), ns, inherits=FALSE)

  assign("signature_struct", load_data("codes_struct.txt"), ns, inherits=FALSE)

  assign("Validator", generate_validator(), ns, inherits=FALSE)
}
