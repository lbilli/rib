.onLoad <- function(libname, pkgname) {

  # ProtoBuf
  RProtoBuf::readProtoFiles2(protoPath=system.file("proto", package=pkgname, lib.loc=libname))

  options("RProtoBuf.int64AsString" = TRUE)
}
