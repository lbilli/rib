\name{enums}
\title{Enumerated Types}
\alias{map_enum2int}
\alias{map_int2enum}

\description{
Enumerated types are used in few places across the API.
These are types that can have only a limited set of named
constant values.

These functions facilitate the conversion between integer value
and string representation.
}

\usage{
map_enum2int(enum, name)

map_int2enum(enum, value)
}

\arguments{
  \item{enum}{name of the enumeration type: \emph{e.g.} \code{"Condition"},
              \code{"FaDataType"}, \code{"MarketData"}, \code{"PriceTrigger"}.}

  \item{name}{string representation of \code{value}.}

  \item{value}{integer representation of \code{name}.}
}

\value{
\code{map_enum2int} returns the corresponding \code{value}.

\code{map_int2enum} returns the corresponding \code{name}.
}

\examples{
map_enum2int("MarketData", "DELAYED")   # -> 3

map_int2enum("MarketData", 3)           # -> "DELAYED"
}
