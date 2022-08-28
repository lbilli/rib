#
# Attach an iterator to a vector
#
make_iter <- function(v)
{

  pos <- 0L

  list(pop= function(n=1L) {

              filter <- pos + seq_len(n)

              if(length(filter) > 0L) {

                stopifnot( (new_pos <- filter[n]) <= length(v))

                pos <<- new_pos
              }

              v[filter]
            },
       left= function() length(v) - pos
      )
}
