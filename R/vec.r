#' vec class
#' 
#' Storage and methods for CPU vector data.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @rdname vec-class
#' @name vec-class
vecR6 = R6::R6Class("vec",
  public = list(
    #' @details
    #' Class initializer. See also \code{?vec}.
    #' @param size The length of the vector.
    #' @param type Storage type for the vector. Should be one of 'int', 'float', or 'double'.
    initialize = function(size=0, type="double")
    {
      private$x = numeric(size)
      private$type = type
    },
    
    #' @details
    #' Change the length of the vector object.
    #' @param size The new length.
    resize = function(size)
    {
      private$x # TODO
      invisible(self)
    },
    
    #' @details
    #' Set the data in the vec object to point to the array in 'data'. See
    #' also \code{?as_vec}.
    #' @param data R vector.
    set = function(data)
    {
      private$x = data
      invisible(self)
    },
    
    #' @details
    #' Print one-line information about the vector.
    info = function()
    {
      cat("# vec")
      cat(sprintf(" %d", length(private$x)))
      cat(" type=d")
      cat("\n")
      invisible(self)
    },
    
    #' @details
    #' Print the data.
    #' @param ndigits Number of decimal digits to print.
    print = function(ndigits=4)
    {
      print(private$x, digits=ndigits)
      invisible(self)
    },
    
    #' @details
    #' Fill all entries with zero.
    fill_zero = function()
    {
      private$x[] = 0
      invisible(self)
    },
    
    #' @details
    #' Fill all entries with one.
    fill_one = function()
    {
      private$x[] = 1
      invisible(self)
    },
    
    #' @details
    #' Fill all entries with supplied value.
    #' @param v Value to set all entries to.
    fill_val = function(v)
    {
      private$x[] = v
      invisible(self)
    },
    
    #' @details
    #' Fill the vector (column-wise) with linearly-spaced values.
    #' @param start,stop Beginning/end of the linear spacing.
    fill_linspace = function(start, stop)
    {
      private$x[] = seq(from=start, to=stop, length.out=length(private$x))
      invisible(self)
    },
    
    #' @details
    #' Scale all entries by the supplied value.
    #' @param s Value to scale all entries by.
    scale = function(s)
    {
      private$x = private$x * s
      invisible(self)
    },
    
    #' @details
    #' Reverse rows.
    rev = function()
    {
      private$x = rev(private$x)
      invisible(self)
    },
    
    #' @details
    #' Returns length of the vector.
    size = function() length(private$x),
    
    #' @details
    #' Returns the external pointer data. For developers only.
    data_ptr = function() private$x,
    
    #' @details
    #' Returns an R vector containing a copy of the class data.
    to_robj = function() private$x,
    
    #' @details
    #' Copies the values of the input to the class data. See also \code{?as_vec}.
    #' @param robj R vector.
    from_robj = function(robj)
    {
      vec_from_robj(private$x, robj)
      invisible(self)
    }
  ),
  
  private = list(
    x = NULL,
    type = ""
  )
)



#' vec
#' 
#' Constructor for vec objects.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @param size Length of the vector.
#' @param type Storage type for the vector. Should be one of 'int', 'float', or 'double'.
#' @return A vec class object.
#' 
#' @seealso \code{\link{vec-class}}
#' 
#' @export
vec = function(size=0, type="double")
{
  vecR6$new(size=size, type=type)
}



#' as_vec
#' 
#' Convert an R vector to a cpumat object.
#' 
#' @param x R vector.
#' @param copy Should the R data be copied? If \code{FALSE}, be careful!
#' @return A vec object.
#' 
#' @export
as_vec = function(x, copy=TRUE)
{
  ret = vec()
  
  if (isTRUE(copy))
    ret$from_robj(x)
  else
    ret$set(x)
  
  ret
}
