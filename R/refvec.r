#' refvec class
#' 
#' Storage and methods for CPU vector data.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @rdname refvec-class
#' @name refvec-class
refvecR6 = R6::R6Class("refvec",
  public = list(
    #' @details
    #' Class initializer. See also \code{?vec}.
    #' @param size The length of the vector.
    #' @param type Storage type for the vector. Should be one of 'int' or 'double'.
    initialize = function(size=0, type="double")
    {
      type = match.arg(tolower(type), TYPES_STR)
      
      size = check_is_natnum(size)
      
      private$type_str = type
      private$type = type_str2int(type)
      
      private$x = numeric(size)
    },
    
    
    
    #' @details
    #' Change the length of the vector object.
    #' @param size The new length.
    resize = function(size)
    {
      size = check_is_natnum(size)
      
      private$x = numeric(size)
      invisible(self)
    },
    
    
    
    #' @details
    #' Set the data in the refvec object to point to the array in 'data'. See
    #' also \code{?as_refvec}.
    #' @param data R matrix.
    inherit = function(data)
    {
      if (!is.double(data))
        storage.mode(data) = "double"
      
      private$x = data
      invisible(self)
    },
    
    
    
    #' @details
    #' Duplicate the matrix in a deep copy.
    dupe = function()
    {
      self$clone(deep=TRUE)
    },
    
    
    
    #' @details
    #' Print one-line information about the vector.
    info = function()
    {
      cat("# refvec")
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
    #' Sum the vector.
    #' @return Returns the sum.
    sum = function()
    {
      sum(private$x)
    },
    
    
    
    #' @details
    #' Get element from the vector.
    #' @param i Index (0-based).
    get = function(i)
    {
      i = check_is_natnum(i)
      private$x[i+1]
    },
    
    
    
    #' @details
    #' Set element of the vector.
    #' @param i Index (0-based).
    #' @param v Value.
    set = function(i, v)
    {
      i = check_is_natnum(i)
      v = check_is_number(v)
      private$x[i+1, j+1] = v
      invisible(self)
    },
    
    
    
    #' @details
    #' Returns length of the vector.
    size = function() length(private$x),
    
    
    
    #' @details
    #' Returns the external pointer data. For developers only.
    data_ptr = function() private$x,
    
    #' @details
    #' Returns the integer code for the underlying storage type. For developers only.
    get_type = function() private$type,
    
    #' @details
    #' Returns the string code for the underlying storage type. For developers only.
    get_type_str = function() private$type_str,
    
    
    
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
    type = -1L,
    type_str = ""
  )
)



#' refvec
#' 
#' Constructor for refvec objects.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @param size Length of the vector.
#' @param type Storage type for the vector. Should be one of 'int' or 'double'.
#' @return A refvec class object.
#' 
#' @seealso \code{\link{vec-class}}
#' 
#' @export
refvec = function(size=0, type="double")
{
  refvecR6$new(size=size, type=type)
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
as_refvec = function(x)
{
  ret = refvec()
  ret$from_robj(x)
  ret
}
