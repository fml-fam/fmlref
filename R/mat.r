#' mat class
#' 
#' Storage and methods for CPU matrix data.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @rdname mat-class
#' @name mat-class
matR6 = R6::R6Class("mat",
  public = list(
    #' @details
    #' Class initializer. See also \code{?mat}.
    #' @param nrows,ncols The dimension of the matrix.
    #' @param type Storage type for the matrix. Should be one of 'int', 'float', or 'double'.
    initialize = function(nrows=0, ncols=0, type="double")
    {
      private$x = numeric(nrows*ncols)
      dim(private$x) = c(nrows, ncols)
      private$type = type
    },
    
    #' @details
    #' Change the dimension of the matrix object.
    #' @param nrows,ncols The new dimension.
    resize = function(nrows, ncols)
    {
      private$x # TODO
      invisible(self)
    },
    
    #' @details
    #' Set the data in the mat object to point to the array in 'data'. See
    #' also \code{?as_mat}.
    #' @param data R matrix.
    set = function(data)
    {
      private$x = data
      invisible(self)
    },
    
    #' @details
    #' Print one-line information about the matrix.
    info = function()
    {
      cat("# mat")
      cat(sprintf(" %dx%d", nrow(private$x), ncol(private$x)))
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
    #' Fill the matrix (column-wise) with linearly-spaced values.
    #' @param start,stop Beginning/end of the linear spacing.
    fill_linspace = function(start, stop)
    {
      private$x[] = seq(from=start, to=stop, length.out=nrow(private$x)*ncol(private$x))
      invisible(self)
    },
    
    #' @details
    #' Fill diagonal values to 1 and non-diagonal values to 0.
    fill_eye = function()
    {
      private$x = diag(1, nrow(private$x), ncol(private$x))
      invisible(self)
    },
    
    #' @details
    #' Fill the matrix with random unifmorm data.
    #' @param seed Seed for the generator. Can be left blank.
    #' @param min,max Parameters for the generator.
    fill_runif = function(seed, min=0, max=1)
    {
      if (!missing(seed))
        set.seed(seed)
      
      private$x[] = runif(min=min, max=max)
      invisible(self)
    },
    
    #' @details
    #' Fill the matrix with random normal data.
    #' @param seed Seed for the generator. Can be left blank.
    #' @param mean,sd Parameters for the generator.
    fill_rnorm = function(seed, mean=0, sd=1)
    {
      if (!missing(seed))
        set.seed(seed)
      
      private$x[] = rnorm(mean=mean, sd=sd)
      invisible(self)
    },
    
    #' @details
    #' Fill diagonal values to 1 and non-diagonal values to 0.
    diag = function()
    {
      ret = vec()
      ret$set(diag(private$x))
      ret
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
    rev_rows = function()
    {
      private$x = private$x[nrow(private$x):1, ]
      invisible(self)
    },
    
    #' @details
    #' Reverse columns.
    rev_cols = function()
    {
      private$x = private$x[, ncol(private$x):1]
      invisible(self)
    },
    
    #' @details
    #' Returns number of rows of the matrix.
    nrows = function() nrow(private$x),
    
    #' @details
    #' Returns number of columns of the matrix.
    ncols = function() ncol(private$x),
    
    #' @details
    #' Returns number of rows and columns of the matrix.
    dim = function() dim(private$x),
    
    #' @details
    #' Returns the external pointer data. For developers only.
    data_ptr = function() private$x,
    
    #' @details
    #' Returns an R vector containing a copy of the class data.
    to_robj = function() private$x,
    
    #' @details
    #' Copies the values of the input to the class data. See also \code{?as_cpumat}.
    #' @param robj R matrix.
    from_robj = function(robj)
    {
      private$x = robj
      invisible(self)
    }
  ),
  
  private = list(
    x = NULL,
    type = ""
  )
)



#' mat
#' 
#' Constructor for mat objects.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @param nrows,ncols The dimensions of the matrix.
#' @param type Storage type for the matrix. Should be one of 'int', 'float', or 'double'.
#' @return A mat class object.
#' 
#' @seealso \code{\link{mat-class}}
#' 
#' @export
mat = function(nrows=0, ncols=0, type="double")
{
  matR6$new(nrows=nrows, ncols=ncols, type=type)
}



#' as_mat
#' 
#' Convert an R matrix to a mat object.
#' 
#' @param x R matrix.
#' @param copy Should the R data be copied? If \code{FALSE}, be careful!
#' @return A mat object.
#' 
#' @export
as_mat = function(x, copy=TRUE)
{
  ret = mat()
  
  if (isTRUE(copy))
    ret$from_robj(x)
  else
    ret$set(x)
  
  ret
}
