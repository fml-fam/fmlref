#' refmat class
#' 
#' Storage and methods for CPU matrix data.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @rdname refmat-class
#' @name refmat-class
refmatR6 = R6::R6Class("refmat",
  public = list(
    #' @details
    #' Class initializer. See also \code{?mat}.
    #' @param nrows,ncols The dimension of the matrix.
    #' @param type Storage type for the matrix. Should be one of 'int' or 'double'.
    initialize = function(nrows=0, ncols=0, type="double")
    {
      type = match.arg(tolower(type), TYPES_STR)
      
      nrows = check_is_natnum(nrows)
      ncols = check_is_natnum(ncols)
      
      private$type_str = type
      private$type = type_str2int(type)
      
      private$x = numeric(nrows*ncols)
      dim(private$x) = c(nrows, ncols)
    },
    
    
    
    #' @details
    #' Change the dimension of the matrix object.
    #' @param nrows,ncols The new dimension.
    resize = function(nrows, ncols)
    {
      nrows = check_is_natnum(nrows)
      ncols = check_is_natnum(ncols)
      
      private$x = numeric(nrows*ncols)
      dim(private$x) = c(nrows, ncols)
      invisible(self)
    },
    
    
    
    #' @details
    #' Set the data in the refmat object to point to the array in 'data'. See
    #' also \code{?as_refmat}.
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
    #' Print one-line information about the matrix.
    info = function()
    {
      cat("# refmat")
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
    #' Set diagonal entries of the matrix to those in the vector. If the vector
    #' is smaller than the matrix diagonal, the vector will recycle until the
    #' matrix diagonal is filled.
    #' @param v A refvec object.
    fill_diag = function(v)
    {
      if (!is_refvec(v))
        v = as_refvec(v)
      
      check_type_consistency(self, v)
      
      private$x = diag(v$data_ptr())
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
      ret = refvec()
      ret$set(diag(private$x))
      ret
    },
    
    
    
    #' @details
    #' Fill diagonal values to 1 and non-diagonal values to 0.
    antidiag = function()
    {
      m = nrow(private$x)
      n = ncol(private$x)
      minmn = min(m, n)
      v = numeric(minmn)
      for (i in 1:minmn)
        v[i] = private$x[m-i+1, i]
      
      ret = refvec()
      ret$inherit(v)
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
    #' Get element from the matrix.
    #' @param i,j Indices (0-based).
    get = function(i, j)
    {
      i = check_is_natnum(i)
      j = check_is_natnum(j)
      
      private$x[i+1, j+1]
    },
    
    
    
    #' @details
    #' Set element of the matrix.
    #' @param i,j Indices (0-based).
    #' @param v Value.
    set = function(i, j, v)
    {
      i = check_is_natnum(i)
      j = check_is_natnum(j)
      v = check_is_number(v)
      
      private$x[i+1, j+1] = v
      invisible(self)
    },
    
    
    
    #' @details
    #' Get the specified row.
    #' @param i Index (0-based).
    #' @param v A refvec object.
    get_row = function(i, v)
    {
      if (!is_refvec(v))
        stop("'v' must be a refvec object")
      
      check_type_consistency(self, v)
      
      i = check_is_natnum(i)
      v$inherit(private$x[i+1, ])
      invisible(self)
    },
    
    
    
    #' @details
    #' Get the specified column.
    #' @param j Index (0-based).
    #' @param v A refvec object.
    get_col = function(j, v)
    {
      if (!is_refvec(v))
        stop("'v' must be a refvec object")
      
      check_type_consistency(self, v)
      
      j = check_is_natnum(j)
      
      v$inherit(private$x[, j+1])
      invisible(self)
    },
    
    
    
    #' @details
    #' Returns number of rows and columns of the matrix.
    dim = function() dim(private$x),
    
    #' @details
    #' Returns number of rows of the matrix.
    nrows = function() nrow(private$x),
    
    #' @details
    #' Returns number of columns of the matrix.
    ncols = function() ncol(private$x),
    
    
    
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
    type = -1L,
    type_str = ""
  )
)



#' refmat
#' 
#' Constructor for refmat objects.
#' 
#' @details
#' Data is held in an external pointer.
#' 
#' @param nrows,ncols The dimensions of the matrix.
#' @param type Storage type for the matrix. Should be one of 'int' or 'double'.
#' @return A refmat class object.
#' 
#' @seealso \code{\link{mat-class}}
#' 
#' @export
refmat = function(nrows=0, ncols=0, type="double")
{
  refmatR6$new(nrows=nrows, ncols=ncols, type=type)
}



#' as_refmat
#' 
#' Convert an R matrix to a refmat object.
#' 
#' @param x R matrix.
#' @param copy Should the R data be copied? If \code{FALSE}, be careful!
#' @return A refmat object.
#' 
#' @export
as_refmat = function(x)
{
  ret = refmat()
  ret$from_robj(x)
  ret
}
