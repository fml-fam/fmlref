check_class_consistency = function(...)
{
  l = list(...)
  funs = c(is_refmat, is_refvec)
  
  for (fun in funs)
  {
    test = sapply(l, fun)
    if (any(fun))
    {
      if (all(fun))
        return(invisible(TRUE))
      else
        stop("inconsistent object usage")
    }
  }
}



check_type_consistency = function(...)
{
  l = list(...)
  if (length(l) < 2)
    return(invisible(TRUE))
  
  gt = function(x) x$get_type()
  test = sapply(l, gt)
  
  if (sum(diff(test)) == 0)
    return(invisible(TRUE))
  else
    stop("inconsistent type usage: can not mix fundamental types")
}



# ------------------------------------------------------------------------------
# reactor
# ------------------------------------------------------------------------------

is.badval = function(x)
{
  is.na(x) || is.nan(x) || is.infinite(x)
}

is.inty = function(x)
{
  abs(x - round(x)) < 1e-10
}

is.zero = function(x)
{
  abs(x) < 1e-10
}

is.negative = function(x)
{
  x < 0
}

is.annoying = function(x)
{
  length(x) != 1 || is.badval(x)
}



check_is_integer = function(x)
{
  x = as.integer(x)
  if (is.annoying(x) || !is.inty(x))
  {
    nm = deparse(substitute(x))
    stop(paste0("argument '", nm, "' must be a natural number (0 or positive integer)"), call.=FALSE)
  }
  
  x
}

check_is_natnum = function(x)
{
  x = as.integer(x)
  if (is.annoying(x) || !is.inty(x) || is.negative(x))
  {
    nm = deparse(substitute(x))
    stop(paste0("argument '", nm, "' must be a natural number (0 or positive integer)"), call.=FALSE)
  }
  
  x
}

check_is_number = function(x)
{
  x = as.double(x)
  if (is.annoying(x))
  {
    nm = deparse(substitute(x))
    stop(paste0("argument '", nm, "' must be a natural number (0 or positive integer)"), call.=FALSE)
  }
  
  x
}
