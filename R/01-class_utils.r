is_mat = function(x) inherits(x, "mat")
is_vec = function(x) inherits(x, "vec")



check_class_consistency = function(...)
{
  l = list(...)
  funs = c(is_mat, is_vec)
  
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
