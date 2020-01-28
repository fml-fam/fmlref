linalg_crossprods = function(x, ret, alpha, xpose)
{
  if (!is.null(ret))
  {
    check_class_consistency(x, ret)
    invisiret = TRUE
  }
  else
    invisiret = FALSE
  
  if (isTRUE(xpose))
    cp = alpha * tcrossprod(x$data_ptr())
  else
    cp = alpha * crossprod(x$data_ptr())
  
  ret = refmat()
  ret$inherit(cp)
  
  if (invisiret)
    invisible(ret)
  else
    ret
}



#' crossprod
#' 
#' Compute crossproducts.
#' 
#' @param x Input data.
#' @param ret Either \code{NULL} or an already allocated fml matrix of the same
#' class and type as \code{x}.
#' @param alpha Number to scale the crossproduct by.
#' @return Returns the crossproduct.
#' 
#' @rdname linalg-crossprod
#' @name crossprod
NULL

#' @rdname linalg-crossprod
#' @export
linalg_crossprod = function(alpha=1, x, ret=NULL)
{
  linalg_crossprods(x=x, ret=ret, alpha=alpha, xpose=FALSE)
}

#' @rdname linalg-crossprod
#' @export
linalg_tcrossprod = function(alpha=1, x, ret=NULL)
{
  linalg_crossprods(x=x, ret=ret, alpha=alpha, xpose=TRUE)
}
