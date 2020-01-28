# crossprod
test = linalg_crossprod(x=x)$to_robj()
truth = crossprod(xr)
cmp(test[lower.tri(test, diag=TRUE)], truth[lower.tri(truth, diag=TRUE)])

test = linalg_tcrossprod(x=x)$to_robj()
truth = tcrossprod(xr)
cmp(test[lower.tri(test, diag=TRUE)], truth[lower.tri(truth, diag=TRUE)])
