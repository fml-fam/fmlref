library(craze)
v = refvec()
x = refmat(5, 3)
x$fill_linspace(1, 2)
x

x$get_row(0, v)
v$info()
v

x$antidiag()
