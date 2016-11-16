.libPaths("lib")

library(fork)
library(metrumrg)

NONR(1,command="/opt/NONMEM/nm73/nmqual/autolog.pl",
     project="../NMStorage/examples",wait=F,grid=T,diag=T)
NONR(2,command="/opt/NONMEM/nm72/nmqual/autolog.pl",
     project="../NMStorage/examples",wait=F,grid=T,diag=T)
NONR(500,command="/opt/NONMEM/nm73/nmqual/autolog.pl",
     project="../NMStorage/mi210",wait=F,grid=T,diag=T)
NONR(510,command="/opt/NONMEM/nm73/nmqual/autolog.pl",
     project="../NMStorage/mi210",wait=F,grid=F,diag=T)
NONR(511,command="/opt/NONMEM/nm73/nmqual/autolog.pl",
     project="../NMStorage/mi210",wait=F,grid=F,diag=F)
