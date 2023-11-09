#include "data.table.h"
#include <Rdefines.h>
#include <Rmath.h>

/*
Copied from src/main/summary.c with the following changes :
i) Rather than mean.default doing x <- x[!is.na(x)] which incurs
several vector allocations and scans, we just skip NA in the for
loop in C.
ii) Overhead of repeated calls to S3 dispatch of mean to mean.default
is avoided by calling C directly. See wiki point 3 for the large
difference this makes.
Ordinarily we prefer not to duplicate base R stats functions in case
we introduce a bug or create additonal maintenance burden. But,
mean() is so slow, and so much slower than sum(), and so commonly used
and benchmarked, that it warrants this fast mean.
We can't call .Internal(mean(x)) because that's disallowed by QC.R, but
anyway .Internal(mean(x)) doesn't respect na.rm=TRUE.
We are careful to retain the double scan that summary.c does that
adjusts for accumalated rounding errors in floating point.

We explicitly test that fastmean returns the same result as
base::mean in test.data.table(). For that we use exact equality under
floating point, not all.equal. A stronger test. These
tests run under R CMD check and run on all CRAN platforms daily to catch
if we become out of line to base R (say if base R changed its mean).
*/

SEXP fastmean(SEXP args)
{
  long double s = 0., t = 0., si = 0.;
  R_len_t l = 0, n = 0;
  SEXP x, ans, tmp;
  Rboolean narm=FALSE;
  x=CADR(args);
  if (length(args)>2) {
    tmp = CADDR(args);
    if (!isLogical(tmp) || LENGTH(tmp)!=1 || LOGICAL(tmp)[0]==NA_LOGICAL)
      error(_("na.rm should be TRUE or FALSE"));  // # nocov ; [.data.table should construct the .External call correctly
    narm=LOGICAL(tmp)[0];
  }
  switch(TYPEOF(x)) {
    case LGLSXP: case INTSXP: case REALSXP: {
      PROTECT(ans = allocNAVector(REALSXP, 1));
    } break;
    case CPLXSXP: {
      PROTECT(ans = allocNAVector(CPLXSXP, 1));
    } break;
    default: 
      error(_("fastmean was passed type %s, not numeric, logical or complex"), type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  l = LENGTH(x);
  if (narm) {
    switch(TYPEOF(x)) {
    case LGLSXP: case INTSXP: {
      const int *restrict xd = INTEGER(x);
      for (int i=0; i<l; ++i) {
        if(xd[i] == NA_INTEGER) continue;
        s += xd[i];   // no under/overflow here, s is long double not integer
        n++;
      }
      if (n>0)
        REAL(ans)[0] = (double) (s/n);
      else
        REAL(ans)[0] = R_NaN;  // consistent with base: mean(NA,na.rm=TRUE)==NaN==mean(numeric(),na.rm=TRUE)
    } break;
    case REALSXP: {
      const double *restrict xd = REAL(x);
      for (int i=0; i<l; ++i) {
        if(ISNAN(xd[i])) continue;  // TO DO: could drop this line and let NA propogate?
        s += xd[i];
        n++;
      }
      if (n==0) {
        REAL(ans)[0] = R_NaN;
        break;
      }
      s /= n;
      if(R_FINITE((double)s)) {
        for (int i=0; i<l; ++i) {
          if(ISNAN(xd[i])) continue;
          t += (xd[i] - s);
        }
        s += t/n;
      }
      REAL(ans)[0] = (double) s;
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict xd = COMPLEX(x);
      for (int i=0; i<l; ++i) {
        if (ISNAN(xd[i].r) || ISNAN(xd[i].i)) continue;
        s += xd[i].r;
        si += xd[i].i;
        n++;
      }
      if (n==0) {
        COMPLEX(ans)[0].r = R_NaN;
        COMPLEX(ans)[0].i = R_NaN;
        break;
      }
      s /= n;
      si /= n;
      COMPLEX(ans)[0].r = (double) s;
      COMPLEX(ans)[0].i = (double) si;
    } break;
    default:
      error(_("Internal error: type '%s' not caught earlier in fastmean"), type2char(TYPEOF(x)));  // # nocov
    }
  } else {  // narm==FALSE
    switch(TYPEOF(x)) {
    case LGLSXP: case INTSXP: {
      const int *restrict xd = INTEGER(x);
      for (int i=0; i<l; ++i) {
        if(xd[i] == NA_INTEGER) {UNPROTECT(1); return(ans);}
        s += xd[i];
      }
      REAL(ans)[0] = (double) (s/l);
    } break;
    case REALSXP: {
      const double *restrict xd = REAL(x);
      for (int i=0; i<l; ++i) {
        if(ISNAN(xd[i])) {UNPROTECT(1); return(ans);}
        s += xd[i];
      }
      s /= l;
      if(R_FINITE((double)s)) {
        for (int i=0; i<l; ++i) {
          // no NA if got this far
          t += (xd[i] - s);
        }
        s += t/LENGTH(x);
      }
      REAL(ans)[0] = (double) s;
    } break;
    case CPLXSXP: {
      const Rcomplex *restrict xd = COMPLEX(x);
      for (int i=0; i<l; ++i) {
        if(ISNAN(xd[i].r) || ISNAN(xd[i].i)) {UNPROTECT(1); return(ans);}
        s += xd[i].r;
        si += xd[i].i;
      }
      s /= l;
      si /= l;
      COMPLEX(ans)[0].r = (double) s;
      COMPLEX(ans)[0].i = (double) si;
    } break;
    default:
      error(_("Internal error: type '%s' not caught earlier in fastmean"), type2char(TYPEOF(x)));  // # nocov
    }
  }
  UNPROTECT(1);
  return(ans);
}

/*
    case CPLXSXP:
      PROTECT(ans = allocVector(CPLXSXP, 1));
      for (int i=0; i<n; ++i) {
      s += COMPLEX(x)[i].r;
      si += COMPLEX(x)[i].i;
      }
      s /= n; si /= n;
      if( R_FINITE((double)s) && R_FINITE((double)si) ) {
      for (int i=0; i<n; ++i) {
        t += COMPLEX(x)[i].r - s;
        ti += COMPLEX(x)[i].i - si;
      }
      s += t/n; si += ti/n;
      }
      COMPLEX(ans)[0].r = (double) s;
      COMPLEX(ans)[0].i = (double) si;
      break;
*/

