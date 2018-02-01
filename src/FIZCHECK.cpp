#include "sindex.h"
#include <Rcpp.h>
using namespace Rcpp;

/*
 * fizcheck.c
 * - determines whether a given FIZ code represents the coast or interior.
 *
 * 1994 oct 19 - Moved here from FredTab.
 * 1999 jan 8  - Changed int to short int.
 */



// [[Rcpp::export]]
short int fiz_check (char fiz)
{
  switch (fiz)
  {
  case 'A':
  case 'B':
  case 'C':
    return FIZ_COAST;
  case 'D':
  case 'E':
  case 'F':
  case 'G':
  case 'H':
  case 'I':
  case 'J':
  case 'K':
  case 'L':
    return FIZ_INTERIOR;
  default:
    return FIZ_UNKNOWN;
  }
}
