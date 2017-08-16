#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool equal_matrices(NumericMatrix a, NumericMatrix b)
{
  if(a.nrow() != b.nrow() || a.ncol() != a.nrow())
  {
    return false;
  }
  for(int i = 0; i < a.nrow(); i++)
  {
    for(int j = 0; j < a.ncol(); j++)
    {
      if(a(i, j) != b(i, j))
      {
        return false;
      }
    }
  }
  return true;
}

// [[Rcpp::export]]
NumericMatrix another_leg(NumericMatrix a, NumericMatrix b)
{
  if(a.nrow() != b.nrow() || a.ncol() != b.ncol() || a.nrow() != a.ncol())
  {
    stop("Not square matrices");
  }
  int d = a.nrow();
  NumericMatrix out(d, d);

  for(int i = 0; i < d; i++)
  {
    for(int j = 0; j < d; j++)
    {
      double mindist = a(i, j); // -1 if we haven't found a route yet
      for(int k = 0; k < d; k++)
      {
        double elt_a = a(i, k);
        double elt_b = b(k, j);
        if(elt_a >= 0 && elt_b >= 0 && (mindist < 0 || elt_a + elt_b < mindist))
        {
          mindist = elt_a + elt_b;
        }
      }
      out(i, j) = mindist;
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericMatrix connectivity(NumericMatrix singleDistances, int max_iter = 100)
{
  int nrow = singleDistances.nrow(), ncol = singleDistances.ncol();
  if(nrow != ncol)
  {
    stop("'singleDistances' isn't a square matrix.");
  }
  NumericMatrix out(nrow, ncol), old(nrow, ncol);
  for(int i = 0; i < nrow; i++)
  {
    for(int j = 0; j < nrow; j++)
    {
      out(i, j) = singleDistances(i, j);
    }
  }
  int i = 0;
  do
  {
    i++;

    // copy old version over
    for(int k = 0; k < nrow*ncol; k++)
    {
      old[k] = out[k];
    }
    out = another_leg(out, singleDistances);

  } while(!equal_matrices(old, out) && i < max_iter);

  if(!equal_matrices(old, out))
  {
    Rcout << "Didn't converge. max_iter = " << max_iter << "." << std::endl;
  }
  return out;
}


/*** R
tmp <- matrix(c(-1,1,3,1,-1,1,3,1,-1), nrow = 3, ncol = 3)
connectivity(tmp)
tmp2 <- matrix(c(-1,1,-1,1,-1,-1,-1,-1,-1), nrow = 3, ncol = 3)
connectivity(tmp2)
*/


