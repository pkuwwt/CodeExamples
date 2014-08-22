/**
   @file      gslRand.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2006 by Mitch Richling.
   @Dist      Released under the GPL
   @brief     Example of GSL random number functions@EOL
   @Keywords  random gsl
   @Std       C89

              Quick example showing how to use the GSL random
              number generators.
*/

#include<gsl/gsl_rng.h>
#include<stdio.h>

int main() {

  /* A pointer to a gls random number generator object */
  gsl_rng *r;

  /* Allocate a random number generator object.  Note that a large
     number of different generators are available.  See the docs. */
  if((r = gsl_rng_alloc(gsl_rng_mt19937)) == NULL) {
    printf("ERROR: Could not create random number generator\n");
    exit(1);
  }

  /* Set the seed for our generator to 1234.  If set to 0, or don't
     even call this function, then we get the default initial seed for
     the specific generator in question.*/
  gsl_rng_set(r, 1234);

  /* Get a random double from U[0,1) */
  double ranD;
  ranD = gsl_rng_uniform(r);
  printf("A random double from U[0,1): %f\n", ranD);

  /* Get a random double from U(0,1) */
  double ranDP;
  ranDP = gsl_rng_uniform_pos(r);
  printf("A random double from U(0,1): %f\n", ranDP);

  /* Get a random integer from a restricted range */
  long int ranLR;
  long int ranLRmax = 123;
  ranLR = gsl_rng_uniform_int(r, ranLRmax);
  printf("A random long from U[0,%ld-1): %ld\n", ranLRmax, ranLR);

  /* Get a random integer from the underlying generator.  This is not
     generally useful for programs wishing to remain independent of
     the underlying generator. */
  long int ranLN;
  unsigned long int ranLNmax, ranLNmin;
  const char *ranLNnam;
  ranLN = gsl_rng_get(r);
  ranLNmax = gsl_rng_max(r);
  ranLNmin = gsl_rng_min(r);
  ranLNnam = gsl_rng_name(r);
  printf("A random long in [%lu, %lu] from native generator(%s): %ld\n", 
         ranLNmin, ranLNmax, ranLNnam, ranLN);

  /* Free up our random number */
  gsl_rng_free(r);

  return 0;
}
