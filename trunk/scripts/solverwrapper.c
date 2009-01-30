#include "mex.h"

/*
  An example code showing how, given a Fortran optimization solver (in
  this case we use newuoa) and a matlab cost function, one can call
  the solver from within matlab.  We need a wrapper both for the
  Fortran solver, so that we can call it from matlab, and for the
  matlab cost function, so to call it from the fortran solver. The
  matlab caller and the matlab cost function can share additional data
  via global variables behind the back of the solver.
*/

/* We will use these pointers to share information between the two wrappers involved. */
mxArray *matlabX[1], *matlabCostFunVal[1];
char *costFunName;

/* Interface to the newuoa Fortran optimization solver */
void newuoa_(int* n, int* npt, double* X,
             double* rhobeg, double* rhoend,
             int* iprint, int* maxfun,
             double* W,
             void* costFun
             );

void costFunWrapper (int* pN, double *X, double* costFunVal){
  
  /*  A wrapper for the matlab cost function that */
  /*  will be called by the Fortran solver.       */
  
  int i;
  int N = *pN;
  
  /* Copy the data from the Fortran array X to the matlab vector matlabX */
  double *matlabXPtr = mxGetPr(matlabX[0]);
  for (i = 0; i < N; i++) matlabXPtr[i] = X[i];
    
  /* Call the matlab code whose name is kept in costFunName with input */
  /* matlabX and output matlabCostFunVal.                              */
  /* We assume the matlab cost function has the form y = costFun(x)    */
  int numInputs = 1, numOutputs = 1;
  mexCallMATLAB(numOutputs, matlabCostFunVal, numInputs, matlabX, costFunName);

  /* Copy the output of the matlab cost function to costFunVal */
  /* to be passed back to Fortran.                             */
  *costFunVal = *mxGetPr(matlabCostFunVal[0]);
  
  return;
}

void mexFunction( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] ){

  /* A wrapper for the newuoa Fortran optimization solver to be called from matlab.  
     We will call this function from matlab as follows:

     y = solverwrapper(2, [1.3 5.2], 100, 'costFunName');
     
     The arguments are: num of optimization variariables, init guess,
     num of itereations, matlab cost fun name. The output y holds the
     optimized variables (same size as the initial guess).
  */
  
  int i;

  /* First input argument, the number of variables */
  double *pN     = mxGetPr(prhs[0]);
  int      N     = (int) pN[0];

  /* Second input argument, the intial guess for optimization */
  double  *X     = mxGetPr(prhs[1]);
  
  /* Third input argument, the maximum number of iterations */
  double *pnIter = mxGetPr(prhs[2]);
  int     nIter  = (int) pnIter[0];

  /* Fourth argument, the name of the matlab cost function to call. */
  /* Will go globally to the cost function wrapper.                 */
  costFunName = mxArrayToString(prhs[3]);
  
  /* Create storage for the input variable to the matlab cost function. */
  /* This storage will be passed globally to costFunWrapper() above.    */
  matlabX[0] = mxCreateDoubleMatrix(1, N, mxREAL);
  mxSetM(matlabX[0], 1);
  mxSetN(matlabX[0], N);

  /* Prepare the newuoa optimization solver */
  int    Npt    = (N+1)*(N+2)/2; /* num of interp conditioins, in [N+2, (N+1)(N+2)/2] */ 
  double rhobeg = 1.0e-1;        /* init value of trust region radius (see newuoa.f)  */
  double rhoend = 1.0e-10;       /* accuracy in the final val of opt vars             */
  int    debug  = 3;             /* 0 <= debug <= 3; debug == 0 means keep quiet      */

  /* Workspace allocation for the solver*/
  mxArray *mxWorkspace;
  int workSize = (Npt+13)*(Npt+N)+3*N*(N+3)/2;
  mxWorkspace  = mxCreateDoubleMatrix( 1, workSize, mxREAL );
  double *W    = mxGetPr(mxWorkspace);
  
  /* Call the optimization solver */
  newuoa_(&N, &Npt,
          X, /* on input, has init guess, on output, has opt vals */
          &rhobeg, &rhoend,
          &debug,
          &nIter,
          W,
          (void*) costFunWrapper /* Cast from function pointer to void pointer */
          );
  
  /* Create storage where we will put the output from the optimization solver */
  plhs[0] = mxCreateDoubleMatrix(1, N, mxREAL);
  mxSetM(plhs[0], 1);
  mxSetN(plhs[0], N);

  /* Copy the optimal value of X returned from the newuoa solver to the output
     to be returned to matlab */
  double *Xout = mxGetPr(plhs[0]);
  for (i = 0; i < N; i++) Xout[i] = X[i];
  
  /* cleanup allocated memory */
  mxDestroyArray(matlabX[0]);
  mxDestroyArray(matlabCostFunVal[0]);
  mxDestroyArray(mxWorkspace);
  //mxDestroyArray(costFunName); // what to do about this
  //pointer?
    
  return;
}
