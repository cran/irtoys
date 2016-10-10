#ifndef interaction_H
#define interaction_H

void ElSym(double *b, int *n, double *g, int *j);
void Update(double *b, double *c, int *n, int *m, int *bsuf, int *csuf, double *converged, int *rasch);
void ItTotal(double *b, double *c, int *n, double* prob, int *i);
void EH(double *b, double *c, int *n, int *m, int *bsuf, int *csuf, double *E, double*H, int *NR);
void estimate_lambda(double *b, double *c, int *n, int *m, double *lambda);

#endif
