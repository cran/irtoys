#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern void ElSym(void *, void *, void *, void *);
extern void Update(void *, void *, void *, void *, void *, void *, void *, void *);
extern void ItTotal(void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"ElSym",      (DL_FUNC) &ElSym,       4},
  {"Update",     (DL_FUNC) &Update,      8},
  {"ItTotal",    (DL_FUNC) &ItTotal,     5},
  {NULL, NULL, 0}
};

void R_init_irtoys(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
