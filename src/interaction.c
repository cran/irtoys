#include<math.h>
#include<R.h>
#include<Rmath.h>
#include<Rdefines.h>
#include<Rinternals.h>
#include "interaction.h"


void ElSym(double *b, int *n, double *g, int *j)
{
	int m=0, initial=0;
	for (int s=0;s<=n[0];s++)
	{
		g[s]=0;
	}
    if (j[0]!=0)
    {
	 	g[0]=1;
	 	g[1]=b[0];
	 	initial=0;
    }
    else
    {
		g[0]=1;
		g[1]=b[1];
		initial=1;
	}
	m=1;
	for (int i=0;i<n[0];i++)
	{
		if ((i!=j[0])&&(i!=initial))
		{
			m++;
			for (int s=m;s>0;s--)
			{
				g[s]=g[s]+g[s-1]*b[i];
			}
		}
	}
}

// m is the score distribution!
void Update(double *b, double *c, int *n, int *m, int *bsuf, int *csuf, double *converged, int *rasch)
{
       
        double *g=NULL, *bc=NULL, E,H,prob;
	int k, i0=-1;
	void *_g = realloc(g, ((n[0]) * sizeof(double)));
	g=(double*)_g;
	void *_bc = realloc(bc, (n[0]-1)*sizeof(double));
	bc=(double*)_bc;
    converged[0]=0;
	for (int i=0;i<n[0];i++)
	{
		E=m[n[0]];
		H=0;
		for (int s=1;s<n[0];s++)
		{
            k=0;
            for (int j=0;j<n[0];j++)
            {
                if (j!=i) {bc[k]=exp(b[j]+(s-1)*c[j]); k++;}
            }
            k=n[0]-1;
            ElSym(bc,&k,g,&i0);
            prob=exp(b[i]+(s-1)*c[i])*g[s-1]/(g[s]+exp(b[i]+(s-1)*c[i])*g[s-1]);
            E+=m[s]*prob;
            H-=m[s]*(prob-prob*prob);
		}
		b[i]=b[i]-(bsuf[i]-E)/H;
		if (fabs(bsuf[i]-E)>converged[0]) {converged[0]=fabs(bsuf[i]-E);}

		if(rasch[0]<1) {
		
		E=m[n[0]]*(n[0]-1);
		H=0;
		for (int s=1;s<n[0];s++)
		{

            k=0;
            for (int j=0;j<n[0];j++)
            {
                if (j!=i) {bc[k]=exp(b[j]+(s-1)*c[j]); k++;}
            }
            k=n[0]-1;
            ElSym(bc,&k,g,&i0);
            prob=exp(b[i]+(s-1)*c[i])*g[s-1]/(g[s]+exp(b[i]+(s-1)*c[i])*g[s-1]);
            E+=m[s]*(s-1)*prob;
            H-=m[s]*((s-1)*(s-1)*prob-((s-1)*prob)*((s-1)*prob));
		}
		c[i]=c[i]-(csuf[i]-E)/H;
		if (fabs(csuf[i]-E)>converged[0]) {converged[0]=fabs(csuf[i]-E);}
		}
        }
	free(g);
	free(bc);
}

void ItTotal(double *b, double *c, int *n, double *prob, int *i)
{
	double *g=NULL, *bc=NULL;
	int k, i0=-1;
	void *_g = realloc(g, ((n[0]) * sizeof(double)));
	g=(double*)_g;
	void *_bc = realloc(bc, (n[0]-1)*sizeof(double));
	bc=(double*)_bc;
	prob[0]=0;
	prob[n[0]]=1;
		for (int s=1;s<n[0];s++)
		{
            k=0;
            for (int j=0;j<n[0];j++)
            {
                if (j!=i[0]) {bc[k]=exp(b[j]+(s-1)*c[j]); k++;}
            }
            k=n[0]-1;
            ElSym(bc,&k,g,&i0);
            prob[s]=exp(b[i[0]]+(s-1)*c[i[0]])*g[s-1]/(g[s]+exp(b[i[0]]+(s-1)*c[i[0]])*g[s-1]);
		}

	free(g);
	free(bc);
}

void estimate_lambda(double *b, double *c, int *n, int *m, double *lambda)
{
	double *g=NULL, *g2=NULL, *bc=NULL;
	int i0=-1,M=0;
	void *_g = realloc(g, ((n[0]+1) * sizeof(double)));
	g=(double*)_g;
	void *_g2 = realloc(g2, ((n[0]+1) * sizeof(double)));
	g2=(double*)_g2;
	void *_bc = realloc(bc, (n[0])*sizeof(double));
	bc=(double*)_bc;
    g2[0]=1;
    M=m[0];
    for (int s=1;s<=n[0];s++)
    {
        for (int i=0;i<n[0];i++)
        {
            bc[i]=exp(b[i]+(s-1)*c[i]);
        }
        ElSym(bc,n,g,&i0);
        g2[s]=g[s];
        M+=m[s];
    }
    for (int s=0;s<=n[0];s++)
    {
        if (m[s]>0) {lambda[s]=log(m[s]/(M*g2[s]));}
    }
    lambda[0]=0;

    free(bc);
    free(g);
    free(g2);
}

// m is the score distribution! NR=1 means Newton-Raphson, any other value means only the diagonal of H is computed
void EH(double *b, double *c, int *n, int *m, int *bsuf, int *csuf, double *E, double *H, int *NR)
{
	double *g=NULL, *gi=NULL, *bc=NULL, prob;
	int k, i0=-1;
	void *_g = realloc(g, ((n[0]) * sizeof(double)));
	g=(double*)_g;
	void *_gi = realloc(gi, ((n[0]-1) * sizeof(double)));
	gi=(double*)_gi;
	void *_bc = realloc(bc, (n[0]-1)*sizeof(double));
	bc=(double*)_bc;
    for (int i=0;i<2*n[0];i++)
    {
        E[i]=0;
        for (int j=0;j<2*n[0];j++)
        {
            H[j+n[0]*i]=0;
        }
    }
	for (int i=0;i<n[0];i++)
	{
		E[i]=m[n[0]];
		H[i+2*n[0]*i]=0;
		E[n[0]+i]=m[n[0]]*(n[0]-1);
		H[(n[0]+i)+2*n[0]*(n[0]+i)]=0;
		for (int s=1;s<n[0];s++)
		{
            k=0;
            for (int j=0;j<n[0];j++)
            {
                if (j!=i) {bc[k]=exp(b[j]+(s-1)*c[j]); k++;}
            }
            k=n[0]-1;
            ElSym(bc,&k,g,&i0);
            prob=exp(b[i]+(s-1)*c[i])*g[s-1]/(g[s]+exp(b[i]+(s-1)*c[i])*g[s-1]);
            E[i]+=m[s]*prob;
            H[i+2*n[0]*i]-=m[s]*(prob-prob*prob);
            E[n[0]+i]+=m[s]*(s-1)*prob;
            H[(n[0]+i)+2*n[0]*(n[0]+i)]-=m[s]*((s-1)*(s-1)*prob-((s-1)*prob)*((s-1)*prob));
            H[i+2*n[0]*(n[0]+i)]-=m[s]*((s-1)*prob-(prob)*((s-1)*prob));
            H[(n[0]+i)+2*n[0]*i]-=m[s]*((s-1)*prob-(prob)*((s-1)*prob));
            // fill off-diagonals of Hessian if so desired
            // rem.: off-diagonal of b[i] and c[i] is always used!
            // rem.: stable difference algorithm is used!
            if (NR[0]==1)
            {
                for (int ii=1;ii<i;ii++)
                {
                    k=0;
                    for (int j=0;j<n[0];j++)
                    {
                        if ((j!=i)&(j!=ii)) {bc[k]=exp(b[j]+(s-1)*c[j]); k++;}
                    }
                    k=n[0]-2;
                    ElSym(bc,&k,gi,&i0);
                }
            }
		}
	}

	free(g);
	free(gi);
	free(bc);
}

