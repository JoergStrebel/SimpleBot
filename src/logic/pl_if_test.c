

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SWI-Prolog.h>

static int iIsInitialized=0;

int init_connection(char* progname)
{
  char *av[4];
  int ac = 0;
  
  av[ac++] = progname;
  av[ac++] = "-q -l";
  av[ac++] = "./logic/dcg_deutsch.pro";
  av[ac]   = NULL;

  if ( !PL_initialise(ac, av) )
  {
    fprintf(stderr, "error initializing\n");
    PL_halt(1);
    return(0);
  }
  else 
  {
    printf("success initializing!\n");
    iIsInitialized=1;
    return(1);
  }
 }

int query(char* strQuery, char** presult)
{
  int rval;
  if (iIsInitialized)
    {
      predicate_t pred = PL_predicate("start",2,"database");
 
      term_t t0 = PL_new_term_refs(2);
      term_t t1 = t0+1;

      PL_put_string_chars(t0,strQuery);
      PL_put_variable(t1);

      rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, t0);
      if (!rval )
	{
	  fprintf(stderr, "error in query\n");
	}
      else 
	{
	  char* aresult;
	  int slen;
	  PL_get_string_chars(t1,&aresult,&slen);
	  *presult=aresult;
	}

      return (rval);
    }
  else
    {
      return(0);
    }
}

void stop_connection(int status)
{
  if (iIsInitialized)
  {
    printf("Shutting down...\n");
    PL_halt(status);
  }
}
