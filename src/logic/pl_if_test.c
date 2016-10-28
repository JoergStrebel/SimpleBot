

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SWI-Prolog.h>


int query(char* progname, char* strQuery)
{
  char *av[4];
  int ac = 0;
  int rval;
  
  av[ac++] = progname;
  av[ac++] = "-l";
  av[ac++] = "./logic/dcg_deutsch.pro";
  av[ac]   = NULL;

  if ( !PL_initialise(ac, av) )
  {
    fprintf(stderr, "error initializing\n");
    PL_halt(1);
  }
  else 
    {
      printf("success initializing!\n");
    }

 
 predicate_t pred = PL_predicate("start",2,"database");
 
 term_t t0 = PL_new_term_refs(2);
 term_t t1 = t0+1;

 PL_put_string_chars(t0,"Was ist ein Hund");
 PL_put_variable(t1);

 rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, t0);
 if (!rval )
 {
    fprintf(stderr, "error in query\n");
    PL_halt(1);
 }
 else 
 {
   char* aresult;
   int slen;
   PL_get_string_chars(t1,&aresult,&slen);
   printf("success in query: %s \n", aresult);
   PL_halt(0);
 }

 return (rval);
}
