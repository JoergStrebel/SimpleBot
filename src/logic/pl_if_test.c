

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SWI-Prolog.h>


int main(int argc, char **argv)
{

  char *av[4];
  int ac = 0;
  int rval;
  
  printf("0tes Argument: %s\n",argv[0]);

  av[ac++] = argv[0];
  av[ac++] = "-l";
  av[ac++] = "is-a-demo.pro";
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

  /* Abfrage: ?- eigenschaft(hund,is_a,X).*/
 predicate_t pred = PL_predicate("eigenschaft",3,"database");
 
 term_t t0 = PL_new_term_refs(3);
 term_t t1 = t0+1;
 term_t t2 = t0+2;

 PL_put_atom_chars(t0,"hund");
 PL_put_atom_chars(t1,"is_a");
 PL_put_variable(t2);

 rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, t0);
 if (!rval )
 {
    fprintf(stderr, "error in query\n");
    PL_halt(1);
 }
 else 
 {
   char* aresult;
   PL_get_atom_chars(t2,&aresult);
   printf("success in query: %s \n", aresult);
    PL_halt(0);
 }

 return (rval);
}
