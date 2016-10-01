

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SWI-Prolog.h>


int main(void)
{
  static char * av[] = {"kb1.plg", NULL};
  if( ! PL_initialise(1,av))
  {
    fprintf(stderr, "error initializing");
    PL_halt(1);
  }
  else 
    {
      printf("success initializing!");
    }

  /*
 predicate_t pred = PL_predicate("calc",1,"user");
 term_t h0 = PL_new_term_refs(1);

 int rval;
 char * expression = "pi/2";
 PL_put_atom_chars(h0,expression);
 rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, h0);
  */

 PL_halt(0);

 return 0;
}
