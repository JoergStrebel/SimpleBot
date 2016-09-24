#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <hunspell/hunspell.h>

#include "simplebot.h"
#include "grammatik/lex.yy.h"


/* A static variable for holding the line. */
static char *line_read = (char *)NULL;
static Hunhandle * pSpeller;

/* Read a string, and return a pointer to it.
   Returns NULL on EOF. */
char * rl_gets ()
{
  /* If the buffer has already been allocated,
     return the memory to the free pool. */
  if (line_read)
    {
      free (line_read);
      line_read = (char *)NULL;
    }

  /* Get a line from the user. */
  line_read = readline ("Benutzer: ");

  /* If the line has any text in it,
     save it on the history. */
  if (line_read && *line_read)
    add_history (line_read);

  return (line_read);
}

/* check the spelling of the user input. */
int check_spelling(char *sLine)
{
  int iSpellResult=1;
  char *str1, *token;
  char *saveptr1;
  char *savsLine;
  int j;
  
  /* copy string from readline to be safe*/
  savsLine=strdup(sLine);
  
  /*tokenize string and spell-check the tokens*/
  for (j = 1, str1 = savsLine; ; j++, str1 = NULL) 
  {
      token = (char *)strtok_r(str1, " ", &saveptr1);
      if (token == NULL) break;
      printf("%d: %s\n", j, token);
      
      /* spell(word) - spellcheck word
	* output: 0 = bad word, not 0 = good word
	*/
      iSpellResult=iSpellResult && Hunspell_spell(pSpeller, token);
  }
  
  free(savsLine);
  return(iSpellResult);
}

int main(int argc, char* argv[]) 
{
    char *sLineInp;
    YY_BUFFER_STATE hdlParseBuf;
    
    printf("Hallo Welt!\n");
    
    /* Dictionary path on openSuse 13.2
     * /usr/share/hunspell/de_DE.aff
      /usr/share/hunspell/de_DE.dic
      */
    pSpeller=Hunspell_create("/usr/share/hunspell/de_DE.aff", "/usr/share/hunspell/de_DE.dic");
    
    while(1)
    {
      sLineInp=rl_gets();
      printf("%s\n",sLineInp);
      
      /*check spelling*/
      if(check_spelling(sLineInp)==0)
      {
	printf("Schreibfehler in Eingabe\n");
      }
      else printf("Ok.\n");
      
      /* parse input*/
      hdlParseBuf=yy_scan_string (sLineInp);
      yyparse();
      yy_delete_buffer(hdlParseBuf);
      
    } /*while*/
    
    return (0);
}

