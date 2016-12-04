/* File for Prolog interface.  */
#ifndef PL_IF_TEST
#define PL_IF_TEST

int init_connection(char* progname);
int query(char* strQuery, char** presult);
void stop_connection(int status);

#endif
