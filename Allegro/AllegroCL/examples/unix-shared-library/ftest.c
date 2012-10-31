/* This code is in the public domain. */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/mman.h>

#include "lisp.h"
#include "shlibs.h"

#ifdef __hpux
extern "C" {
#endif

/* #define FI_DEBUG 1 */

pthread_mutex_t m_fact_setup_complete = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m_lisp_work_complete = PTHREAD_MUTEX_INITIALIZER;

int factorial_arg = 0;
char *factorial_result = 0;

extern void *(*lisp_lookup_address)(char *);
extern void *(*acl_main_address)();
extern int lisp_init_complete();

typedef char *(*LispFactorial)(int);

char *(*lisp_factorial)(int) = 0;

void
set_factorial_callback()
{
    lisp_factorial = (LispFactorial)
	(*lisp_lookup_address)((char *)"factorial-callback");
    if(!lisp_factorial) {
        fprintf(stderr, "Unable to locate factorial-callback (%x)\n",
		lisp_factorial);
	exit( 1 );
    }
}


void
usage()
{
    fprintf( stderr, "usage: ftest n, where n is a positive integer.\n" );
}


unsigned int
initialize_lisp(char **, char *, int, int, 
		void (*exit_fn)( int ),
		struct shlib_library_item **linked_shared_libraries);

extern void lisp_ready_hook()
{
#ifdef FI_DEBUG
    fprintf(stdout, "ACL_main, on entry.\n");
    fflush(stdout);
#endif
    pthread_mutex_lock(&m_lisp_work_complete);
#ifdef FI_DEBUG
    fprintf(stdout, "ACL_main, signalled work complete.\n");
    fflush(stdout);
#endif
    lisp_init_complete();
    pthread_mutex_lock(&m_fact_setup_complete);
#ifdef FI_DEBUG
    fprintf(stdout, "ACL_main, pre setup.\n");
    fflush(stdout);
#endif
    set_factorial_callback();
#ifdef FI_DEBUG
    fprintf(stdout, "ACL_main, after callback setup.\n");
    fflush(stdout);
#endif
    if ((factorial_result = (*lisp_factorial) ( factorial_arg )) == 0) {
	fprintf(stderr, "factorial internal error\n" );
	exit( 1 );
    }
#ifdef FI_DEBUG
    fprintf(stdout, "ACL_main, after callback returned! (%x)\n",
	    factorial_result);
    fflush(stdout);
#endif
    pthread_mutex_unlock(&m_lisp_work_complete);
}

int
main( int argc, char *argv[], char **envp )
{
   int i, n;
   unsigned int res;

   /* printf("this is the factorial program!\n"); */

   if (argc != 2) {
      usage();
	exit( 1 );
    }
    for (i = 0; *(argv[1] + i); i++) {
    	if (!isdigit( *(argv[1] + i) )) {
	    usage();
	    exit( 1 );
	}
    }
    sscanf( argv[1], "%d", &n );

    pthread_mutex_lock(&m_fact_setup_complete);

    if (res = initialize_lisp(envp,
			      (char *)"fact.dxl", /* image name */
			      1,  /* libacli */
			      1,  /* suppress i/o */
			      0,  /* exit routine. 0 for default */
			      linked_shared_libraries)
	!= 0)
    {
	fprintf(stderr, "factorial startup error (%u)\n",res);
	fflush(stderr);
	exit( 1 );
    }

#ifdef FI_DEBUG
    fprintf(stdout, "2 this is the factorial program! (%x)\n",
	    factorial_result);
    fflush(stdout);
#endif
    /* set up args. */
    factorial_arg = n;
    pthread_mutex_unlock(&m_fact_setup_complete);
    pthread_mutex_lock(&m_lisp_work_complete);
    /* process result */
#ifdef FI_DEBUG
    fprintf(stdout, "3 this is the factorial program! (%x)\n",
	    factorial_result);
    fflush(stdout);
#endif
    fprintf(stdout, "%s\n", factorial_result);
    fflush(stdout);
#ifdef FI_DEBUG
    fprintf(stderr, "DEBUG: %s\n", factorial_result);
    fflush(stderr);
#endif
    exit(0);
}

#ifdef __hpux
};
#endif
