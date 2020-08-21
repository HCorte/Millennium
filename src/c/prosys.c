/*  ===This code is taken from PROSYS system===================================

V02 01-aug-2000 uxn OPSTXT() added.
V01 25-nov-1999 uxn Initial release for EuroGOLS
------------------------------------------------------------------------------
        This item is the property of GTECH Corporation, Providence,
        Rhode Island, and contains confidential and trade secret information.
        It may not be transfered from the custody or control of GTECH except
        as authorized in writing by an officer of GTECH.  Neither this item
        nor the information it contains may be used, transfered, reproduced,
        published, or disclosed, in whole or in part, and directly or
        indirectly, except as expressly authorized by an officer of GTECH,
        pursuant to written agreement.

        Copyright (c) 1993 GTECH Corporation.  All rights reserved.

    =========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ssdef.h>
#include <time.h>
#include <starlet.h>
#include <descrip.h>

#include "inclib:prosys.h"

extern void OPSTXT( void* );

int gh_wakeself( void) {

    SYS$WAKE( 0, 0);
    return 1;
}


void g_hiber(void) 
{
    SYS$HIBER();
}

static int waiting;

/*
** AST subroutine:
**  - sets the waiting flag to 0
**  - wakes the task
*/

static void wtmrast(int tmrid)
{
    waiting = 0;
    gh_wakeself();
}

void g_waitms(int msec)		/* number of milliseconds to sleep for */
{
    int timarg[2];
		/* convert msec to (negative) 100 nanosecs 
	        NOTE: only one word of the 64 bit time argument is converted
		this limits the millisecond argument to less than 429495 msec */


    timarg[0] = -10000*msec;
    timarg[1] = 0xFFFFFFFF;

    waiting = 1;
    if (SYS$SETIMR(0,timarg,wtmrast,0,0) != SS$_NORMAL) {
        exit(0);
    }

    while (waiting) {
	g_hiber();
    }
}
/*****************************************************************************/
void ops_msg( char *s )
{
   struct dsc$descriptor_s desc;
   
   desc.dsc$w_length  = strlen(s);
   desc.dsc$a_pointer = s;
   desc.dsc$b_dtype = DSC$K_DTYPE_T;
   desc.dsc$b_class = DSC$K_CLASS_S;

   OPSTXT( &desc );
}
/*****************************************************************************/
void g_crash() {
    ops_msg("STOP0005  ****  FATAL ERROR");
    exit(0);
}
/*****************************************************************************/
/*  ===[ GH_OPRPROMPT.C ]=======================================================

    Description:
    
     Issues prompt by combining the prompt string with the default string.
     The default, if specified, is enclosed by ' [' and ']'.
     A " >" pair is added to the end of the promptline.
     Reads input into supplied buffer
     returns 1 if EOF character entered
     returns 0 otherwise    
    
    Revisions:

        REV     DATE      BY       DESCRIPTION
        ----  ---------   ---      ------------------------------------------
        1.00  01-FEB-96   LSD      initial revision

    -------------------------------------------------------------------------
        This item is the property of GTECH Corporation, Providence,
        Rhode Island, and contains confidential and trade secret information.
        It may not be transfered from the custody or control of GTECH except
        as authorized in writing by an officer of GTECH.  Neither this item
        nor the information it contains may be used, transfered, reproduced,
        published, or disclosed, in whole or in part, and directly or
        indirectly, except as expressly authorized by an officer of GTECH,
        pursuant to written agreement.

        Copyright (c) 1996 GTECH Corporation.  All rights reserved.
    =========================================================================*/


/*  ===[ GH_OPRINPUT.C ]=======================================================

    Description:
    
     Reads input into supplied buffer
     returns 1 if EOF character entered
     returns 0 otherwise    
    
    Revisions:

        REV     DATE      BY       DESCRIPTION
        ----  ---------   ---      ------------------------------------------
        1.00  01-FEB-96   LSD      initial revision

    -------------------------------------------------------------------------
        This item is the property of GTECH Corporation, Providence,
        Rhode Island, and contains confidential and trade secret information.
        It may not be transfered from the custody or control of GTECH except
        as authorized in writing by an officer of GTECH.  Neither this item
        nor the information it contains may be used, transfered, reproduced,
        published, or disclosed, in whole or in part, and directly or
        indirectly, except as expressly authorized by an officer of GTECH,
        pursuant to written agreement.

        Copyright (c) 1996 GTECH Corporation.  All rights reserved.
    =========================================================================*/


#include <stdio.h>
#include <string.h>



int gh_oprinput( char *input_buffer) {


    *input_buffer = 0;		/* incase no input read */
    if( !gets( input_buffer) ) {
	return 1;
    }
    return 0;
}


int gh_oprprompt(   char *prompt_string,
		    char *default_string,
		    char *input_buffer)
{

    if( prompt_string) {
	printf( "%s", prompt_string);

	if( default_string && *default_string) {
	    printf( " [%s]", default_string);
	}
    }	

    printf( " >");

    return( gh_oprinput( input_buffer) );
}

int g_oprstr( char *prompt_string, 
	      char *default_string, 
	      int  minlen, 
	      int  maxlen, 
	      char *return_buffer) {

  char input_buffer[256];
  int default_length;
  int input_length;


				/* should default be ignored */
    if( default_string && (minlen <= maxlen) ) {
	default_length = strlen( default_string );
	if( ( default_length < minlen) || (default_length > maxlen ) ) {
	    default_string = 0;
	}
    }

    while( 1 ) {

        if( gh_oprprompt( prompt_string, default_string, input_buffer) ) {
            return 1;
        }

	if( (input_buffer[0] == 0) && default_string) {
	    strcpy( return_buffer, default_string);
	    return 0;
	}

	if( minlen > maxlen) {
	    strcpy( return_buffer, input_buffer);
	    return 0;
	}

	input_length = strlen( input_buffer);
	if( (input_length >= minlen) && (input_length <= maxlen) ) {
	    strcpy( return_buffer, input_buffer);
	    return 0;
	}

	printf(" Enter a string");
	if( minlen == maxlen ) {
	    printf(" %d characters long", minlen);
	}
	else if( minlen < maxlen) {
	    printf( " between %d and %d characters long", minlen, maxlen);
	}
    }
}
/*  ===[ G_OPRINT.C ]=======================================================

    Description:
    
     Operator numeric entry.
     An operator is prompted with 'prompt_string' for numeric entry input.
     The entered input is parsed according to g_parnum and the result
     returned in 'rtn_value'. If min_value <= default_value <= max_value 
     the default value is converted to a ascii
     string 'defstr'. The operator is then prompted with ['defstr']
     appended to the 'prompt_string' and may choose the default value
     by entering <CR>. If min_value <= max_value then the entered number must
     fall between these bounds, if the entered number does not fall
     between these bounds then the operator is prompted again until valid
     input is received or EOF is entered.
    
    Return: long
     1 if EOF was entered
     0 otherwise
    
    Revisions:

      REV     DATE      BY       DESCRIPTION
      ----  ---------   ---      ------------------------------------------
      1.00  01-FEB-93   LSD      initial revision
      4.00  18-feb-96	rm	 multiplatform conversion
      4.10  06-nov-96	rm	 bounds check now applied even if min == max

    -------------------------------------------------------------------------
        This item is the property of GTECH Corporation, Providence,
        Rhode Island, and contains confidential and trade secret information.
        It may not be transfered from the custody or control of GTECH except
        as authorized in writing by an officer of GTECH.  Neither this item
        nor the information it contains may be used, transfered, reproduced,
        published, or disclosed, in whole or in part, and directly or
        indirectly, except as expressly authorized by an officer of GTECH,
        pursuant to written agreement.

        Copyright (c) 1993 GTECH Corporation.  All rights reserved.
    =========================================================================*/
/*  ===[ G_PARNUM.C ]=======================================================

    Description:
    
     Extracts a number from an ascii string ('string') skipping leading spaces
     and stopping at the first nondigit character. Then parsed number is
     returned in 'value'.
    
    Return type: int
     count - the # of characters processed in making number
     0 implies no number found and converted
    
    Revisions:

        REV     DATE      BY       DESCRIPTION
        ----  ---------   ---      ------------------------------------------
        1.00  01-FEB-93   LSD      initial revision

    -------------------------------------------------------------------------
        This item is the property of GTECH Corporation, Providence,
        Rhode Island, and contains confidential and trade secret information.
        It may not be transfered from the custody or control of GTECH except
        as authorized in writing by an officer of GTECH.  Neither this item
        nor the information it contains may be used, transfered, reproduced,
        published, or disclosed, in whole or in part, and directly or
        indirectly, except as expressly authorized by an officer of GTECH,
        pursuant to written agreement.

        Copyright (c) 1993 GTECH Corporation.  All rights reserved.
    -------------------------------------------------------------------------
    =========================================================================*/


int g_parnum(	char *string,	/* string to parse number from, input */
		int *value)	/* value of number parsed, output */
{
  int minus_sign;
  int number_started;
  int number;
  int count;
  char byte;


    minus_sign = 0;
    number_started = 0;
    number = 0; 
    count = 0;	

    while (1) {

	byte = string[count++];

	if(!number_started) {

	    if(byte == 0)
		return(0);

	    if(byte == ' ')
		continue;

	    if (byte == '-') {
		byte = string[count++];
		minus_sign = 1;
	    }
	    else if (byte == '+') {
		byte = string[count++];
	    }

	    if(byte < '0' || byte > '9')
		return(0);

	    number_started = 1;
	    number = byte - '0';
	}
	else {
	    if(byte < '0' || byte > '9')
		break;

	    number = number * 10 + (byte - '0');
	}
    }

    if(minus_sign)
	*value = - number;
    else
	*value = number;

    return(count-1);
}



int g_oprint(	char *prompt_string,
		int default_value,  /* in <CR> entered */
		int min_value,	    /* minimum allowable input */ 
		int max_value,	    /* maximum allowable input */
		int *rtn_value) {

  char default_buffer[16], *default_string;
  char input_buffer[256];
  int value;

				    /* set up default */

    if( (default_value >= min_value) && (default_value <= max_value) ) {
        sprintf( default_buffer, "%d", default_value);
        default_string = default_buffer;
    }
    else {
        default_string = 0;
    }


    while (1) {

        if( gh_oprprompt( prompt_string, default_string, input_buffer) ) {
            return 1;
        }

        if( (input_buffer[0] == 0) && default_string ) {
            *rtn_value = default_value;
            return 0;
        }

	if( g_parnum( input_buffer, &value) &&
	  ( ( min_value > max_value) ||	/* no checking if bounds reversed */
            ( (value >= min_value) && (value <= max_value) ) ) ) {
	    *rtn_value = value;
            return(0);
        }

        printf(" Enter a number");
        if (min_value <= max_value) {
	    printf(" between %d and %d", min_value, max_value);
        }
    }
}
