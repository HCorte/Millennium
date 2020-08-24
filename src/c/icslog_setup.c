/*    
   V03 30-DEC-1999 UXN RUN_ON_PRIMARY, RUN_ON_BACKUP, RUN_ON_SPARE and
                       ACCEPT0,ACCEPT1 etc. added.
		       MAX_BADMSG added.
   V02 25-NOV-1999 UXN Initial release for EuroGOLS.
   V01 12-aug-1996  mf initial version

  --------------------------------------------------------------------
   This item is the property of GTECH Corporation, West Greewich, Rhode
   Island, and contains confidential and trade secret information. It
   may not be transferred from the custody or control of GTECH except
   as authorized in writing by an officer of GTECH. Neither this item
   nor the information it contains may be used, transferred,
   reproduced, published, or disclosed, in whole or in part, and
   directly or indirectly, except as expressly authorized by an
   officer of GTECH, pursuant to written agreement.

   Copyright 1996 GTECH Corporation. All rights reserved.
   --------------------------------------------------------------------
   ==================================================================== */
#include <types.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <ssdef.h>

#include "inclib:icslog.h"

#define MXPRMBUFLEN  8192
static char prmbuf[MXPRMBUFLEN];

#pragma extern_model common_block
extern struct ICSLOG ICSCOM; 

char msg[255];

static int get_config(int prmbuf_length, char *prmbuf_p)
{
  char pcfnam[] = "gxtsk:icslog.fil";
  int pcflen;
  FILE  *pcfp;


    if (!( pcfp = fopen( pcfnam, "r" ) ))
    {
        sprintf(msg,  "Could not open pcf file %s", pcfnam );
	ops_msg(msg);
        return 0;
    }

    pcflen = fread( prmbuf_p, prmbuf_length, 1, pcfp );
    fclose(pcfp);

    if (pcflen < 0) 
    {
        sprintf(msg, "PCF is not in the correct format");
	ops_msg( msg );
        return 0;
    }

    if( pcflen > prmbuf_length)
    {
        sprintf(msg, "PCF size( %d ) exceeds limit( %d )",
                pcflen, prmbuf_length);
	ops_msg( msg );
        return 0;
    }
    return 1;
}


static int get_integer(char *name, char mode, int replacement, int *target)
{
  char *ptr, *last;

    
    ptr = strstr( prmbuf, name );
    prmbuf[ sizeof(prmbuf)-1 ] = 0;

    if( !ptr ) {
        if( mode == 'D' || mode == 'H' ) {
           *target = replacement;
	   return 1;
        }
        return 0;
    }	   
    
    while( *ptr && (*ptr < '0' || *ptr > '9')) ptr++;
    last = ptr;
    while( *last && *last >= '0' && *last <='9') last++;
    *target = atoi(ptr);
    return(1);
}

#define my_is_space( ch ) \
((ch) == ' '  || (ch) == '\t' || (ch) == '\r' || (ch) == '\n')

static int get_string(char *name, char *target, int max)
{
    int  len;
    char *last;
    char *ptr = strstr( prmbuf, name );
    prmbuf[ sizeof(prmbuf)-1 ] = 0;

    if( !ptr )
	return 0;
    
    ptr += strlen(name);
    while(  my_is_space( *ptr ) ) ptr++;

    last = ptr;
    
    while( !my_is_space( *last ) ) last++;


    len = last - ptr;
    if( len > max ) len = max;

    strncpy(target, ptr, len);
    target[len] = 0;

    return 1;	
    
}
void setup(struct ICSLOG **icsp)
{
    struct ICSLOG *ccp;
    int temp, i;
    char str_name[255];

		/* map and initialize ICSLOG global section */

    ccp = &ICSCOM;

    memset(ccp, 0, sizeof(struct ICSLOG));

	    /* set defaults */
    ccp->max_clients = 1;
    ccp->max_out = 4096;
    ccp->do_checksum = 0;
    ccp->keep_alive = 1;
    ccp->sleep_time = 200;
    ccp->batch_size = 10;
    ccp->batch_break = 5;
    ccp->retry_delay = 10;
    ccp->max_retries = 3;
    ccp->max_errors = 20;
    ccp->max_badmsg = 20;
    ccp->verbosity = 3;

    ccp->version = VERSION;
    if (!get_config(MXPRMBUFLEN, prmbuf)) 
    {
        sprintf(msg,"get_config failed");
	ops_msg( msg );
        g_crash( );
    }

    if (get_integer("MAX_CLIENTS", 'D', 2, &temp))
    {
	if ((temp <= MAX_CLIENTS) && (temp > 0))
	    ccp->max_clients = temp;
    }	

    if (!get_integer("TCP_PORT", 'M', 0, &ccp->tcp_port)) 
    {
	sprintf(msg, "can't get TCP_PORT from PCF\n");
	ops_msg( msg );
	g_crash( );
    }

    if (get_integer("MAX_OUT_SIZE", 'D', 4096, &temp))
    {
	if ((temp >= 1024) && (temp <= 16394))
	    ccp->max_out = temp;
    }	

    if (get_integer("CHECKSUM", 'D', 1, &temp))
    {
	if ((temp == 1) || (temp == 0))
	    ccp->do_checksum = temp;
    }	

    if (get_integer("KEEP_ALIVE", 'D', 1, &temp))
    {
	if ((temp == 1) || (temp == 0))
	    ccp->keep_alive = temp;
    }	

    if (get_integer("SLEEP_PERIOD", 'D', 200, &temp))
    {
	if ((temp >= 50) && (temp <= 2000))
	    ccp->sleep_time = temp;
    }	

    if (get_integer("BATCH_SIZE", 'D', 4, &temp))
    {
	if ((temp <= 100) && (temp >= 1))
	    ccp->batch_size = temp;
    }	

    if (get_integer("BATCH_BREAK", 'D', 2, &temp))
    {
	if ((temp <= 9999) && (temp >= 1))
	    ccp->batch_break = temp;
    }	

    if (get_integer("RETRY_DELAY", 'D', 5, &temp))
    {
	if ((temp <= 100) && (temp >= 1))
	    ccp->retry_delay = temp;
    }	

    if (get_integer("MAX_RETRIES", 'D', 2, &temp))
    {
	if ((temp <= 10) && (temp >= 1))
	    ccp->max_retries = temp;
    }	

    if (get_integer("MAX_ERRORS", 'D', 20, &temp))
    {
	if ((temp <= 1000) && (temp >= 10))
	    ccp->max_errors = temp;
    }	

    if (get_integer("MAX_BADMSG", 'D', 20, &temp))
    {
	if ((temp <= 1000) && (temp >= 10))
	    ccp->max_badmsg = temp;
    }	

    if (get_integer("VERBOSITY", 'D', 2, &temp))
    {
	if ((temp <= 3) && (temp >= 0))
	    ccp->verbosity = temp;
    }	

    if (get_integer("VERBOSITY", 'D', 2, &temp))
    {
	if ((temp <= 3) && (temp >= 0))
	    ccp->verbosity = temp;
    }	

    if (get_integer("RUN_ON_PRIMARY", 'D', 1, &temp))
    {
	if ((temp == 1) || (temp == 0))
	    ccp->run_on_primary = temp;
    }	

    if (get_integer("RUN_ON_BACKUP", 'D', 1, &temp))
    {
	if ((temp == 1) || (temp == 0))
	    ccp->run_on_backup = temp;
    }	
    if (get_integer("RUN_ON_SPARE", 'D', 1, &temp))
    {
	if ((temp == 1) || (temp == 0))
	    ccp->run_on_spare = temp;
    }	

    for(i=0; i<MAX_CLIENTS; i++)
    {
       sprintf(str_name, "ACCEPT%1d", i);     
       get_string(str_name, ccp->ac[i].host, HOSTLEN);
    }	

    ccp->io_cycles = (MAX_IDLE + ccp->sleep_time - 1) / ccp->sleep_time;
    for (temp = 0; temp < MAX_CLIENTS; temp++)
    {
	ccp->client[temp].status = NEVERCONNECTED;
    }

    *icsp = ccp;
}
