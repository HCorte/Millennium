/* ===[ICSLOG.C]===================================================

   Description: ICSLOG main module and selected functions

   V02 25-NOV-1999  UXN Initial release for EuroGOLS.
   V01 12-aug-1996  mf  initial version

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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <starlet.h>
#include <ssdef.h>

#include "inclib:icslog.h"

#define MYNAME	"ICSLOG"
#define YEAR	"1996"

struct ICSLOG *ccp;
char line[256];
struct ICS_ERR icserr;

/* ----------------- advance breaks if any ------------- */
/*		    thruput stats if tick flag set	 */

static void advance_breaks()
{
    int k, df;

    if (ccp->tick)
    {
	ccp->tick = 0;
	df = 1;
    }
    else
	df = 0;
	    
    for (k = 0; k < ccp->max_clients; k++)
    {
	if (df && (ccp->client[k].status >= CONNECTED))
	{
	    ccp->client[k].thruput = ccp->client[k].blkcnt;
	    ccp->client[k].blkcnt = 0;
	}

	switch (ccp->client[k].status)
	{
	    case BATCHBREAK:
		ccp->client[k].batchtick--;
		if (ccp->client[k].batchtick <= 0)
		{
		    ccp->client[k].status = RECEIVING;
		    ccp->client[k].batchtick = 0;
		}
		break;
	    case RETRYBREAK:
                ccp->client[k].rtrytick--;
                if (ccp->client[k].rtrytick <= 0)
		{
                    ccp->client[k].status = RECEIVING;
		    ccp->client[k].rtrytick = 0;
		}
		break;
	    case XOFFBREAK:
		break;
	    default:
		break;
	}
    }
}

/* ------------ stats update ---------------- */

static void do_stats(void)
{
    int k;
    struct tm *mt;
    char msg[30];
    time_t t;

    ccp->tick = 0;
    for (k = 0; k < ccp->max_clients; k++)
    {
	if (ccp->client[k].status >= CONNECTED)
	{
	    ccp->client[k].thruput = ccp->client[k].blkcnt;
	    ccp->client[k].blkcnt = 0;
	}
    }
    if (ccp->verbosity >= RL_DEBUG)
    {
	
	sprintf(msg, "connected clients %d", ccp->act_clients);
	ops_msg( msg );

    }
}

/* ------------ timer interrupt handler ------------ */

static void minute_minder(int astid)
{
    int timarg[2];

    if (!astid)
	ccp->tick++;

    timarg[0] = -10000 * 60000;
    timarg[1] = 0xFFFFFFFF;

    if (SYS$SETIMR(0,timarg,minute_minder,0,0) != SS$_NORMAL) 
    {
	ops_msg("can't set timer");
	g_crash();
    }

    if (SYS$WAKE(0,0) != SS$_NORMAL)
    {
	ops_msg("can't wake myself");
	g_crash();
    }
}

/* ===================== main ======================== */

int main( )
{

    COPYRITE();        
    SNIF_AND_WRKSET();

    setup(&ccp);

    atexit(serv_exit);
    init_tcp(ccp);

    minute_minder(1);
    
    ccp->prodstate = PROD_RUNNING;

    while (ccp->prodstate == PROD_RUNNING) 
    {
	g_waitms(ccp->sleep_time); 
	ccp->cycles++;
	if (ccp->tick)
	   do_stats();

	if( ccp->act_clients < ccp->max_clients)
	   do_connect();

	if (ccp->act_clients)
	{
	   advance_breaks();
	   check_client();
	   read_client();
	   write_client();
	}
    }
    exit(0);
}
