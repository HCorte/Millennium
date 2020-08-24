static const char *fileid = "";

/*============================================================================*/
/*                                                                            */
/* This item is the property of GTECH Corporation, West Greewich, Rhode       */
/* Island, and contains confidential and trade secret information. It may     */
/* not be transferred from the custody or control of GTECH except as          */
/* authorized in writing by an officer of GTECH. Neither this item not the    */
/* information it contains may be used, transferred, reproduced, published    */
/* or disclosed, in whole or in part, and directly or indirectly, except      */
/* as expressly authorized by an officer of GTECH, pursuant to written        */
/* agreement.                                                                 */
/*                                                                            */
/* Any and all modifications to this item must have the prior written         */
/* authorization of GTECH's Enterprise Series Platform Team.  GTECH shall     */
/* not be liable in any way for any direct or indirect damages,  whatsoever,  */
/* as a result of any unauthorized modifications.  The Enterprise Series      */
/* Platform Team reserves the right to refuse support as a result of          */
/* unauthorized modification.                                                 */
/*                                                                            */
/* Copyright 2005 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[SERVER_MAIN_LOOP.C]====================================================*/
/*                                                                            */
/* Server_Main_Loop()                                                         */
/*                                                                            */
/* Purpose: TNI server main processing loop.                                  */
/*                                                                            */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*                                                                            */
/*====[SERVER_MAIN_LOOP.C]====================================================*/
/*                                                                            */

#include <stddef.h>
#include <stdlib.h>

#include "includes.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <sys/timeb.h>
#   include <signal.h>

#elif defined(XOS_VMS)

#   include <timeb.h>
#   include <iodef.h>

#else

#   error - OS-specific logic not handled.

#endif

#define TAKEOVER_EVENT    0
#define GAME_OUTPUT_EVENT 1
#define BLOCKING_EVENT    2
#define GAME_INPUT_EVENT  3
#define CONNECTION_EVENT  4
#define KEEP_ALIVE_EVENT  5
#define STATISTICS_EVENT  6
#define LOG_EVENT         7
#define COMM_MODE_EVENT   8
#define RPC_TIMEOUT_EVENT 9
#define AUTHENTICATION_EVENT 10
#define MAX_EVENTS        11

#if defined(PROSYS_ENV_ALL)

    G_ERROR  error;
    GM_ADDRESS srcadr;
    GM_MESDSC mesdsc, cntxtdsc;

#endif

#if defined(GOLS_ENV_ALL)

#   if defined(XOS_VMS)

#       include <stdlib.h>
#       include <stdio.h>
#       include <ssdef.h>

        unsigned int SYS$HIBER();
        unsigned int SYS$WAKE();
        unsigned int SYS$SETIMR();

        static int waiting;

        static void wtmrast(int tmrid)
        {
            waiting = 0;
            SYS$WAKE(0, 0);
        }

        void g_waitms(int msec)        /* number of milliseconds to sleep for */
        {

            /* convert msec to (negative) 100 nanosecs                        */
	    /* NOTE: only one word of the 64 bit time argument is converted   */
            /* this limits the millisecond argument to less than 429495 msec  */

            int timarg[2];

            timarg[0] = -10000*msec;
            timarg[1] = 0xFFFFFFFF;

            waiting = 1;

            if (SYS$SETIMR(0,timarg,wtmrast,0,0) != SS$_NORMAL)
            {
                printf( "SYS$SETIMR error.\n");
                exit(0);
            }

            while (waiting)
            {
                SYS$HIBER();
            }
        }
#   endif

#endif

void Server_Main_Loop() {
    int st;                              /* System service return status */
    int event;

    int event_time[MAX_EVENTS];

    int takeo_evnt_invl;
    int conn_evnt_invl;
    int gameout_evnt_invl;
    int block_evnt_invl;
    int gamein_evnt_invl;
    int alive_evnt_invl;
    int stats_evnt_invl;
    int log_evnt_invl;
    int cmode_evnt_invl;
    int rpctimeout_evnt_invl;
    int authentication_evnt_invl;

    int elapsed_time_ms;
    int wait_time_ms;
    int event_invl_zero = 0;

    struct timeb elapsed_time;
    struct timeb last_loop_time;

#   if defined(PROSYS_ENV_ALL)

        /* allocate memory for input, output message buffer and context buffer*/

        inp_msg_p   = (ubyte_1 *)malloc( volcom_p->sys.prod_meslen );
        out_msg_p   = (ubyte_1 *)malloc( volcom_p->sys.prod_meslen );
        cntxt_p     = (ubyte_1 *)malloc( volcom_p->sys.prod_meslen );

        /* set up receive buffers */

        mesdsc.mesbufsiz= volcom_p->sys.prod_meslen;
        mesdsc.mesbufp  = inp_msg_p;

        cntxtdsc.mesbufsiz  = volcom_p->sys.prod_meslen;
        cntxtdsc.mesbufp    = cntxt_p;

#   endif

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

        Set_Up_Signal_Handler();

#   endif

    err_string = null_err_string;

    /* initialize the time intervals in milliseconds of each event */

    stats_evnt_invl = tnicon_p->statistics_time.time * 1000 +
                      tnicon_p->statistics_time.millitm;

    alive_evnt_invl = 1000;

    block_evnt_invl = 10;

    gamein_evnt_invl = tnicon_p->read_atmpt_time.time * 1000 +
                     tnicon_p->read_atmpt_time.millitm;

    takeo_evnt_invl = tnicon_p->takeover_time.time * 1000 +
                      tnicon_p->takeover_time.millitm;

    conn_evnt_invl = tnicon_p->check_conn_time.time * 1000 +
                     tnicon_p->check_conn_time.millitm;

    gameout_evnt_invl = tnicon_p->cycle_rate;

    log_evnt_invl = 5000;

    cmode_evnt_invl = 500;

    rpctimeout_evnt_invl = tnicon_p->check_rpc_timeout.time * 1000 +
                           tnicon_p->check_rpc_timeout.millitm;

    authentication_evnt_invl = 30000;

    /* initialize event time table */

    event_time[TAKEOVER_EVENT] = takeo_evnt_invl;

    event_time[CONNECTION_EVENT] = conn_evnt_invl;

    event_time[GAME_OUTPUT_EVENT] = gameout_evnt_invl;

    event_time[BLOCKING_EVENT] = block_evnt_invl;

    event_time[GAME_INPUT_EVENT] = gamein_evnt_invl;

    event_time[KEEP_ALIVE_EVENT] = alive_evnt_invl;

    event_time[STATISTICS_EVENT] = stats_evnt_invl;

    event_time[LOG_EVENT] = log_evnt_invl;

    event_time[COMM_MODE_EVENT] = cmode_evnt_invl;

    event_time[RPC_TIMEOUT_EVENT] = rpctimeout_evnt_invl;

    event_time[AUTHENTICATION_EVENT] = authentication_evnt_invl;

    if( (st = ftime(&last_loop_time)) == -1 ) {

        output_err("Server_Main_Loop",
                   MI_TNI_NO_TIME,
                   MX_ERR_LVL_FATAL,
                   err_string);
    }

    init_event_log();

    Check_Comm_Mode();

    /* Make sure all event timer interval are not initialized to zero.   */
    /* An event timer interval of zero will cause the MX Server to enter */
    /* into a tight processing loop on the single event.                 */

    for (event = 0; event < MAX_EVENTS; event++)
    {
        if (event_time[event] == 0)
        {
            event_invl_zero = 1;
        }
    }

    if (event_invl_zero == 0)
    {

#   if defined(PROSYS_ENV_ALL)

        while( volcom_p->prod.state > DEAD && 
               volcom_p->prod.state < STOPPING ) {

#   endif

#   if defined(GOLS_ENV_ALL)

        while( tnicon_p->comm_mode != COMM_SHUTDOWN ) {

#   endif

            if( (st = ftime(&current_time)) == -1 ) {

                output_err("Server_Main_Loop",
                           MI_TNI_NO_TIME,
                           MX_ERR_LVL_FATAL,
                           err_string);
            }

            subtimes(&current_time,
                     &last_loop_time,
                     &elapsed_time);

            /* calculate elapsed time since completion of main loop */

            elapsed_time_ms = elapsed_time.time * 1000 +
                              elapsed_time.millitm;

            last_loop_time = current_time;
            wait_time_ms = 999999;

            for (event = 0; event < MAX_EVENTS; event++) {

                event_time[event] = event_time[event] - elapsed_time_ms;

                if (event_time[event] <= 0) {

                    switch (event) {

                    case (TAKEOVER_EVENT):

                        Check_Server_Takeover();

                        event_time[event] = takeo_evnt_invl;
                        break;

                    case (GAME_OUTPUT_EVENT):

#                       if defined(PROSYS_ENV_ALL)

                            Check_Game_Output(&mesdsc, &cntxtdsc);

#                       endif

#                       if defined(GOLS_ENV_ALL)

                            Check_Game_Output();
#                        endif

                        event_time[event] = gameout_evnt_invl;
                        break;

                    case (BLOCKING_EVENT):

                        Check_Blocking_Timers();
                        event_time[event] = block_evnt_invl;
                        break;

                    case (GAME_INPUT_EVENT):

                        Read_Client_Data();
                        event_time[event] = gamein_evnt_invl;
                        break;

                    case (CONNECTION_EVENT):

                        Get_New_Connections();
                        event_time[event] = conn_evnt_invl;
                        break;

                    case (KEEP_ALIVE_EVENT):

                        Check_Keep_Alive(); 
                        event_time[event] = alive_evnt_invl;
                        break;

                    case (STATISTICS_EVENT):

                        Check_Statistics_Timer();
                        event_time[event] = stats_evnt_invl;
                        break;

                    case (RPC_TIMEOUT_EVENT):

                        Check_Rpc_Request_Timeout();
                        event_time[event] = rpctimeout_evnt_invl;
                        break;

                    case (LOG_EVENT):

                        ChkExcessiveLogging();
                        event_time[event] = log_evnt_invl;
                        break;

                    case (COMM_MODE_EVENT):

                        Check_Comm_Mode();
                        event_time[event] = cmode_evnt_invl;
                        break;

                    case (AUTHENTICATION_EVENT):

                        Clear_Authentication_Lockouts();
                        Send_Authentication_Requests();
                        event_time[event] = authentication_evnt_invl;
                        break;
                    }
                }
                if (event_time[event] < wait_time_ms) {

                    wait_time_ms = event_time[event];
                }
            }

            /* calculate time taken to perform all events in this loop */

            if( (st = ftime(&current_time)) == -1 ) {

                output_err("Server_Main_Loop",
                           MI_TNI_NO_TIME,
                           MX_ERR_LVL_FATAL,
                           err_string);
            }

            subtimes(&current_time,
                     &last_loop_time,
                     &elapsed_time);

            elapsed_time_ms = elapsed_time.time * 1000 +
                              elapsed_time.millitm;

            /* calculate main loop wait time */

            wait_time_ms = wait_time_ms - elapsed_time_ms;

            if (wait_time_ms > 0) {
                g_waitms(wait_time_ms);
            }

        }  /* end while state */
    }
    else
    {
        sprintf(err_string.par1, "Main loop exit: event interval zero");
        output_err("Server_Main_Loop",
                   MI_TNI_STRING,
                   MX_ERR_LVL_FATAL,
                   err_string);
    }

#   if defined(PROSYS_ENV_ALL)

        /* task is coming down */
        gtms_deregtask(&error);

        g_endtask();                        

        if( error.errflg ) {
            g_senderror(&error);
        }

#   endif

#   if defined(GOLS_ENV_ALL)

        output_err("Server_Main_Loop",
                   MI_TNI_COMM_SHUTDWN,
                   MX_ERR_LVL_INFO,
                   err_string);

#   endif

} /* end main */

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

    void Set_Up_Signal_Handler(void) {

        struct sigaction sa;               /* sigaction structure             */

        sa.sa_handler = Signal_Handler;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART;

        if (sigaction(SIGTERM, &sa, NULL) == -1) {

            sprintf(err_string.par1, "SIGTERM");

            output_err("Set_Up_Signal_Handler",
                       MI_TNI_INIT_SIG,
                       MX_ERR_LVL_WARNING,
                       err_string);
        }

        sa.sa_handler = SIG_IGN;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = 0;

        sigaction(SIGUSR2, &sa, NULL);
    }

    void Signal_Handler(int sig) {

        err_string = null_err_string;

        switch (sig) {

        case (SIGTERM):

            output_err("Signal_Handler",
                       MI_TNI_TERM_SIGNAL,
                       MX_ERR_LVL_INFO,
                       err_string);

            Close_All_Connections();
            Close_Listen_Sockets();

            exit(EXIT_SUCCESS);
            break;
        }
    }

#endif
