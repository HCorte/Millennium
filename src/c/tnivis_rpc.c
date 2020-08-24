static const char *fileid = "";

/*
 * ===[tnivis_rpc.c]======================================================
 *
 * Description:
 *
 * Functions to process an RPC messaging snapshot.
 *
 * Functions:
 *
 * Rpc_Messaging - Parse RPC Client Data PDU
 * Rpc_Messaging_Input - Process each RPC message in RPC Client Data PDU
 *
 * -----------------------------------------------------------------------
 * This item is the property of GTECH Corporation, Providence,
 * Rhode Island, and contains confidential and trade secret information.
 * It may not be transfered from the custody or control of GTECH except
 * as authorized in writing by an officer of GTECH.  Neither this item
 * nor the information it contains may be used, transfered, reproduced,
 * published, or disclosed, in whole or in part, and directly or
 * indirectly, except as expressly authorized by an officer of GTECH,
 * pursuant to written agreement.
 *  
 * Any and all modifications to this item must have the prior written
 * authorization of GTECH's Enterprise Series Platform Team.  GTECH shall
 * not be liable in any way for any direct or indirect damages,  whatsoever,
 * as a result of any unauthorized modifications.  The Enterprise Series
 * Platform Team reserves the right to refuse support as a result of
 * unauthorized modification.
 *
 * Copyright (c) 2005 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * =======================================================================
 */

#include <string.h>

#include "includes_vision.h"

#define RPCMSGS_SNAPSHOT  0 
#define RPCSTATS_SNAPSHOT 1

static int current_snapshot;

static void Rpc_Msgs_Snapshot();
static void Rpc_Stats_Snapshot();

/* [Rpc_Messaging]
 *
 * Summary:
 *
 * Rpc_Messaging()
 *
 * Description:
 *
 * This function breaks the RPC request tag into its component parts.
 *
 * Returns Values: None
 *
 */

void
Rpc_Messaging()
{
    if(current_snapshot == RPCSTATS_SNAPSHOT)
    {
        Rpc_Stats_Snapshot();
    }
    else  /* Defualt */
    {
        Rpc_Msgs_Snapshot();
    }
}

/* [Rpc_Msgs_Snapshot]
 *
 * Summary:
 *
 * Rpc_Msgs_Snapshot()
 *
 * Description:
 *
 * This function displays RPC Message request/response summary
 *
 * Returns Values: None
 *
 */
static void
Rpc_Msgs_Snapshot()
{
    int                 line_no = 0;

/*   snap shot title                                                          */

    wmove (win, 0, 0);
    wprintw (win, "RPC MESSAGING SNAPSHOT");

    wmove (win, 0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time());

    line_no = 2;
    wmove (win, line_no, (MAXCOL - 20)/2);
    wprintw (win, "Outbound (ES) RPCS");

    line_no++;

    wmove (win, line_no, 13);
    wprintw (win, "Requests");

    wmove (win, line_no, 33);
    wprintw (win, "Responses");

    wmove (win, line_no, 52);
    wprintw (win, "Response Time");


    line_no++;

    wmove (win, line_no, 2);
    wprintw (win,
             "----------------------------------------------------------------");

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Received      : %8d",
             tnicon_p->tot_rpc_rqsts_ack);

    wmove (win, line_no, 28);
    wprintw (win, "Received : %8d",
             tnicon_p->tot_rpc_resps_rcvd);

    wmove (win, line_no, 51);
    wprintw (win, "Minimum : %5.5d",
             tnicon_p->min_rpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Sent          : %8d",
             tnicon_p->tot_rpc_rqsts_sent);

    wmove (win, line_no, 28);
    wprintw (win, "Returned : %8d",
             tnicon_p->tot_rpc_resps);

    wmove (win, line_no, 51);
    wprintw (win, "Maximum : %5.5d",
             tnicon_p->max_rpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Rejected      : %8d",
             tnicon_p->tot_rpc_rqst_failures);

    wmove (win, line_no, 28);
    wprintw (win, "Rejected : %8d",
             tnicon_p->tot_rpc_resp_failures);

    wmove (win, line_no, 51);
    wprintw (win, "Average : %5.5d",
             tnicon_p->ave_rpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Wrt Fail      : %8d",
             tnicon_p->tot_rpc_rqsts_wrt_lost);

    wmove (win, line_no, 28);
    wprintw (win, "Late     : %8d",
            tnicon_p->tot_rpc_resp_late);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Timeouts      : %8d",
             tnicon_p->tot_rpc_rqst_timeouts);

    wmove (win, line_no, 28);
    wprintw (win, "Err Pdus : %8d",
             tnicon_p->tot_rpc_err_pdus_rcvd);

    line_no++;
    line_no++;

    wmove (win, line_no, (MAXCOL - 20)/2);
    wprintw (win, "Inbound (Host) RPCs");

    line_no++;

    wmove (win, line_no, 13);
    wprintw (win, "Requests");

    wmove (win, line_no, 33);
    wprintw (win, "Responses");

    wmove (win, line_no, 52);
    wprintw (win, "Response Time");

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win,
             "----------------------------------------------------------------");

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Received      : %8d",
             tnicon_p->tot_rpc_rqsts_rcvd);

    wmove (win, line_no, 28);
    wprintw (win, "Sent     : %8d",
             tnicon_p->tot_rpc_resps_sent);

    wmove (win, line_no, 51);
    wprintw (win, "Minimum : %5.5d",
             tnicon_p->min_hostrpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 28);
    wprintw (win, "Rejected : %8d",
             tnicon_p->tot_hostrpc_resp_failures);

    wmove (win, line_no, 51);
    wprintw (win, "Maximum : %5.5d",
             tnicon_p->max_hostrpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 28);
    wprintw (win, "Wrt Fail : %8d",
             tnicon_p->tot_rpc_resps_wrt_lost);

    wmove (win, line_no, 51);
    wprintw (win, "Average : %5.5d",
             tnicon_p->ave_hostrpc_resp_time_dis);
    
    line_no++;

    wmove (win, line_no, 28);
    wprintw (win, "Err Pdus : %8d",
             tnicon_p->tot_rpc_err_pdus_sent);

    wmove  (win,21,0);
    wprintw (win,"  STATS, EXIT, MENU");

    wmove (win,23,0);
    wprintw (win,"Selection: ");

    refresh ();
    wrefresh (win);

    wmove (win, 23, 11);

    input_y = 23;
    input_x = 11;
}

/* [Rpc_Stats_Snapshot]
 *
 * Summary:
 *
 * Rpc_Stats_Snapshot()
 *
 * Description:
 *
 * This function displays RPC statistics
 *
 * Returns Values: None
 *
 */
static void
Rpc_Stats_Snapshot()
{
    int                 line_no = 0;

    char                timeret[9];

    struct tm           loc_time;
    struct tm          *loc_time_p;

    loc_time_p = &loc_time;

/*   snap shot title                                                          */

    wmove (win, 0, 0);
    wprintw (win, "RPC MESSAGING SNAPSHOT");

    wmove (win, 0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time());

    line_no = 2;
    loc_time_p = localtime (&tnicon_p->last_stats_time.time);
    strftime (timeret, 9, "%H:%M:%S", loc_time_p);
 
    wmove (win, line_no, (MAXCOL - 34)/2);
    wprintw (win, "Last Statistics Interval (%s)",
             timeret);

    line_no++;
    wmove (win, line_no, (MAXCOL - 20)/2);
    wprintw (win, "Outbound (ES) RPCs");

    line_no++;
    wmove (win, line_no, 13);
    wprintw (win, "Requests");

    wmove (win, line_no, 33);
    wprintw (win, "Responses");

    wmove (win, line_no, 52);
    wprintw (win, "Response Time");

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win,
             "----------------------------------------------------------------");

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Received      : %8d",
             tnicon_p->num_rpc_rqsts_ack_dis);

    wmove (win, line_no, 28);
    wprintw (win, "Received : %8d",
             tnicon_p->num_rpc_resps_rcvd_dis);

    wmove (win, line_no, 51);
    wprintw (win, "Minimum : %5.5d",
             tnicon_p->min_rpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Sent          : %8d",
             tnicon_p->num_rpc_rqsts_sent_dis);

    wmove (win, line_no, 28);
    wprintw (win, "Returned : %8d",
             tnicon_p->num_rpc_resps_dis);

    wmove (win, line_no, 51);
    wprintw (win, "Maximum : %5.5d",
             tnicon_p->max_rpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Rejected      : %8d",
             tnicon_p->num_rpc_rqst_failures_dis);

    wmove (win, line_no, 28);
    wprintw (win, "Rejected : %8d",
             tnicon_p->num_rpc_resp_failures_dis);

    wmove (win, line_no, 51);
    wprintw (win, "Average : %5.5d",
             tnicon_p->ave_rpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Wrt Fail      : %8d",
             tnicon_p->num_rpc_wrt_failures_dis);

    wmove (win, line_no, 28);
    wprintw (win, "Late     : %8d",
             tnicon_p->num_rpc_resp_late_dis);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Timeouts      : %8d",
             tnicon_p->num_rpc_rqst_timeouts_dis);

    wmove (win, line_no, 28);
    wprintw (win, "Err Pdus : %8d",
             tnicon_p->num_rpc_err_pdus_rcvd_dis);

    line_no++;
    line_no++;

    wmove (win, line_no, (MAXCOL - 20)/2);
    wprintw (win, "Inbound (Host) RPCs");

    line_no++;

    wmove (win, line_no, 13);
    wprintw (win, "Requests");

    wmove (win, line_no, 33);
    wprintw (win, "Responses");

    wmove (win, line_no, 52);
    wprintw (win, "Response Time");

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win,
             "----------------------------------------------------------------");

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "Received      : %8d",
             tnicon_p->num_rpc_rqsts_rcvd_dis);

    wmove (win, line_no, 28);
    wprintw (win, "Sent     : %8d",
             tnicon_p->num_rpc_resps_sent_dis);

    wmove (win, line_no, 51);
    wprintw (win, "Minimum : %5.5d",
             tnicon_p->min_hostrpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 28);
    wprintw (win, "Rejected : %8d",
             tnicon_p->num_hostrpc_resp_failures);

    wmove (win, line_no, 51);
    wprintw (win, "Maximum : %5.5d",
             tnicon_p->max_hostrpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 28);
    wprintw (win, "Wrt Fail : %8d",
             tnicon_p->num_rpc_resps_wrt_lost);

    wmove (win, line_no, 51);
    wprintw (win, "Average : %5.5d",
            tnicon_p->ave_hostrpc_resp_time_dis);

    line_no++;

    wmove (win, line_no, 28);
    wprintw (win, "Err Pdus : %8d",
             tnicon_p->num_rpc_err_pdus_sent_dis);

    wmove  (win,21,0);
    wprintw (win,"  MSGS, EXIT, MENU");

    wmove (win,23,0);
    wprintw (win,"Selection: ");

    refresh ();
    wrefresh (win);

    wmove (win, 23, 11);

    input_y = 23;
    input_x = 11;
}

/* [Rpc_Messaging_Input]
 *
 * Summary:
 *
 * Rpc_Messaging_Input(char *command, int par)
 *
 * rpcRequestTag   - pointer to the RPC request tag of response
 * replyCode       - reply code to send to product
 *
 * Description:
 *
 * This function breaks the RPC request tag into its component parts.
 *
 * Returns Values: None
 *
 */

void
Rpc_Messaging_Input(char *command, int par)
{

    if ((strcasecmp(command, "MSGS")) == 0 )
    {
        current_snapshot = RPCMSGS_SNAPSHOT;
        wclear(win);
    }
    else if ((strcasecmp(command, "STATS")) == 0 )
    {
        current_snapshot = RPCSTATS_SNAPSHOT;
        wclear(win);
    }

}
