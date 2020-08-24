static const char *fileid = "";

/*
 * ===[mx_rpc_request_timeout.c]==========================================
 *
 * Description:
 *
 * Functions to process RPC message timeouts
 *
 * Functions:
 *
 * Check_Rpc_Request_Timeout
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

#include "includes.h"

/* [Check_Rpc_Request_Timeout]
 *
 * Summary:
 *
 * Check_Rpc_Request_Timeout(void)
 *
 * Description:
 *
 * This function looks at each event in the RPC event queue and determines
 * if the RPC request has timed out.  Upon a time out the RPC event is
 * removed from the RPC event queue and a reply, indicating a time out 
 * error, is sent to the product that make the RPC request.
 *
 * Returns Values: None
 *
 */

void
Check_Rpc_Request_Timeout(void)
{
    int                 app_num = -1;
    int                 elapsed_time_ms;
    int                 next_rpc_rqst_event;
    struct timeb        elapsed_time;

    char                rpc_request_tag[MAX_RPC_RQST_TAG_LEN + 1];

    RQST_EVENT         *rpc_rqst_event;

    if ((rpc_events_queue->length > 0) &&
        (rpc_events_queue->head != -1))
    {
        rpc_rqst_event = request_events + (rpc_events_queue->head - 1);

        do
        {
/* Determine next event prior to the time out check because a timed out */
/* event will be deleted from the RPC event queue */

            if (rpc_rqst_event->next_event != -1)
            {
                next_rpc_rqst_event = rpc_rqst_event->next_event;
            }
            else
            {
                next_rpc_rqst_event = -1;
            }

/* Check for time out on current event */

            subtimes (&current_time,
                      &rpc_rqst_event->sent_time,
                      &elapsed_time);

            elapsed_time_ms = elapsed_time.time * 1000 +
                              elapsed_time.millitm;

            app_num = rpc_rqst_event->app_num;

            if (elapsed_time_ms >
                (tnicon_p->app[app_num].rpc_rqst_timeout * 1000))
            {
                sprintf (rpc_request_tag,
                         "%d-%d-%d-%d-%d:%d.%d",
                         rpc_rqst_event->event_num,
                         rpc_rqst_event->app_num,
                         rpc_rqst_event->transaction_id,
                         rpc_rqst_event->cdc,
                         rpc_rqst_event->hours,
                         rpc_rqst_event->minutes,
                         rpc_rqst_event->seconds);

                Send_Rpc_Response_To_Game (rpc_request_tag,
                                           MXSRV_RPC_ERR_RQST_TIMEOUT,
                                           "RPC request timeout",
                                           0,
                                           NULL,
                                           NO_RESP_CALC);

                tnicon_p->tot_rpc_rqst_timeouts++;
                tnicon_p->num_rpc_rqst_timeouts++;
            }

/* Set pointer to next event */

            if (next_rpc_rqst_event != -1 )
            {
                rpc_rqst_event = request_events + (next_rpc_rqst_event - 1);
            }

        } while (next_rpc_rqst_event != -1);
    }
    return;
}
