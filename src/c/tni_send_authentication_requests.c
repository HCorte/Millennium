static const char  *fileid =
    "";

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
/* Copyright 2007 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[TNI_SEND_AUTHENTICATION_REQUESTS.C]====================================*/
/*                                                                            */
/* Send_Authentication_Requests()                                             */
/*                                                                            */
/* Purpose: This function sends authencation challenge request to all         */
/*          connections with failed authentication state.                     */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_SEND_AUTHENTICATION_REQUESTS.C]====================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void
Send_Authentication_Requests(void)
{
    int                 j;                          /* local connection index            */
    int                 timeout;
    int                 app_idx;


    err_string = null_err_string;

    for (j = 1; j <= MAX_CONFIGURABLE_CONN; j++)
    {

        if (tnicon_p->connection[j].tni_proto_ver >= TNI_VERSION_23)
        {
            app_idx = tnicon_p->connection[j].app_idx;

            if (app_idx >= 0 && 
                tnicon_p->app[app_idx].auth_required == 1 &&
                tnicon_p->app[app_idx].auth_lockout_state != APP_AUTH_LOCKED )
            {
                if (tnicon_p->connection[j].last_auth_atmpt_time.time + DEF_AUTH_ATMPT_TIMEOUT >= 
                        current_time.time)
                {
                    timeout = 1;
                }
                else
                {
                    timeout = 0;
                }

                /* 
                 * We send Authentication Request to a connection with state >= CONN_CONNECTED
                 * and one of the following is True
                 * 1. Authentication failed earlier on this connection
                 * 2. Authentication request was never sent on this connection as the application 
                 *    was in locked state 
                 * 3. Authentication request has been sent but no response received for
                 *    DEF_AUTH_ATMPT_TIMEOUT seconds.
                 */

                if(tnicon_p->connection[j].conn_state >= CONN_CONNECTED &&
                    (tnicon_p->connection[j].auth_state == AUTH_FAILURE ||
                     tnicon_p->connection[j].auth_state == AUTH_NOTATTEMPTED_LOCKED ||
                    (tnicon_p->connection[j].auth_state == AUTH_ATTEMPTED &&  timeout)))
                {
                    Server_Challenge_Request(j);
                }
            }
        }
           
    }
}

/*====[TNI_CLEAR_AUTHENTICATION_LOCKOUTS.C]===================================*/
/*                                                                            */
/* CLEAR_AUTHENTICATION_LOCKOUTS()                                            */
/*                                                                            */
/* Purpose: This function clears application authencation lockouts            */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_CLEAR_AUTHENTICATION_LOCKOUTS.C]===================================*/
/*                                                                            */

void
Clear_Authentication_Lockouts(void)
{
    int app_idx;
    int j;

    for (app_idx = 0; app_idx < MAX_APPS; app_idx++ ) 
    {
        if(tnicon_p->app[app_idx].auth_lockout_state == APP_AUTH_LOCKED &&
           current_time.time >= tnicon_p->app[app_idx].auth_lockout_release_time.time)
        {
            /* Clear the lockout state */
            tnicon_p->app[app_idx].auth_lockout_state = APP_AUTH_NOTLOCKED;
            memset(&tnicon_p->app[app_idx].auth_lockout_release_time.time,
                0x00,
                sizeof(tnicon_p->app[app_idx].auth_lockout_release_time.time));

            /* Reset consec_auth_failure_cnt */
            for (j = 1; j <= MAX_CONFIGURABLE_CONN; j++)
            {
                if(app_idx == tnicon_p->connection[j].app_idx)
                {
                    tnicon_p->connection[j].consec_auth_failure_cnt = 0;
                }
            }
        }
    }
}
