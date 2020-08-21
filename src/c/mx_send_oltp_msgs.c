static const char *fileid = "";

/*
 * ===[tni_build_oltp_msgs.c]=============================================
 *
 * Description:
 *
 * Functions to send oltp messages into to the appropriate client's
 * connection(s).
 *
 * Functions:
 *
 * Send_Oltp_Resp_Msg
 * Send_Oltp_Unso_Msg
 * Send_Oltp_Bro_Msg
 * Sned_Oltp_Rqst_Msg
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

#include <stdio.h>

#include "includes_mbuf.h"

/* [Send_Oltp_Resp_Msg]
 *
 * Summary:
 *
 * Send_Oltp_Resp_Msg ()
 *
 * Description:
 *
 * This function takes an oltp response message and sends it to be encoded
 * in the TNI protocol.  The resulting message is then insert into a Terminal
 * Server Data pdu.
 *
 * Returns Values:
 *
 * None.
 *
 */

void
Send_Oltp_Resp_Msg (long int term_server_id,
                    long int msg_len,
                    unsigned char *msg)
{
    int                 conn_num = 0;
    int                 resp_mbuf_len = 0;

    struct mbuf        *resp_mbuf;         /* pointer to mbuf holding terminal*/
                                           /* message data, excluding TNI     */
                                           /* protocol header                 */
    err_string = null_err_string;

    AppMsgDbg (term_server_id,
               "App Response",
               msg_len,
               msg);

    Build_Oltp_Resp_Msg (term_server_id,
                         msg_len,
                         msg);

    return;
}

/* [Send_Oltp_Unso_Msg]
 *
 * Summary:
 *
 * Send_Oltp_Unso_Msg ()
 *
 * Description:
 *
 * This function determines the OLTP message routing for each application and
 * then send the OLTP unsolicited message to the appropriate client 
 * connection(s).
 *
 * Returns Values:
 *
 * None.
 *
 */

void
Send_Oltp_Unso_Msg (long int term_server_id,
                    long int msg_len,
                    unsigned char *msg)
{
    unsigned char       unso_sent_on[MAX_CONN_PLUS_ONE];

    static unsigned char previous_unso_sender[MAX_CONN_PLUS_ONE];

    int                 alt_conn_num = 0;
    int                 app_idx = 0;
    int                 client_id = 0;
    int                 conn_idx = 0;
    int                 conn_num = 0;
    int                 first_client_id_sent = 0;
    int                 first_conn_num_sent = 0;
    int                 more_conns = 1;
    int                 term_stats_idx = -1;
    int                 tmp_conn_num = 0;
    int                 rc = P_SUCCESS;
    int                 unso_sender = -1;

    static int          init_unso_senders = 1;

    if (init_unso_senders == 1)
    {
         memset (previous_unso_sender, 0x00, sizeof(previous_unso_sender));
	 init_unso_senders = 0;
    }
    err_string = null_err_string;

    memset (unso_sent_on, 0x00, sizeof(unso_sent_on));

    AppMsgDbg (term_server_id,
               "App Unsolicited",
               msg_len,
               msg);

/*
**  Get the terminal's index to the terminal statistics global section
**  and verify it is within the proper range.
*/

#   if defined(PROSYS_ENV_PLATFORM)

        term_stats_idx = find_term_stats_idx (term_server_id);

#   else

        term_stats_idx = term_server_id;

#   endif

    if ((term_stats_idx <= 0) ||
        (term_stats_idx > max_terminal))
    {
        sprintf (err_string.par1, "%s", "Unsolicited");
        sprintf (err_string.par2, "%ld", term_server_id);
        sprintf (err_string.par3, "%s", "?");
        sprintf (err_string.par4, "%s", "bad statistics index");

        output_err ("Send_Oltp_Unso_Msg",
                    MI_TNI_NO_DELIVER,
                    MX_ERR_LVL_ERROR,
                    err_string);

        rc = P_FAILURE;
    }

    if (rc == P_SUCCESS)
    {

/*
**      Send unsolicited message to clients managed by application.
*/

        for (app_idx = 0; app_idx < MAX_APPS; app_idx++)
        {
            if (tnicon_p->app[app_idx].state == APP_DEFINED)
            {
                conn_num = Find_Conn_For_App (MSG_TYPE_OLTP,
                                              ENC_NONE,
                                              -1,
                                              tnicon_p->app[app_idx].name);

                if (conn_num != 0)
                {
                    switch (tnicon_p->app[app_idx].oltp_unso_routing)
                    {
                    case OLTP_ROUTE_ROUND_ROBIN:

                        Build_Oltp_Unso_Msg (conn_num,
                                             term_server_id,
                                             term_stats_idx,
                                             msg_len,
                                             msg);
                        break;

                    case OLTP_ROUTE_ALL_CLIENTS:

/*
** Clear the previuos unso sender state on any of the application's connections
** that are no longer PRIMARY.
*/
                        for (conn_idx = 0;
                             conn_idx < tnicon_p->app[app_idx].client_conn_cnt;
                             conn_idx++)
                        {
                            tmp_conn_num = tnicon_p->app[app_idx].client_conn[conn_idx];

                            if (tnicon_p->connection[tmp_conn_num].conn_state !=
                                CONN_PRIMARY)
                            {
                                previous_unso_sender[tmp_conn_num] = 0;
                            }
                        }

                        more_conns = 1;
                        first_conn_num_sent = conn_num;
/*
** Loop thru each of the application's connections.  If the connection does
** not have a redundant connection, then the unsolicited message will be
** sent on it.  Mark the connection has the unsolicited sender.  If the 
** connection has one of more redundant connection, then the connection
** marked as the unsolicited sender must be used to send the unsolicited
** message.  Why?   Because when game parameter blocks are sent as 
** unsolicited messages they must be sent in order.  Alternating the
** sending of unsolicited messages over redundant connections will
** not guarantee the game parameter block will be sent in order.
** Therefore, when redundant connections are present the one marked as the
** unsolicited sender is used to send all unsolicited messages.
** The unso_sent_on array is used to keep track of the connections the
** unsolicited message is sent over.  When an unsolicited message is sent
** over a connection the connection is marked (unso sent) in the unso_sent_on
** array.  In addition, all of that connection's redundant connections are
** also marked (unso sent) in the unso_sent_on array.  Marking the
** redundant connections prevents the unsolicited message from being sent
** across redundant and thereby being received twice by the terminal.
*/
                        while ((more_conns == 1) &&
                               (conn_num != 0))
                        {
                            if (unso_sent_on[conn_num] == 0)
                            {
                                unso_sent_on[conn_num] = 1;
                                unso_sender = -1;

                                tmp_conn_num = conn_num;
                                alt_conn_num = Find_Alternate_Connection (tmp_conn_num);

                                if (tmp_conn_num == alt_conn_num)
                                {
                                    unso_sender = tmp_conn_num;
                                }
                                else
                                {
                                    if (previous_unso_sender[tmp_conn_num] == 1)
                                    {
                                        unso_sender = tmp_conn_num;
                                    }

                                    while ((alt_conn_num != tmp_conn_num) &&
                                           (alt_conn_num != conn_num))
                                    {
                                        if (previous_unso_sender[alt_conn_num] == 1)
                                        {
                                            if (unso_sender != -1)
                                            {
                                                previous_unso_sender[unso_sender] = 0;
                                                printf("More than one previous unso sender found!\n");
                                            }
					    unso_sender = alt_conn_num;
                                        }
                                        unso_sent_on[alt_conn_num] = 1;
                                        tmp_conn_num = alt_conn_num;
                                        alt_conn_num = Find_Alternate_Connection (tmp_conn_num);
                                    }

                                    if (unso_sender == -1)
                                    {
                                        unso_sender = conn_num;
                                    }
                                }
                                Build_Oltp_Unso_Msg (unso_sender,
                                                     term_server_id,
                                                     term_stats_idx,
                                                     msg_len,
                                                     msg);

                                previous_unso_sender[unso_sender] = 1;
                            }
                            conn_num = Find_Conn_For_App (MSG_TYPE_OLTP,
                                                          ENC_NONE,
                                                          -1,
                                                          tnicon_p->app[app_idx].name);
                            if ((conn_num == 0) ||
                                (conn_num == first_conn_num_sent))
                            {
                                more_conns = 0;
                            }
                        }
                        break;
                    }
                }
                else
                {
                    if ((tnicon_p->app[app_idx].oltp_unso_routing !=
                         OLTP_ROUTE_NONE) &&
			(tnicon_p->unso_failure_notify2 == MX_ENABLED))
                    {
                        sprintf (err_string.par1,"%ld",
                                 term_server_id);

                        sprintf (err_string.par2,"%s",
                                 tnicon_p->app[app_idx].name);

                        output_err ("Send_Oltp_Unso_Msg",
                                    MI_TNI_ERR_UNSO,
                                    MX_ERR_LVL_WARNING,
                                    err_string);
                    }
                }

             }
        }

/*
**      Send unsolicited message to clients managed by client id. 
*/

        client_id = Find_Conn_For_Oltp_Unso (&conn_num);
        first_client_id_sent = client_id;

        while (client_id != 0)
        {
            if (conn_num != 0)
            {
                Build_Oltp_Unso_Msg (conn_num,
                                     term_server_id,
                                     term_stats_idx,
                                     msg_len,
                                     msg);
            }
            else
            {
	        if (tnicon_p->unso_failure_notify1 == MX_ENABLED)
		{
                    sprintf(err_string.par1,"%s","unsolicited");
                    sprintf(err_string.par2,"%ld",term_server_id);
                    sprintf(err_string.par3,"%d",conn_num);

                    if (tnicon_p->connection[conn_num].messaging_type ==
                        MSG_TYPE_ES_RPC)
                    {
                        sprintf(err_string.par4,
                                "App %s",
                                tnicon_p->connection[conn_num].application_name);
                    }
                    else
                    {
                        sprintf(err_string.par4,
                                "Client Id %d",
                                client_id);
                    }

                    output_err("Send_Oltp_Unso_Msg",
                               MI_TNI_NO_DELIVER,
                               MX_ERR_LVL_WARNING,
                               err_string);
	        }
            }

            client_id = Find_Conn_For_Oltp_Unso (&conn_num);

            if (client_id == first_client_id_sent)
            {
                client_id = 0;
            }
        }
    }

    return;
}

/* [Send_Oltp_Bro_Msg]
 *
 * Summary:
 *
 * Send_Oltp_Bro_Msg ()
 *
 * Description:
 *
 * This function determines the OLTP message routing for each application and
 * then send the OLTP broadbast message to the appropriate client connection(s).
 *
 * Returns Values:
 *
 * None.
 *
 */

void
Send_Oltp_Bro_Msg (long int term_server_id,
                   long int msg_len,
                   unsigned char *msg)
{
    unsigned char       bro_sent_on[MAX_CONN_PLUS_ONE];

    static unsigned char previous_bro_sender[MAX_CONN_PLUS_ONE];

    int                 alt_conn_num = 0;
    int                 app_idx = 0;
    int                 client_id = 0;
    int                 conn_idx = 0;
    int                 conn_num = 0;
    int                 first_client_id_sent = 0;
    int                 first_conn_num_sent = 0;
    int                 more_conns = 1;
    int                 tmp_conn_num = 0;
    int                 bro_sender = -1;

    static int          init_bro_senders = 1;

    if (init_bro_senders == 1)
    {
         memset (previous_bro_sender, 0x00, sizeof(previous_bro_sender));
         init_bro_senders = 0;
    }

    err_string = null_err_string;

    memset (bro_sent_on, 0x00, sizeof(bro_sent_on));

    AppMsgDbg (term_server_id,
               "App Broadcast",
               msg_len,
               msg);

/*
**  Send broadcast message to clients managed by application.
*/

    for (app_idx = 0; app_idx < MAX_APPS; app_idx++)
    {
        if (tnicon_p->app[app_idx].state == APP_DEFINED)
        {
            conn_num = Find_Conn_For_App (MSG_TYPE_OLTP,
                                          ENC_NONE,
                                          -1,
                                          tnicon_p->app[app_idx].name);

            if (conn_num != 0)
            {
                switch (tnicon_p->app[app_idx].oltp_unso_routing)
                {
                case OLTP_ROUTE_ROUND_ROBIN:

                    Build_Oltp_Bro_Msg (conn_num,
                                        term_server_id,
                                        msg_len,
                                        msg);
                    break;

                case OLTP_ROUTE_ALL_CLIENTS:
/*
** Clear the previuos bro sender state on any of the application's connections
** that are no longer PRIMARY.
*/
                    for (conn_idx = 0;
                         conn_idx < tnicon_p->app[app_idx].client_conn_cnt;
                         conn_idx++)
                    {
                        tmp_conn_num = tnicon_p->app[app_idx].client_conn[conn_idx];

                        if (tnicon_p->connection[tmp_conn_num].conn_state !=
                            CONN_PRIMARY)
                        {
                            previous_bro_sender[tmp_conn_num] = 0;
                        }
                    }

                    more_conns = 1;
                    first_conn_num_sent = conn_num;

/*
** Loop thru each of the application's connections.  If the connection does
** not have a redundant connection, then the broadcast message will be
** sent on it.  Mark the connection has the broadcast sender.  If the 
** connection has one of more redundant connection, then the connection
** marked as the broadcast sender must be used to send the broadcast
** message.  Why?   Because when game parameter blocks are sent as 
** broadcast messages they must be sent in order.  Alternating the
** sending of broadcast messages over redundant connections will
** not guarantee the game parameter block will be sent in order.
** Therefore, when redundant connections are present the one marked as the
** broadcast sender is used to send all broadcast messages.
** The bro_sent_on array is used to keep track of the connections the
** broadcast message is sent over.  When an broadcast message is sent
** over a connection the connection is marked (bro sent) in the bro_sent_on
** array.  In addition, all of that connection's redundant connections are
** also marked (bro sent) in the bro_sent_on array.  Marking the
** redundant connections prevents the broadcast message from being sent
** across redundant and thereby being received twice by the terminal.
*/

                    while ((more_conns == 1) &&
                           (conn_num != 0))
                    {
                        if (bro_sent_on[conn_num] == 0)
                        {
                            bro_sent_on[conn_num] = 1;
                            bro_sender = -1;

                            tmp_conn_num = conn_num;
                            alt_conn_num = Find_Alternate_Connection (tmp_conn_num);

                            if (tmp_conn_num == alt_conn_num)
                            {
                                bro_sender = tmp_conn_num;
                            }
                            else
                            {
                                if (previous_bro_sender[tmp_conn_num] == 1)
                                {
                                    bro_sender = tmp_conn_num;
                                }

                                while ((alt_conn_num != tmp_conn_num) &&
                                       (alt_conn_num != conn_num))
                                {
                                    if (previous_bro_sender[alt_conn_num] == 1)
                                    {
                                        if (bro_sender != -1)
                                        {
                                            previous_bro_sender[bro_sender] = 0;
                                            printf("More than one previous bro sender found!\n");
                                        }
					bro_sender = alt_conn_num;
                                    }
                                    bro_sent_on[alt_conn_num] = 1;
                                    tmp_conn_num = alt_conn_num;
                                    alt_conn_num = Find_Alternate_Connection (tmp_conn_num);
                                }

                                if (bro_sender == -1)
                                {
                                    bro_sender = conn_num;
                                }
                            }
                            Build_Oltp_Bro_Msg (bro_sender,
                                                term_server_id,
                                                msg_len,
                                                msg);

                            previous_bro_sender[bro_sender] = 1;
                        }
                        conn_num = Find_Conn_For_App (MSG_TYPE_OLTP,
                                                      ENC_NONE,
                                                      -1,
                                                      tnicon_p->app[app_idx].name);

                        if ((conn_num == 0) ||
                            (conn_num == first_conn_num_sent))
                        {
                            more_conns = 0;
                        }
                    }
                    break;
                }
            }
            else
            {
                if ((tnicon_p->app[app_idx].oltp_unso_routing !=
                     OLTP_ROUTE_NONE) &&
		    (tnicon_p->unso_failure_notify2 == MX_ENABLED))
                {
                    sprintf (err_string.par1,"%s",
                             tnicon_p->app[app_idx].name);

                    output_err ("Send_Oltp_Bro_Msg",
                                MI_TNI_ERR_BRO,
                                MX_ERR_LVL_WARNING,
                                err_string);
                }
            }

        }
    }

/*
**  Send broadcast message to clients managed by client id.
*/

    client_id = Find_Conn_For_Oltp_Unso (&conn_num);
    first_client_id_sent = client_id;

    while (client_id != 0)
    {
        if (conn_num != 0)
        {
            Build_Oltp_Bro_Msg (conn_num,
                                 term_server_id,
                                 msg_len,
                                 msg);
        }
        else
        {
	    if (tnicon_p->unso_failure_notify1 == MX_ENABLED)
	    {
                sprintf(err_string.par1,"%s","broadcast");
                sprintf(err_string.par2,"%ld",term_server_id);
                sprintf(err_string.par3,"%d",conn_num);

                if (tnicon_p->connection[conn_num].messaging_type ==
                    MSG_TYPE_ES_RPC)
                {
                    sprintf(err_string.par4,
                            "App %s",
                            tnicon_p->connection[conn_num].application_name);
                }
                else
                {
                    sprintf(err_string.par4,
                            "Client Id %d",
                            client_id);
                }

                output_err("Send_Oltp_Bro_Msg",
                           MI_TNI_NO_DELIVER,
                           MX_ERR_LVL_WARNING,
                           err_string);
	    }
        }

        client_id = Find_Conn_For_Oltp_Unso (&conn_num);

        if (client_id == first_client_id_sent)
        {
            client_id = 0;
        }
    }

    return;
}

/* [Send_Oltp_Rqst_Msg]
 *
 * Summary:
 *
 * Send_Oltp_Rqst_Msg ()
 *
 * Description:
 *
 * This function determines the proper connection to send the Oltp request
 * message to.  The message is then sent it to be encoded in the TNI 
 * protocol.  The resulting message is then insert into a Terminal
 * Server Data pdu.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

byte_4
Send_Oltp_Rqst_Msg (byte_4 terminal_server_id,
                    byte_1 *app_name,
                    byte_4 msg_len,
                    ubyte_1 *msg,
                    byte_4 *result_code,
                    byte_1 *result_text)
{
    ubyte_1             rqst_sent_on[MAX_CONN_PLUS_ONE];
    byte_1              loc_app_name[MXSRV_MAX_APP_NAME_LEN + 1];
    byte_4              alt_conn_num = 0;
    byte_4              app_idx = -1;
    byte_4              conn_num;
    byte_4              first_conn_num_sent = 0;
    byte_4              more_conns = 1;
    byte_4              rc = P_SUCCESS;
    byte_4              term_stats_idx = -1;
    byte_4              tmp_conn_num = 0;


    /* initialize local variables */
    memset (rqst_sent_on, 0x00, sizeof(rqst_sent_on));
    memset (loc_app_name, 0x00, sizeof(loc_app_name));
    
    /* If destination application not specified, then use default application */
    
    if (app_name[0] == '\0')
    {
        strncpy(loc_app_name,
	        tnicon_p->def_term_rqst_dest,
                sizeof (loc_app_name)-1);
    }
    else
    {
        strncpy(loc_app_name, app_name, sizeof (loc_app_name)-1);
    }

    /* Verify the destination application is defined */

    app_idx = Find_Application_Number (loc_app_name);

    if (-1 == app_idx)
    {
        *result_code = MXSRV_RESP_ERR_APP_NOTDEF;
        sprintf (result_text,
                 "Unable to send Terminal request, application %s is undefined",
                 loc_app_name);
        rc = P_FAILURE;
    }

    /* Compute and verify terminal statistics index */

    if (P_SUCCESS == rc)
    {
#       if defined(PROSYS_ENV_PLATFORM)

            term_stats_idx = find_term_stats_idx(terminal_server_id);

#       else

            term_stats_idx = terminal_server_id;

#       endif

        if ((term_stats_idx <= 0) || (term_stats_idx > max_terminal))
        {
            *result_code = MXSRV_RESP_ERR_STATS_IDX;
            sprintf (result_text,
                     "Unable to send Terminal request to %d, bad statistics index",
                     terminal_server_id);
            rc = P_FAILURE;
        }
    }

    if (P_SUCCESS == rc)
    {
        AppMsgDbg (terminal_server_id,
                   "App Request",
                   msg_len,
                   msg);

        conn_num = Find_Conn_For_App (MSG_TYPE_OLTP,
                                      ENC_NONE,
                                      app_idx,
                                      NULL);

        if ((conn_num != 0) &&
            (tnicon_p->connection[conn_num].tni_proto_ver >= TNI_VERSION_23))
        {
            switch (tnicon_p->app[app_idx].oltp_rqst_routing)
            {
            case OLTP_ROUTE_ROUND_ROBIN:
                *result_code = Build_Oltp_Rqst_Msg (conn_num,
                                                    terminal_server_id,
                                                    term_stats_idx,
                                                    msg_len,
                                                    msg);

                switch (*result_code)
                {
                case MXSRV_RESP_ERR_NOBUILD:
                    sprintf (result_text,
                             "Unable to send Terminal request, mbuf allocation failure");
                    rc = P_FAILURE;
                    break;

                case MXSRV_RESP_ERR_MSG_TOBIG:
                    sprintf (result_text,
                             "Unable to send Terminal request, message exceeds maximum PDU size");
                    rc = P_FAILURE;
                    break;
                }
                break;

            case OLTP_ROUTE_ALL_CLIENTS:
                first_conn_num_sent = conn_num;
                while ((more_conns == 1) && (conn_num != 0) && (P_SUCCESS == rc))
                {
                    if (rqst_sent_on[conn_num] == 0)
                    {
                        *result_code = Build_Oltp_Rqst_Msg (conn_num,
                                                            terminal_server_id,
                                                            term_stats_idx,
                                                            msg_len,
                                                            msg);

                        switch (*result_code)
                        {
                        case MXSRV_RESP_ERR_NOBUILD:
                            sprintf (result_text,
                                     "Unable to send Terminal request, mbuf allocation failure");
                            rc = P_FAILURE;
                            break;

                        case MXSRV_RESP_ERR_MSG_TOBIG:
                            sprintf (result_text,
                                     "Unable to send Terminal request, message exceeds maximum PDU size");
                            rc = P_FAILURE;
                            break;
                        }
                        if (P_SUCCESS == rc)
                        {
                            rqst_sent_on[conn_num] = 1;

                            tmp_conn_num = conn_num;
                            alt_conn_num = Find_Alternate_Connection (tmp_conn_num);

                            while ((alt_conn_num != tmp_conn_num) &&
                                   (alt_conn_num != conn_num))
                            {
                                rqst_sent_on[alt_conn_num] = 1;
                                tmp_conn_num = alt_conn_num;
                                alt_conn_num = Find_Alternate_Connection (tmp_conn_num);
                            }
                        }
                    }
                    conn_num = Find_Conn_For_App (MSG_TYPE_OLTP,
                                                  ENC_NONE,
                                                  app_idx,
                                                  NULL);

                    if ((conn_num == 0) ||
                        (conn_num == first_conn_num_sent))
                    {
                        more_conns = 0;
                    }
                }
                break;
            default:
                *result_code = MXSRV_RESP_ERR_NOCONN;
                sprintf (result_text,
                         "No connection available: application %s not configured to route Terminal requests",
                         loc_app_name);
                rc = P_FAILURE;
	        break;
            }
        }
        else
        {
            *result_code = MXSRV_RESP_ERR_NOCONN;
            sprintf (result_text,
                     "No connection available to send Terminal request for application %s",
                     loc_app_name);
            rc = P_FAILURE;
        } 
    }

    return(rc);
}
