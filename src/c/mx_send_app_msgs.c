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
/*====[MX_SEND_APP_MSGS.C]====================================================*/
/*                                                                            */
/* Purpose: These functions are used send the various types of terminal       */
/*          application messages.                                             */
/*                                                                            */
/* Functions:                                                                 */
/*          Send_App_Bro ( long int term_msg_len,                             */
/*                         char *app_msg_ptr)                                 */
/*                                                                            */
/*          Send_App_Rqst_Resp (int conn_num ,                                */
/*                              long int terminal_server_id                   */
/*                              long int terminal_client_id,                  */
/*                              char *terminal_client_tag,                    */
/*                              long int term_msg_len,                        */
/*                              struct PDU_STRUCT *pdu_struct,                */
/*                              long int length,                              */
/*                              long int offset,                              */
/*                              int msg_offset,                               */
/*                              int msg_type)                                 */
/*                                                                            */
/*          Send_App_Unso (unsigned long int term_net_id,                     */
/*                         long int term_host_id,                             */
/*                         long int term_msg_len,                             */
/*                         char *app_msg_ptr )                                */
/*                                                                            */
/*====[MX_SEND_APP_MSGS.C]====================================================*/
/*                                                                            */

#include <stdio.h>

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <sys/timeb.h>

#elif defined(XOS_VMS)

#   include <timeb.h>

#else

#   error - OS-specific logic not handled.

#endif

#include "includes.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Send_App_Bro (long int term_msg_len, char *app_msg_ptr)                    */
/*                                                                            */
/*                                                                            */
/* Purpose: This function adds the TNI protocol header to an application      */
/*          broadcast message and sends the resulting message to PX2XPRO      */
/*          using the m_broadcast function.                                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          term_msg_len     Application message length based upon terminal   */
/*                           message header                                   */
/*          app_msg_ptr      Pointer to application message                   */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: term_msg_len is nonzero.                                      */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Send_App_Bro (long int term_msg_len, char *app_msg_ptr) {

    err_string = null_err_string;

    sprintf(err_string.par1, "OLTP broadcast");

    output_err("Send_App_Bro",
               MI_TNI_NORECEIPT_SUP,
               MX_ERR_LVL_WARNING,
               err_string);

    return;
}

/*============================================================================*/
/*                                                                            */
/* Send_App_Rqst_Resp (int conn_num, long int terminal_server_id,             */
/*                     long int terminal_client_id, char *terminal_client_tag,*/
/*                     char *correlation_tag, long int term_msg_len,          */
/*                     struct PDU_STRUCT *pdu_struct, long int length,        */
/*                     long int offset, int msg_offset, int msg_type )        */
/*                                                                            */
/* Purpose: This function adds the TNI protocol header to an application      */
/*          response and sends the resulting message to PX2XPRO.              */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn_num         Connection Number                                */
/*          terminal_server_id                                                */
/*          terminal_client_id                                                */
/*          terminal_client_tag                                               */
/*          correlation_tag  Correlation tag sent from MX Client              */
/*          term_msg_len     Application message length based upon terminal   */
/*                           message header                                   */
/*          pdu_struct       Pointer to application message                   */
/*          lenght           Length of pdu                                    */
/*          offset           offset value                                     */
/*          msg_offset       Message offset value                             */
/*          msg_type         Request or Response                              */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: term_msg_len is nonzero.                                      */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void
Send_App_Rqst_Resp (int conn_num,
                    long int terminal_server_id, 
                    long int terminal_client_id, 
                    char *terminal_client_tag,
                    char *correlation_tag,
                    long int term_msg_len, 
                    struct PDU_STRUCT *pdu_struct,
                    long int length,
                    long int offset,
                    int msg_offset,
                    int msg_type)
{
    unsigned short      conn;              /* connection number               */

    char               *app_msg_ptr;

    int                 queue_msg_to_game = 0;
    int                 rc = P_SUCCESS;
    int                 term_id_parm_code;
    int                 term_stats_app_idx = 0;
    int                 term_stats_idx = -1;
    int                 status;
    int                 msg_undeliverable = 0;
    int                 terminal_unknown = 0;

    long int            terminal_id;       /* either the terminal server id   */
                                           /* or terminal client id dependend */
                                           /* on terminal identification      */
                                           /* method                          */

    struct timeb        elapsed_time;      /* Time trans has been in game     */
    
    TERM_STATS         *term_stats_p;      /* Pointer to the statistics       */
                                           /* area for each network terminal  */

#   if defined(GOLS_ENV_ALL)

        static int          dsp_comm_disabled_msg = 1;

#   endif

#   if defined(PROSYS_ENV_ALL)

        GM_MESDSC           mesdscp;
        GM_MESDSC           cntxtdsc;
        GM_MESDSC          *cntxtdsc_p;
        G_ERROR             err;
        P_CONTEXT           context;         /* context information */

#   endif

    err_string = null_err_string;

/* If, as part of a prior Host Paramter Request PDU, the host has chosen to   */
/* use terminal host ids to identify terminal messages then make sure that    */
/* the terminal message header has included the terminal host id.  If it's    */
/* missing, then send an Error PDU back to the host and drop the message.     */

    app_msg_ptr = (char *)                         
                  (&pdu_struct->msg_data[msg_offset]+length+offset);

    switch (tnicon_p->connection[conn_num].term_id_method)
    {
    
    case METH_TERM_CLIENT_ID:

        term_id_parm_code = TERM_CLIENT_ID;

        if (terminal_client_id != -1)
        {

#           if defined(PROSYS_ENV_ALL)

                terminal_id = terminal_client_id;

#               if !defined(PROSYS_ENV_PLATFORM)

                    terminal_server_id = get_term_net_id_from_term_ndx(terminal_client_id);

#               endif

#           endif

        }
        break;

    case METH_TERM_CLIENT_TAG:

        term_id_parm_code = TERM_CLIENT_TAG;
        break;

    case METH_TERM_SERVER_ID:

        term_id_parm_code = TERM_SERVER_ID;

        if (terminal_server_id != -1)
        {
            terminal_id = terminal_server_id;
        }
        break;

    }

    switch (msg_type)
    {
    case REQUEST:
        AppMsgDbg(terminal_id,
                  "App Request",
                  term_msg_len,
                  (unsigned char *)app_msg_ptr);
        break;
    case RESPONSE:
        AppMsgDbg(terminal_id,
                  "App Response",
                  term_msg_len,
                  (unsigned char *)app_msg_ptr);
        break;
    }

#   if defined(PROSYS_ENV_ALL)

/*
 *  Verify the terminal server id is known and for non-este platform
 *  systems verify it is within the proper range.
 */

        if (terminal_server_id <= 0)
        {
            terminal_unknown = 1;
            rc = P_FAILURE;
        }

#       if !defined(PROSYS_ENV_PLATFORM)

            if (rc == P_SUCCESS)
            {
                if (terminal_server_id != -1 )
                {
                    if ((terminal_server_id <= 0) ||
                        (terminal_server_id > volcom_p->sys.mxtrm))
                    {
                        terminal_unknown = 1;
                        rc = P_FAILURE;
                    }
                }
            }

#       endif

/*
 *  Verify the request message is within the proper range for the
 *  PRO:SYS system.
 */

        if (rc == P_SUCCESS)
        {
            if ((term_msg_len <= 0 ) ||
                (term_msg_len > volcom_p->sys.term_meslen))
            {
                switch (msg_type)
                {
                case REQUEST:
                    sprintf(err_string.par1,"%s", "Request");
                    break;
                case RESPONSE:
                    sprintf(err_string.par1,"%s", "Response");
                    break;
                }
                sprintf(err_string.par2,"%ld", terminal_id);
                sprintf(err_string.par3,"%ld", term_msg_len);

                output_err("Send_App_Rqst_Resp",
                           MI_TNI_INV_MSG_SIZE,
                           MX_ERR_LVL_ERROR,
                           err_string);

                LogPdu(pdu_struct, terminal_id, conn_num);

                msg_undeliverable = 1;
                rc = P_FAILURE;
            }
        }

/*
 *  Verify the terminal's index to the terminal statistics global section
 *  is valid.
 */

        if (rc == P_SUCCESS)
        {

#           if defined(PROSYS_ENV_PLATFORM)

                term_stats_idx = find_term_stats_idx(terminal_server_id);

#           else

                term_stats_idx = terminal_server_id;

#           endif

            if ((term_stats_idx <= 0) ||
                (term_stats_idx > volcom_p->sys.mxtrm))
            {
                msg_undeliverable = 1;
                rc = P_FAILURE;
            }
        }

/*
 *  Verify the MX Server is in the LIVE state before attempting
 *  to queue a request to GTMS.
 */

        if (rc == P_SUCCESS)
        {
            if (volcom_p->prod.state != LIVE)
            {
                msg_undeliverable = 1;
                rc = P_FAILURE;
            }
        }

/*
 *  Attempt to queue request message to GTMS.
 */

        if (rc == P_SUCCESS)
        {
            mesdscp.mesbufp = (unsigned char *)app_msg_ptr;
            mesdscp.meslen = (int)term_msg_len;
            mesdscp.mesbufsiz = (int)term_msg_len;

/*      Get pointer to this part of the global section */

            term_stats_p = &tstats_p->term_stats;

/*      Get pointer to this terminal's information in global section */

            term_stats_p = &term_stats_p[term_stats_idx];

/*
 *      Increment count of total number of application response messages
 *      MX has received from this application or client id destined for
 *      this terminal for the day.
 */

            if (tnicon_p->connection[conn_num].managed_by ==
                CONNS_APP_NAME)
            {
               term_stats_app_idx = tnicon_p->connection[conn_num].app_idx;
            }
            else if (tnicon_p->connection[conn_num].managed_by ==
                     CONNS_CLIENT_ID)
            {
                term_stats_app_idx = MAX_APPS +
                    tnicon_p->connection[conn_num].client_id_idx;
            }

            if (msg_type == REQUEST)
            {
                cntxtdsc_p = NULL;  /* Do not setup a context for requests */

                if (term_stats_p->trans_in_game_cnt != 0)
                {
                    if (term_stats_p->trans_in_game_cnt == 1)
                    {
                        /* Compute trans in game time */
                        subtimes(&current_time, 
                                 &term_stats_p->last_trans_sent_time,
                                 &elapsed_time);

			sprintf(err_string.par1, "%ld", terminal_id);	 
                        sprintf(err_string.par2, "%d", (int)app_msg_ptr[2]);
                        sprintf(err_string.par3,
                                "%ld.%d",
                                (long)elapsed_time.time,
                                elapsed_time.millitm);

                        output_err("Send_App_Rqst_Resp",
                                   MI_TNI_OUTSTAND_TRANS,
                                   MX_ERR_LVL_WARNING,
                                   err_string);
                    }

                    term_stats_p->trans_in_game_cnt++;

                    LogPdu(pdu_struct, terminal_id, conn_num);
                }
                else
                {
                    term_stats_p->trans_in_game_cnt = 1;
                    term_stats_p->requesting_conn_num = conn_num;

                    if (correlation_tag != NULL)
                    {
                        strncpy(term_stats_p->correlation_tag,
                                correlation_tag,
                                MAX_CORRELATION_TAG_LEN + 1);
                    }

                    term_stats_p->app_request_tot[term_stats_app_idx]++;
 
                    Start_Game_Delay_Timer(term_stats_idx);
                    queue_msg_to_game = 1;
                }
            }
            else if (msg_type == RESPONSE)
            {
                context.prodbuf_size = 0;
                context.hdr.nextfunc = p_null_reply;

                cntxtdsc_p = &cntxtdsc;
                cntxtdsc_p->mesbufp = &context;
                cntxtdsc_p->meslen = sizeof(context);

                term_stats_p->host_term_resp_tot[term_stats_app_idx]++;
                queue_msg_to_game = 1;
            }

            if (1 == queue_msg_to_game)
            {
#               if defined(PROSYS_ENV_PLATFORM)

                    gtms_trminp2(terminal_server_id, &mesdscp, cntxtdsc_p, &err);

#               else

                    gtms_trminp(terminal_server_id, &mesdscp, cntxtdsc_p, &err);

#               endif

                if (err.errflg)
                {
                    g_senderror(&err);
                    msg_undeliverable = 1;

                    switch (msg_type)
                    {
                    case REQUEST:
                        term_stats_p->trans_in_game_cnt  = 0;
                        term_stats_p->requesting_conn_num = 0;
                        break;
                    }
                }
                queue_msg_to_game = 0;
            }
        }

#   endif

#   if defined(GOLS_ENV_ALL)

        QUEUE_TO_GAME (&terminal_server_id,
                       &term_msg_len,
                       app_msg_ptr,
                       &status);

        switch (status)
        {
        case 1:

            switch (msg_type)
            {
            case REQUEST:
                Start_Game_Delay_Timer(terminal_server_id);

/*      Get pointer to this terminal's information in global section */

                term_stats_p = &tstats_p->term_stats;
                term_stats_p = &term_stats_p[terminal_server_id];

                term_stats_p->requesting_conn_num = conn_num;

                if (correlation_tag != NULL)
                {
                    strncpy(term_stats_p->correlation_tag,
                            correlation_tag,
                            MAX_CORRELATION_TAG_LEN + 1);
                }
                break;
            }
            break;

        case -1:

            sprintf(err_string.par1, "%d", terminal_server_id);
            sprintf(err_string.par2, "1");
            sprintf(err_string.par3, "%d", max_terminal);

            output_err("Send_App_Rqst_Resp",
                       ME_BAD_TER_RNG,
                       MX_ERR_LVL_ERROR,
                       err_string);

            terminal_unknown = 1;
            break;

        case -2:

            switch (msg_type)
            {
            case REQUEST:
                sprintf(err_string.par1, "%s", "Request");
                break;
            case RESPONSE:
                sprintf(err_string.par1, "%s", "Response");
                break;
            }
            sprintf(err_string.par2, "%d", terminal_server_id);
            sprintf(err_string.par3, "%d", term_msg_len);

            output_err("Send_App_Rqst_Resp",
                       MI_TNI_INV_MSG_SIZE,
                       MX_ERR_LVL_ERROR,
                       err_string);

            msg_undeliverable = 1;
            break;

        case -4:

            /* Compute trans in game time */
            term_stats_p = &tstats_p->term_stats;
            term_stats_p = &term_stats_p[terminal_server_id];

            subtimes(&current_time,
                     &term_stats_p->last_trans_sent_time,
                     &elapsed_time);
 
            sprintf(err_string.par1,
	            "%ld.%d",
		    (long)elapsed_time.time,
		    elapsed_time.millitm);
            sprintf(err_string.par2, "%d", terminal_server_id);
	    sprintf(err_string.par3,
	            "%2.2x%2.2x",
		    app_msg_ptr[0],
		    app_msg_ptr[1]);

            output_err("Send_App_Rqst_Resp",
                       MI_TNI_IN_GAME,
                       MX_ERR_LVL_WARNING,
                       err_string);
            break;

        case -5:

            sprintf(err_string.par1, "%d", terminal_server_id);

            output_err("Send_App_Rqst_Resp",
                       MI_TNI_NO_PROCOM,
                       MX_ERR_LVL_ERROR,
                       err_string);

            msg_undeliverable = 1;
            break;

        case -6:

            if (dsp_comm_disabled_msg)
            {
                dsp_comm_disabled_msg = 0;

                sprintf(err_string.par1, "X2X");

                output_err("Send_App_Rqst_Resp",
                           MI_TNI_COMM_WAIT,
                           MX_ERR_LVL_WARNING,
                           err_string);
            }
            msg_undeliverable = 1;
            break;
        }

#   endif

    if (terminal_unknown == 1)
    {
        LogPdu(pdu_struct, terminal_id, conn_num);

        rc = send_term_unknown_error (conn_num,
                                      terminal_server_id,
                                      terminal_client_id,
                                      terminal_client_tag,
                                      correlation_tag);
    }

    if (msg_undeliverable == 1)
    {
        LogPdu(pdu_struct, terminal_id, conn_num);

        rc = send_msg_undelivered_error (conn_num,
                                         terminal_server_id,
                                         terminal_client_id,
                                         terminal_client_tag,
                                         correlation_tag);
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Send_App_Unso (unsigned long int term_net_id,                              */
/*                long int term_host_id, long int term_msg_len,               */
/*                char *app_msg_ptr)                                          */
/*                                                                            */
/* Purpose: This function adds the TNI protocol header to an unsolicited      */
/*          application message and sends the resulting message to PX2XPRO.   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          term_net_id      Terminal's Network ID or ULONG_MAX if not in     */
/*                           terminal message header                          */
/*          term_host_id     Terminal's Host ID or -1 if not in terminal      */
/*                           message header                                   */
/*          term_msg_len     Application message length based upon terminal   */
/*                           message header                                   */
/*          app_msg_ptr      Pointer to application message                   */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: term_msg_len is nonzero.                                      */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Send_App_Unso (unsigned long int term_net_id,
                    long int term_host_id, long int term_msg_len, 
                    char *app_msg_ptr) {

    err_string = null_err_string;

    sprintf(err_string.par1, "OLTP unsolicited");

    output_err("Send_App_Unso",
               MI_TNI_NORECEIPT_SUP,
               MX_ERR_LVL_WARNING,
               err_string);

    return;
}
