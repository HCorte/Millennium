static const char *fileid = "";

/*
 * ===[tni_build_oltp_msgs.c]=============================================
 *
 * Description:
 *
 * Functions to encode oltp response and insolicited messages into 
 * Terminal Server Data pdu.
 *
 * Functions:
 *
 * Build_Oltp_Msg
 * Build_Oltp_Resp_Msg
 * Build_Oltp_Unso_Msg
 * Build_Oltp_Bro_Msg
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

/* [Build_Oltp_Msg]
 *
 * Summary:
 *
 * Build_Oltp_Msg (int conn,
 *                 int delivery_mode,
 *                 long int term_server_id,
 *                 long int msg_len,
 *                 unsigned char *msg,
 *                 char *correlation_tag,
 *                 int *msg_mbuf_len)
 *
 *
 * Input Arguments:
 * 
 * conn            -  Connection
 * delivery_mode   -  Delivery Mode
 * term_server_id  -  Terminal server id
 * msg_len         -  Message Length
 * msg             -  Pointer to the built message
 * correlation_tag -  Pointer to correlation tag 
 * msg_mbuf_len    -  Pointer to the Lenght of message built
 *
 * Description:
 *
 * This function builds a mbuf chain consisting of the terminal message header
 * and the terminal message data.
 *
 * Returns Values:
 *
 * Pointer to the mbuf chain.  NULL is return when an error is incountered.
 *
 */

struct mbuf *
Build_Oltp_Msg (int conn,
                int delivery_mode,
                long int term_server_id,
                long int msg_len,
                unsigned char *msg,
                char *correlation_tag,
                int *msg_mbuf_len)
{
    short int           tot_msg_len;
    long int            term_client_id;
    long int            app_msg_len;       /* length of the message in        */
                                           /* characters, excluding TNI       */
                                           /* protocol header                 */

    unsigned char      *term_msg_ptr;      /* points to received terminal     */
    unsigned char       status_code;       /* status value returned from      */
                                           /* Valid_Term_Input.               */

    char                term_client_tag[MAX_TERM_CLIENT_TAG_LEN];

    struct mbuf        *headermbuf;        /* pointer to mbuf holding terminal*/
                                           /* message header, i.e. parameters */
    struct mbuf        *msgmbuf;           /* pointer to mbuf holding terminal*/
                                           /* message data, excluding TNI     */
                                           /* protocol header                 */
    err_string = null_err_string;

    status_code = Valid_Term_Input (term_server_id,
                                    delivery_mode,
                                    conn,
                                    &term_client_id,
                                    term_client_tag);

    switch( status_code )
    {

        case EXT_SUCCESS:

            app_msg_len  = msg_len;

#           if defined(PROSYS_ENV_ALL)

                if ((msg_len <= 0) || (msg_len > volcom_p->sys.term_meslen)) {

                    switch (delivery_mode)
                    {
                        case REQUEST:

                            sprintf(err_string.par1,"%s","Request");
                            break;

                        case RESPONSE:

                            sprintf(err_string.par1,"%s","Response");
                            break;

                        case UNSOLICITED:

                           sprintf(err_string.par1,"%s","Unsolicited");
                           break;

                        case BROADCAST:

                            sprintf(err_string.par1,"%s","Broadcast");
                            break;
                    }

                    sprintf (err_string.par2, "%ld", term_server_id);
                    sprintf (err_string.par3, "%ld", msg_len);

                    output_err ("Build_Oltp_Msg",
                                MI_TNI_INV_MSG_SIZE,
                                MX_ERR_LVL_ERROR,
                                err_string);

                   return (NULL);
                }

#           endif

            break;

        case EXT_BAD_CONVERSION:
        case EXT_INV_CONN:
        case EXT_NO_TERM_METHOD:
        case EXT_NO_CONN_AVAIL:

            return (NULL);
            break;
    }

    tnicon_p->oltp_msgs_from_game++;               /* total oltp messages     */
                                                   /* received from game      */

    /*
     *  Build the mbufs for the terminal message header and the terminal message
     */

    tot_msg_len = 0;
    term_msg_ptr = (unsigned char *)msg;               /* exclude header  */

    if ((headermbuf = Build_Term_Pdu_Header (term_server_id,
                                             term_client_id,
                                             term_client_tag,
                                             app_msg_len,
                                             delivery_mode,
                                             conn,
                                             correlation_tag)) == NULL)
    {
        return (NULL);
    }

    if ((msgmbuf = mb_alloc (app_msg_len)) == NULL)
    {
        mb_free_p(headermbuf);         /* free previously allocated mbuf  */

        sprintf (err_string.par1, "data mbuf");
        sprintf (err_string.par2, "%ld", app_msg_len);

        output_err ("Build_Oltp_Msg",
                    MI_TNI_MB_ALLOC,
                    MX_ERR_LVL_ERROR,
                    err_string);

        return (NULL);
    }

    memcpy(msgmbuf->m_data, term_msg_ptr, app_msg_len);
    msgmbuf->m_len = app_msg_len;

    if (msgmbuf != NULL)
    {
        mb_append (&headermbuf, msgmbuf);
    }
    else
    {
        printf ("Read_Px2xpro received NULL mbuf from mb_qdata, "
                "dropping message from terminal %ld \n", term_server_id);

        mb_free_p (headermbuf);
        return (NULL);
    }

    /*
     *  Get length of terminal message and its header
     */

    tot_msg_len = mb_len_p (headermbuf);

    if (tot_msg_len == 0)
    {
        printf ("Read_Px2xpro received zero length mbuf from mb_len_p, "
                "dropping message from terminal %ld \n", term_server_id);

        mb_free_p (headermbuf);          /* free previously allocated mbuf    */
        return (NULL);
    }

    *msg_mbuf_len = tot_msg_len;

    return (headermbuf);
}

/* [Build_Oltp_Resp_Msg]
 *
 * Summary:
 *
 * Build_Oltp_Resp_Msg (long int term_server_id,
 *                      long int msg_len,
 *                      unsigned char *msg)
 *
 * Input Arguments :
 *
 * term_server_id    - Terminal server id
 * msg_len           - Message Length
 * msg               - Pointer to the Response Message 
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
Build_Oltp_Resp_Msg (long int term_server_id,
                     long int msg_len,
                     unsigned char *msg)
{
    int                 conn_num = 0;
    int                 resp_mbuf_len = 0;
    int                 term_stats_app_idx = 0;
    int                 term_stats_idx = -1;

    struct mbuf        *resp_mbuf;         /* pointer to mbuf holding terminal*/
                                           /* message data, excluding TNI     */
                                           /* protocol header                 */
    TERM_STATS         *term_stats_p;      /* Pointer to the statistics       */
                                           /* area for each network terminal  */
    struct ERR_STRING   local_err_string;

    local_err_string = null_err_string;
    err_string = null_err_string;

/*
 *  Get the terminal's index to the terminal statistics global section 
 *  and verify it is within the proper range.
 */

#   if defined(PROSYS_ENV_PLATFORM)

        term_stats_idx = retrieve_term_stats_idx (term_server_id);

#   else

        term_stats_idx = term_server_id;

#   endif

    if ((term_stats_idx > 0) &&
        (term_stats_idx <= max_terminal))
    {
        Calc_Game_Delay (term_stats_idx);

/*
 *      Get pointer to this terminal's information in global section
 */

        term_stats_p = &tstats_p->term_stats;
        term_stats_p = &term_stats_p[term_stats_idx];

        term_stats_p->trans_in_game_cnt = 0;

        conn_num = term_stats_p->requesting_conn_num;
        term_stats_p->requesting_conn_num = 0;

        resp_mbuf = Build_Oltp_Msg (conn_num,
                                    RESPONSE,
                                    term_server_id,
                                    msg_len,
                                    msg,
                                    term_stats_p->correlation_tag,
                                    &resp_mbuf_len);

        memset(term_stats_p->correlation_tag,
               0x00,
               sizeof(term_stats_p->correlation_tag));

        if (resp_mbuf != NULL)
        {
            if (Insert_Msg_In_Server_Data_Pdu (conn_num,
                                               resp_mbuf_len,
                                               RESPONSE,
                                               P_FALSE,
                                               resp_mbuf) == P_FAILURE)
            {
                sprintf (local_err_string.par1, "%s", "Response");
                sprintf (local_err_string.par2, "%ld", term_server_id);
                sprintf (local_err_string.par3, "%d", resp_mbuf_len +
                         Calc_Pdu_Hdr_Size(TERMINAL_SERVER_DATA_PDU));

                output_err ("Build_Oltp_Resp_Msg",
                            MI_TNI_INV_MSG_SIZE,
                            MX_ERR_LVL_ERROR,
                            local_err_string);
            }
            else
            {
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

                term_stats_p->app_response_tot[term_stats_app_idx]++;
            }
        }
    }
    else
    {
        sprintf (err_string.par1, "%s", "Response");
        sprintf (err_string.par2, "%ld", term_server_id);
        sprintf (err_string.par3, "%s", "?");
        sprintf (err_string.par4, "%s", "bad stats idx");

        output_err ("Build_Oltp_Resp_Msg",
                    MI_TNI_NO_DELIVER,
                    MX_ERR_LVL_ERROR,
                    err_string);
    }

    return;
}

/* [Build_Oltp_Unso_Msg]
 *
 * Summary:
 *
 * Build_Oltp_Unso_Msg (int conn_num,
 *                      long int term_server_id,
 *                      int term_stats_idx,
 *                      long int msg_len,
 *                      unsigned char *msg)
 * 
 * Input Arguments :
 *
 * conn_num           - Connection number
 * terminal_server_id - Terminal Server ID
 * int term_stats_idx - Terminal statistics index
 * msg_len            - Message Length
 * msg                - Pointer unsolicited message data
 *  
 *
 * Description:
 *
 * This function takes an oltp unsolicited message and sends it to be encoded
 * in the TNI protocol.  The resulting message is then insert into a Terminal
 * Server Data pdu.
 *
 * Returns Values:
 *
 * None.
 *
 */

void
Build_Oltp_Unso_Msg (int conn_num,
                     long int term_server_id,
                     int term_stats_idx,
                     long int msg_len,
                     unsigned char *msg)
{
    int                 app_idx = 0;
    int                 app_num = 0;
    int                 term_stats_app_idx = 0;
    int                 unso_mbuf_len = 0;

    struct mbuf        *unso_mbuf;         /* pointer to mbuf holding terminal*/
                                           /* message data, excluding TNI     */
                                           /* protocol header                 */

    TERM_STATS         *term_stats_p;      /* Pointer to the statistics       */
                                           /* area for each network terminal  */
    struct ERR_STRING   local_err_string;

    local_err_string = null_err_string;
/*
**  Get pointer to this terminal's information in global section
*/

    term_stats_p = &tstats_p->term_stats;
    term_stats_p = &term_stats_p[term_stats_idx];

    unso_mbuf = Build_Oltp_Msg (conn_num,
                                UNSOLICITED,
                                term_server_id,
                                msg_len,
                                msg,
                                NULL,
                                &unso_mbuf_len);

    if (unso_mbuf != NULL)
    {
        if (Insert_Msg_In_Server_Data_Pdu (conn_num,
                                           unso_mbuf_len,
                                           UNSOLICITED,
                                           P_FALSE,
                                           unso_mbuf) == P_FAILURE)
        {
            sprintf (local_err_string.par1, "%s", "Unsolicited");
            sprintf (local_err_string.par2, "%ld", term_server_id);
            sprintf (local_err_string.par3, "%d", unso_mbuf_len +
                     Calc_Pdu_Hdr_Size(TERMINAL_SERVER_DATA_PDU));

            output_err ("Build_Oltp_Unso_Msg",
                        MI_TNI_INV_MSG_SIZE,
                        MX_ERR_LVL_ERROR,
                        local_err_string);
        }
        else
        {
            if (tnicon_p->connection[conn_num].managed_by ==
                CONNS_APP_NAME)
            {
               term_stats_app_idx = 
                    tnicon_p->connection[conn_num].app_idx;
            }
            else if (tnicon_p->connection[conn_num].managed_by ==
                     CONNS_CLIENT_ID)
            {
                term_stats_app_idx = MAX_APPS +
                    tnicon_p->connection[conn_num].client_id_idx;
            }

            term_stats_p->app_unso_tot[term_stats_app_idx]++;
        }
    }
}

/* [Build_Oltp_Bro_Msg]
 *
 * Summary:
 *
 * Build_Oltp_Bro_Msg ( int conn_num,
 *                       long int term_server_id,
 *                       long int msg_len,
 *                       unsigned char *msg)
 *
 * Input Arguments:
 *
 * conn_num       - Connection number
 * term_server_id - Terminal Server ID
 * msg_len        - Message Lenght
 * msg            - Pointer to Broadcast Message data
 *
 * Description:
 *
 * This function takes an oltp broadcast message and sends it to be encoded
 * in the TNI protocol.  The resulting message is then insert into a Terminal
 * Server Data pdu.
 *
 * Returns Values:
 *
 * None.
 *
 */

void
Build_Oltp_Bro_Msg (int conn_num,
                     long int term_server_id,
                     long int msg_len,
                     unsigned char *msg)
{
    int                 app_idx = 0;
    int                 app_num = 0;

    int                 bro_mbuf_len = 0;

    struct mbuf        *bro_mbuf;          /* pointer to mbuf holding terminal*/
                                           /* message data, excluding TNI     */
                                           /* protocol header                 */
    struct ERR_STRING   local_err_string;

    local_err_string = null_err_string;

    bro_mbuf = Build_Oltp_Msg (conn_num,
                               BROADCAST,
                               term_server_id,
                               msg_len,
                               msg,
                               NULL,
                               &bro_mbuf_len);

    if (bro_mbuf != NULL)
    {
        if (Insert_Msg_In_Server_Data_Pdu (conn_num,
                                           bro_mbuf_len,
                                           BROADCAST,
                                           P_FALSE,
                                           bro_mbuf) == P_FAILURE)
        {
            sprintf (local_err_string.par1, "%s", "Broadcast");
            sprintf (local_err_string.par2, "%ld", term_server_id);
            sprintf (local_err_string.par3, "%d", bro_mbuf_len +
                     Calc_Pdu_Hdr_Size(TERMINAL_SERVER_DATA_PDU));

            output_err ("Build_Oltp_Bro_Msg",
                        MI_TNI_INV_MSG_SIZE,
                        MX_ERR_LVL_ERROR,
                        local_err_string);
        }
    }
}

/* [Build_Oltp_Rqst_Msg]
 *
 * Summary:
 *
 * Build_Oltp_Rqst_Msg (byte_4 conn_num,
 *                      byte_4 term_server_id,
 *                      byte_4 msg_len,
 *                      ubyte_1 *msg)
 * 
 * Input Arguments :
 *
 * conn_num           - Connection number
 * terminal_server_id - Terminal Server ID
 * term_stats_idx     - Terminal statistics index
 * msg_len            - Message Length
 * msg                - Pointer request message data
 *  
 *
 * Description:
 *
 * This function takes an oltp request message and sends it to be encoded
 * in the TNI protocol.  The resulting message is then insert into a Terminal
 * Server Data pdu.
 *
 * Returns Values:
 *
 * P_SUCCESS, MXSRV_RESP_ERR_NOBUILD, MXSRV_RESP_ERR_MSG_TOBIG
 *
 */

byte_4
Build_Oltp_Rqst_Msg (byte_4 conn_num,
                     byte_4 terminal_server_id,
                     byte_4 term_stats_idx,
                     byte_4 msg_len,
                     ubyte_1 *msg)
{
    byte_4              rc = P_SUCCESS;
    int                 rqst_mbuf_len = 0;
    byte_4              term_stats_app_idx = 0;

    struct ERR_STRING   local_err_string;
    struct mbuf        *rqst_mbuf;         /* pointer to mbuf holding terminal*/
                                           /* message data, excluding TNI     */
                                           /* protocol header                 */
    TERM_STATS         *term_stats_p;      /* Pointer to the statistics       */
                                           /* area for each network terminal  */

    local_err_string = null_err_string;

    rqst_mbuf = Build_Oltp_Msg (conn_num,
                                REQUEST,
                                terminal_server_id,
                                msg_len,
                                msg,
                                NULL,
                                &rqst_mbuf_len);

    if (rqst_mbuf != NULL)
    {
        if (Insert_Msg_In_Server_Data_Pdu (conn_num,
                                           rqst_mbuf_len,
                                           REQUEST,
                                           P_FALSE,
                                           rqst_mbuf) == P_FAILURE)
        {
            rc = MXSRV_RESP_ERR_MSG_TOBIG;

            sprintf (local_err_string.par1, "%s", "Request");
            sprintf (local_err_string.par2, "%d", terminal_server_id);
            sprintf (local_err_string.par3, "%d", rqst_mbuf_len +
                     Calc_Pdu_Hdr_Size(TERMINAL_SERVER_DATA_PDU));

            output_err ("Build_Oltp_Rqst_Msg",
                        MI_TNI_INV_MSG_SIZE,
                        MX_ERR_LVL_ERROR,
                        local_err_string);
        }
        else
        {
            term_stats_p = &tstats_p->term_stats;
            term_stats_p = &term_stats_p[term_stats_idx];

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
            term_stats_p->host_term_rqst_tot[term_stats_app_idx]++;
        }
    }
    else
    {
        rc = MXSRV_RESP_ERR_NOBUILD;
    }
    return(rc);
}
