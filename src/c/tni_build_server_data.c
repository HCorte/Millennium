static const char *fileid = "";

/*
 * ===[tni_build_server_data.c]===========================================
 *
 * Description:
 *
 * Function to insert a OLTP message into a Server Data Pdu.
 *
 * Functions:
 *
 * Insert_Msg_In_Server_Data_Pdu
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

#include "includes_mbuf.h"

/* local proto types */

void
Server_Data_Pdu_Msg_Append (byte_4 conn_num,
                            byte_4 msg_len,
                            byte_4 delivery_mode,
                            struct mbuf *msg_mbuf);

void
Server_Data_Pdu_Msg_Send (byte_4 conn_num,
                          byte_4 pdu_type,
                          byte_4 messaging_type,
                          byte_4 enc_msg_flag);

/* [Insert_Msg_In_Server_Data_Pdu]
 *
 * Summary:
 *
 * Insert_Msg_In_Server_Data_Pdu (int conn_num,
 *                                int msg_len,
 *                                int delivery_mode,
 *                                int enc_msg_flag,
 *                                struct mbuf *msg_mbuf))
 * Input Arguments:
 *
 * conn_num  - Connection number
 * msg_len   - Message Length
 * delivery_mode - Delicvery mode (used to collect statistics)
 * enc_msg_flag - message being sent is encrypted
 * msg_mbuf  - Pointer to Message Mbuf
 *
 * Description:
 *
 * This function inserts an OLTP message into the Server Data Pdu
 * currently being built.  If the Server Data Pdu has reached its
 * maximum size or maximum number of messages the Pdu is sent.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Insert_Msg_In_Server_Data_Pdu (int conn_num,
                               int msg_len,
                               int delivery_mode,
                               int enc_msg_flag,
                               struct mbuf *msg_mbuf)
{
    int                 app_num = 0;
    int                 messaging_type;
    int                 rc = P_SUCCESS;
    int                 server_data_pdu;
    int                 blockSize = 0;     /* current block size value        */
    int                 sendFlag = 0;      /* Server Data PDU send flag,      */
                                           /* to send PDU to client           */

    messaging_type = tnicon_p->connection[conn_num].messaging_type;

    switch (messaging_type)
    {
        case MSG_TYPE_OLTP:

            server_data_pdu = TERMINAL_SERVER_DATA_PDU;

            break;

        case MSG_TYPE_ES_RPC:

            server_data_pdu = RPC_SERVER_DATA_PDU;
            break;
    }

    /*  Check to see if this terminal message, its header, and the terminal data
     *  PDU header exceeds the maximum PDU size.  If so, return an error.
     */

    if ((msg_len + Calc_Pdu_Hdr_Size (server_data_pdu)) > 
         tnicon_p->connection[conn_num].max_pdu_size)
    {
        mb_free_p (msg_mbuf);
        rc = P_FAILURE;
    }

    /*
     *  At this point, msg_mbuf is a chain consisting of this
     *  message's message header followed by the actual message.
     *  At this point we have 3 possibilities 
     *  1. Current message would exceed max PDU size.  Send what we 
     *     currently have queued, excluding the current message, and use the 
     *     current message to start the next mbuf write chain. 
     *  2. Current message would cause us to hit the max number of 
     *     messages allowed in a single PDU.  Add the current message to the 
     *     mbuf write chain and sent the entire chain. 
     *  3. Current message doesn't force us to exceed max PDU length or 
     *     meet max number of messages.  Simply append this header/message pair 
     *     to the write chain. 
     * 
     *  Per the TNI Functional Spec, setting the max number of messages per PDU 
     *  to 0 inhibits blocking by number of messages per PDU, i.e. only block by 
     *  timer or total PDU length.  There is no explicit check for this case as 
     *  it becomes a combination of cases 1 and 3; either we will eventually 
     *  exceed the PDU size or the blocking timer will expire and the timer 
     *  function will send the data to the host. 
     */

    if (rc == P_SUCCESS)
    {
        if (enc_msg_flag == P_TRUE)
        {
            /* When on demand encryption is configured for the application
             * ensure that the encrypted message is sent as the only
             * message in the PDU.  If the current PDU has messages
             * (unencrypted), then send them before creating the new PDU
             * containing the encrytped message.
             */

            app_num = tnicon_p->connection[conn_num].app_idx;
            if (tnicon_p->app[app_num].es_rpc_enc_mode == ENC_ON_DEMAND)
            {
                if (totwritelen[conn_num] > 0)
                {
                    sendFlag = SEND_UNENC_AND_SEND_ENC;
                }
                else
                {
                    sendFlag = APPEND_AND_SEND;
                }
            }
        }

        if (sendFlag == 0)
        {
             blockSize = msg_len + totwritelen[conn_num] +
                         Calc_Pdu_Hdr_Size (server_data_pdu);

            if (blockSize > tnicon_p->connection[conn_num].max_pdu_size) 
            {
                sendFlag = SEND_IMMEDIATE;
            } 
            else if (((tothostmsgs[conn_num] + 1) ==
                      tnicon_p->connection[conn_num].max_messages_per_pdu) ||
                     (tnicon_p->connection[conn_num].max_messages_per_pdu == 1)) 
            {
                sendFlag = APPEND_AND_SEND;
            } 
            else
            {
                sendFlag = APPEND_ONLY;
            }
        }

        switch (sendFlag)
        {
            case SEND_IMMEDIATE:
                Server_Data_Pdu_Msg_Send (conn_num,
                                          server_data_pdu,
                                          messaging_type,
                                          enc_msg_flag);

                Server_Data_Pdu_Msg_Append (conn_num,
                                            msg_len,
                                            delivery_mode,
                                            msg_mbuf);
                break;

            case APPEND_AND_SEND:
                Server_Data_Pdu_Msg_Append (conn_num,
                                            msg_len,
                                            delivery_mode,
                                            msg_mbuf);

                Server_Data_Pdu_Msg_Send (conn_num,
                                          server_data_pdu,
                                          messaging_type,
                                          enc_msg_flag);
                break;

            case APPEND_ONLY:
                Server_Data_Pdu_Msg_Append (conn_num,
                                            msg_len,
                                            delivery_mode,
                                            msg_mbuf);
                break;

            case SEND_UNENC_AND_SEND_ENC:
                Server_Data_Pdu_Msg_Send (conn_num,
                                          server_data_pdu,
                                          messaging_type,
                                          P_FALSE);

                Server_Data_Pdu_Msg_Append (conn_num,
                                            msg_len,
                                            delivery_mode,
                                            msg_mbuf);

                Server_Data_Pdu_Msg_Send (conn_num,
                                          server_data_pdu,
                                          messaging_type,
                                          enc_msg_flag);
                break;
        }
    }
    return (rc); 
}

void
Server_Data_Pdu_Msg_Append (byte_4 conn_num,
                            byte_4 msg_len,
                            byte_4 delivery_mode,
                            struct mbuf *msg_mbuf)
{
    mb_append (&writechain[conn_num], msg_mbuf);
    totwritelen[conn_num] += msg_len;
    tothostmsgs[conn_num] += 1;

    switch(delivery_mode)
    {
        case REQUEST:
            tnicon_p->srv_data_rqsts_outstanding[conn_num]++;
            break;
        case RESPONSE:
            tnicon_p->srv_data_resps_outstanding[conn_num]++;
            break;
        case BROADCAST:
        case UNSOLICITED:
            tnicon_p->srv_data_unsos_outstanding[conn_num]++;
            break;
    }
    return;
}

void
Server_Data_Pdu_Msg_Send (byte_4 conn_num,
                          byte_4 pdu_type,
                          byte_4 messaging_type,
                          byte_4 enc_msg_flag)
{
    byte_4              act_conn;          /* Actual connection PDU was sent  */
    byte_4              send_error = 0;

    struct mbuf        *pdu = NULL;        /* pointer to mbuf chain containing*/
                                           /* entire Server Data PDU, only    */
                                           /* used if we're sending the PDU to*/
                                           /* the cleint from this function   */
    struct TNI_PARAM_PAIR params[2];       /* only the message count variable */
                                           /* data parameter is used for      */
                                           /* Terminal Data PDU               */

    params[0].param_code = NUMBER_MESSAGES;
    params[0].param_value = tothostmsgs[conn_num];
    params[1].param_code = 0;
    params[1].param_value = 0;

    pdu = Build_Pdu (conn_num,
                     pdu_type,
                     writechain[conn_num],
                     &params[0]);

    /*
     * Do not allow a PDU containing an encrypted message to be send on an
     * alternate connection upon a send failure.  The encryption key could
     * be different for the alternate connection.
     */

    if (enc_msg_flag == P_FALSE)
    {
        act_conn = Send_To_Conn (pdu, conn_num, CHKAUTH_ALT_CONN);
    }
    else
    {
        act_conn = Send_To_Conn (pdu, conn_num, CHKAUTH_NO_ALT_CONN);
    }

    if (act_conn == 0) 
    {
        send_error = 1;
    }
    else
    {
        tnicon_p->srv_data_rqst_msgs_cnt[act_conn] +=
                  tnicon_p->srv_data_rqsts_outstanding[conn_num];
        tnicon_p->srv_data_resp_msgs_cnt[act_conn] +=
                  tnicon_p->srv_data_resps_outstanding[conn_num];
        tnicon_p->srv_data_unso_msgs_cnt[act_conn] +=
                  tnicon_p->srv_data_unsos_outstanding[conn_num];
            
        tnicon_p->srv_data_rqst_msgs_cnt_tot[act_conn] +=
                  tnicon_p->srv_data_rqsts_outstanding[conn_num];
        tnicon_p->srv_data_resp_msgs_cnt_tot[act_conn] +=
                  tnicon_p->srv_data_resps_outstanding[conn_num];
        tnicon_p->srv_data_unso_msgs_cnt_tot[act_conn] +=
                  tnicon_p->srv_data_unsos_outstanding[conn_num];

        switch (messaging_type)
        {
            case MSG_TYPE_OLTP:

                tnicon_p->oltp_srv_data_cnt_tot[act_conn]++;
                break;

            case MSG_TYPE_ES_RPC:

                tnicon_p->rpc_srv_data_cnt_tot[act_conn]++;

                /* Accumulate requests and responses over all RPC connections */

                tnicon_p->tot_rpc_rqsts_sent += 
                          tnicon_p->srv_data_rqsts_outstanding[conn_num];
                tnicon_p->num_rpc_rqsts_sent +=
                          tnicon_p->srv_data_rqsts_outstanding[conn_num];

                tnicon_p->tot_rpc_resps_sent += 
                          tnicon_p->srv_data_resps_outstanding[conn_num];
                tnicon_p->num_rpc_resps_sent +=
                          tnicon_p->srv_data_resps_outstanding[conn_num];
                break;
        }
    }

    if (send_error)
    {
        /*
         *  update the lost server data messages counter totals for
         *  statistics interval and for the day.
         */

        tnicon_p->srv_data_rqst_msgs_lost[conn_num] +=
                  tnicon_p->srv_data_rqsts_outstanding[conn_num];
        tnicon_p->srv_data_resp_msgs_lost[conn_num] +=
                  tnicon_p->srv_data_resps_outstanding[conn_num];
        tnicon_p->srv_data_unso_msgs_lost[conn_num] +=
                  tnicon_p->srv_data_unsos_outstanding[conn_num];

        tnicon_p->srv_data_rqst_msgs_lost_tot[conn_num] +=
                  tnicon_p->srv_data_rqsts_outstanding[conn_num];
        tnicon_p->srv_data_resp_msgs_lost_tot[conn_num] +=
                  tnicon_p->srv_data_resps_outstanding[conn_num];
        tnicon_p->srv_data_unso_msgs_lost_tot[conn_num] +=
                  tnicon_p->srv_data_unsos_outstanding[conn_num];

        /*
         *  Accumulate requests and responses write errors over all 
         *  RPC connections
         */

        if (messaging_type == MSG_TYPE_ES_RPC)
        {
            tnicon_p->tot_rpc_rqsts_wrt_lost += 
                      tnicon_p->srv_data_rqsts_outstanding[conn_num];
            tnicon_p->num_rpc_rqsts_wrt_lost +=
                      tnicon_p->srv_data_rqsts_outstanding[conn_num];

            tnicon_p->tot_rpc_resps_wrt_lost +=
                      tnicon_p->srv_data_resps_outstanding[conn_num];
            tnicon_p->num_rpc_resps_wrt_lost +=
                      tnicon_p->srv_data_resps_outstanding[conn_num];
        }
    }

    totwritelen[conn_num] = 0;
    tothostmsgs[conn_num] = 0;
    writechain[conn_num] = Init_Write_Chain ();

    tnicon_p->srv_data_rqsts_outstanding[conn_num] = 0;
    tnicon_p->srv_data_resps_outstanding[conn_num] = 0;
    tnicon_p->srv_data_unsos_outstanding[conn_num] = 0;

    return;
}
