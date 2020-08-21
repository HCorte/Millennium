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
/*====[TNI_CHECK_BLOCKING_TIMERS.C]===========================================*/
/*                                                                            */
/* Check_Blocking_Timers()                                                    */
/*                                                                            */
/* Purpose: This function checks the message blocking timers for each         */
/*          host id for which we currently have message(s) queued.  If a      */
/*          host's blocking timer has expired, then send the queued           */
/*          messages.  If a write error occurs, then send a message to        */
/*          PX2XPRO telling him to release the terminal's procom buffers.     */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_CHECK_BLOCKING_TIMERS.C]===========================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void Check_Blocking_Timers() {

/*  This function checks all connections to determine if we have any terminal */
/*  data to send to the host.  If we have data to send, then check the        */
/*  blocking timer to determine if it's time to send.                         */

    int act_conn;                          /* Actual connection PDU was sent  */
    int conn_num;
    short int error;                   /* socket write error flag             */
    struct TNI_PARAM_PAIR params[2];   /* only the message count variable data*/
                                       /* parameter is used for term data PDU */
    struct timeb elapsed_time;
    struct mbuf *p;                    /* mbuf pointer                        */
    int msg_type;                      /* OLTP or RPC                         */

    for( conn_num = 1; conn_num <= MAX_CONN; conn_num++ ) {

        if( (totwritelen[conn_num] > 0) && 
            (tnicon_p->connection[conn_num].max_messages_per_pdu != 1) ) {

            subtimes(&current_time, 
                     &tnicon_p->connection[conn_num].time_last_block_sent,
                     &elapsed_time);

            if( (elapsed_time.time > 
                 tnicon_p->connection[conn_num].blocking_time.time) ||
                ((elapsed_time.time == 
                  tnicon_p->connection[conn_num].blocking_time.time) &&
                  (elapsed_time.millitm >= 
                   tnicon_p->connection[conn_num].blocking_time.millitm)) ) {

/*              Blocking timer has expired so send what we already have       */

                params[0].param_code = NUMBER_MESSAGES;
                params[0].param_value = tothostmsgs[conn_num];
                params[1].param_code = 0;
                params[1].param_value = 0;

/*              Get messaging type of client                                  */

                msg_type = tnicon_p->connection[conn_num].messaging_type;

                switch (msg_type)
                {
                case MSG_TYPE_OLTP:

                    p = Build_Pdu (conn_num,
                                   TERMINAL_SERVER_DATA_PDU,
                                   writechain[conn_num],
                                   &params[0]);
                    break;

                case MSG_TYPE_ES_RPC:

                    p = Build_Pdu (conn_num,
                                   RPC_SERVER_DATA_PDU,
                                   writechain[conn_num],
                                   &params[0]);
                    break;
                }

                error = 0;

                act_conn = Send_To_Conn (p, conn_num, CHKAUTH_ALT_CONN);

                if (act_conn == 0) {

                    sprintf(err_string.par1,"ERROR");
                    sprintf(err_string.par2,"%d",conn_num);

                    output_err("Check_Blocking_Timers",
                               MI_TNI_ERR_SEND,
                               MX_ERR_LVL_ERROR,
                               err_string);

                    error = -1;
                } else {

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

                    switch (msg_type)
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

                if( error==-1 ) {

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

                   if (msg_type == MSG_TYPE_ES_RPC)
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

                tnicon_p->connection[conn_num].time_last_block_sent = current_time;
            }                          /* end if timer check                  */

        }                              /* end if size check                   */

    }
    return;                            /* end for                             */
}
