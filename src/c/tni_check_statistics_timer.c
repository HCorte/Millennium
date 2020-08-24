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
/*====[TNI_CHECK_STATISTICS_TIMER.C]==========================================*/
/*                                                                            */
/* Check_Statistics_Timer()                                                   */
/*                                                                            */
/* Purpose: This function updates the statistics which are maintained on a    */
/*          statistics interval basis for display by TNIVISION.  It the       */
/*          statistics timer has expired, then the statistics are updated as  */
/*          well as the last statistics time stamp.                           */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_CHECK_STATISTICS_TIMER.C]==========================================*/
/*                                                                            */

#include "includes.h"

void Check_Statistics_Timer()
{

unsigned short conn;                 /* connection number                     */

/*     Statistics timer has expired so update display information and         */
/*     initialize interval statistics for next statistics update cycle        */

/*     Statistics information for a host id which is accumulated ONLY over    */
/*     the statistics interval                                                */

       for ( conn = 1; conn < MAX_CONN; conn++ ) {

/*         Statistics information for a connection which is accumulated ONLY  */
/*         over the statistics interval                                       */

/*         total number of terminal messages contained in all Host Data PDUs  */

           tnicon_p->clt_data_rqst_msgs_cnt_dis[conn] = 
                     tnicon_p->clt_data_rqst_msgs_cnt[conn];
           tnicon_p->clt_data_resp_msgs_cnt_dis[conn] = 
                     tnicon_p->clt_data_resp_msgs_cnt[conn];
           tnicon_p->clt_data_unso_msgs_cnt_dis[conn] = 
                     tnicon_p->clt_data_unso_msgs_cnt[conn];

            tnicon_p->clt_data_rqst_msgs_cnt[conn] = 0;
            tnicon_p->clt_data_resp_msgs_cnt[conn] = 0;
            tnicon_p->clt_data_unso_msgs_cnt[conn] = 0;

/*         total number of terminal and RPC messages sent by the MX Server    */
/*         broken down into requests, respones, and unsoliciteds              */

           tnicon_p->srv_data_rqst_msgs_cnt_dis[conn] = 
               tnicon_p->srv_data_rqst_msgs_cnt[conn];
           tnicon_p->srv_data_resp_msgs_cnt_dis[conn] =
               tnicon_p->srv_data_resp_msgs_cnt[conn];
           tnicon_p->srv_data_unso_msgs_cnt_dis[conn] =
               tnicon_p->srv_data_unso_msgs_cnt[conn];

           tnicon_p->srv_data_rqst_msgs_cnt[conn] = 0;
           tnicon_p->srv_data_resp_msgs_cnt[conn] = 0;
           tnicon_p->srv_data_unso_msgs_cnt[conn] = 0;

/*         total number of terminal and RPC messages which failed to be sent  */
/*         by the MX Server broken down into requests, respones, and          */
/*         unsoliciteds                                                       */

           tnicon_p->srv_data_rqst_msgs_lost_dis[conn] = 
               tnicon_p->srv_data_rqst_msgs_lost[conn];
           tnicon_p->srv_data_resp_msgs_lost_dis[conn] = 
               tnicon_p->srv_data_resp_msgs_lost[conn];
           tnicon_p->srv_data_unso_msgs_lost_dis[conn] = 
               tnicon_p->srv_data_unso_msgs_lost[conn];

           tnicon_p->srv_data_rqst_msgs_lost[conn] = 0;
           tnicon_p->srv_data_resp_msgs_lost[conn] = 0;
           tnicon_p->srv_data_unso_msgs_lost[conn] = 0;

/*         read/write byte and error counts                                   */

           tnicon_p->conn_read_byte_count_dis[conn] = 
                     tnicon_p->conn_read_byte_count[conn];
           tnicon_p->conn_read_byte_count[conn] = 0;

           tnicon_p->conn_write_byte_count_dis[conn] = 
                     tnicon_p->conn_write_byte_count[conn];
           tnicon_p->conn_write_byte_count[conn] = 0;
           tnicon_p->conn_read_err_count_dis[conn] = 
                     tnicon_p->conn_read_err_count[conn];
           tnicon_p->conn_read_err_count[conn] = 0;
           tnicon_p->conn_write_err_count_dis[conn] = 
                     tnicon_p->conn_write_err_count[conn];
           tnicon_p->conn_write_err_count[conn] = 0;

/*         number of times a connection's socket actually had something to be */
/*         read                                                               */

           tnicon_p->conn_read_select_success_count_dis[conn] =
                     tnicon_p->conn_read_select_success_count[conn];
           tnicon_p->conn_read_select_success_count[conn] = 0;
       }                                              /* end conn loop        */

/*     Statistics information across ALL connections which is accumulated     */
/*     ONLY over the statistics interval                                      */

/*     number of times a select statement is executed when we intend to do a  */
/*     read operation                                                         */

       tnicon_p->read_select_count_dis = tnicon_p->read_select_count;
       tnicon_p->read_select_count = 0;

/*     number of times ANY socket actually had something to be read.  This    */
/*     counter is only incremented once for a single read select, even if     */
/*     multiple bits are set in the read mask returned by the select.         */

       tnicon_p->read_select_success_count_dis = 
                 tnicon_p->read_select_success_count;
       tnicon_p->read_select_success_count = 0;

/*     Statistics information relating to GTMS which is accumulated ONLY over */
/*     the statistics interval                                                */

/*     total number of oltp messages the MX SERVER has received from the game */

       tnicon_p->oltp_msgs_from_game_dis = tnicon_p->oltp_msgs_from_game;
       tnicon_p->oltp_msgs_from_game = 0;

/*     total number of RPC messages the MX SERVER has received from the game  */

       tnicon_p->rpc_msgs_from_game_dis = tnicon_p->rpc_msgs_from_game;
       tnicon_p->rpc_msgs_from_game = 0;

/*     number of times the TNI main function executes gtms_receive            */

       tnicon_p->gr_receive_count_dis = tnicon_p->gr_receive_count;
       tnicon_p->gr_receive_count = 0;

/*     number of times we read from GTMS and had no message to receive        */

       tnicon_p->gr_nomsg_count_dis = tnicon_p->gr_nomsg_count;
       tnicon_p->gr_nomsg_count = 0;

/*     Update OLTP responses times                                            */

       if (tnicon_p->num_oltp_resps != 0)
       {
           tnicon_p->ave_oltp_resp_time_dis = tnicon_p->tot_oltp_resp_time /
                                              tnicon_p->num_oltp_resps;
       }
       else
       {
           tnicon_p->ave_oltp_resp_time_dis = 0;
       }

       tnicon_p->tot_oltp_resp_time = 0;
       tnicon_p->num_oltp_resps = 0;

       tnicon_p->min_oltp_resp_time_dis = tnicon_p->min_oltp_resp_time;
       tnicon_p->min_oltp_resp_time = 99999;

       tnicon_p->max_oltp_resp_time_dis = tnicon_p->max_oltp_resp_time;
       tnicon_p->max_oltp_resp_time = 0;

/*     Update ES RPC stats                                                       */

       if (tnicon_p->num_rpc_resps != 0)
       {
           tnicon_p->ave_rpc_resp_time_dis = tnicon_p->tot_rpc_resp_time /
                                             tnicon_p->num_rpc_resps;
       }
       else
       {
           tnicon_p->ave_rpc_resp_time_dis = 0;
       }

       tnicon_p->tot_rpc_resp_time = 0;

       tnicon_p->min_rpc_resp_time_dis = tnicon_p->min_rpc_resp_time;
       tnicon_p->min_rpc_resp_time = 99999;

       tnicon_p->max_rpc_resp_time_dis = tnicon_p->max_rpc_resp_time;
       tnicon_p->max_rpc_resp_time = 0;

       tnicon_p->num_rpc_rqsts_ack_dis = tnicon_p->num_rpc_rqsts_ack;
       tnicon_p->num_rpc_rqsts_ack = 0;

       tnicon_p->num_rpc_rqsts_noack_dis = tnicon_p->num_rpc_rqsts_noack;
       tnicon_p->num_rpc_rqsts_noack = 0;

       tnicon_p->num_rpc_resps_dis = tnicon_p->num_rpc_resps;
       tnicon_p->num_rpc_resps = 0;

       tnicon_p->num_rpc_rqst_failures_dis = tnicon_p->num_rpc_rqst_failures;
       tnicon_p->num_rpc_rqst_failures = 0;

       tnicon_p->num_rpc_rqst_timeouts_dis = tnicon_p->num_rpc_rqst_timeouts;
       tnicon_p->num_rpc_rqst_timeouts = 0;

       tnicon_p->num_rpc_resp_failures_dis = tnicon_p->num_rpc_resp_failures;
       tnicon_p->num_rpc_resp_failures = 0;

       tnicon_p->num_rpc_resp_late_dis = tnicon_p->num_rpc_resp_late;
       tnicon_p->num_rpc_resp_late = 0;

       tnicon_p->num_rpc_err_pdus_rcvd_dis = tnicon_p->num_rpc_err_pdus_rcvd;
       tnicon_p->num_rpc_err_pdus_rcvd = 0;

       tnicon_p->num_rpc_rqsts_sent_dis = tnicon_p->num_rpc_rqsts_sent;
       tnicon_p->num_rpc_rqsts_sent = 0;

       tnicon_p->num_rpc_resps_rcvd_dis = tnicon_p->num_rpc_resps_rcvd;
       tnicon_p->num_rpc_resps_rcvd = 0;

       tnicon_p->num_rpc_wrt_failures_dis = tnicon_p->num_rpc_rqsts_wrt_lost;
       tnicon_p->num_rpc_rqsts_wrt_lost = 0;

/*     Update HOST RPC stats                                                       */

       if (tnicon_p->num_rpc_resps_sent != 0)
       {
           tnicon_p->ave_hostrpc_resp_time_dis = tnicon_p->tot_hostrpc_resp_time /
                                                 tnicon_p->num_rpc_resps_sent;
       }
       else
       {
           tnicon_p->ave_hostrpc_resp_time_dis = 0;
       }

       tnicon_p->tot_hostrpc_resp_time = 0;

       tnicon_p->min_hostrpc_resp_time_dis = tnicon_p->min_hostrpc_resp_time;
       tnicon_p->min_hostrpc_resp_time = 99999;

       tnicon_p->max_hostrpc_resp_time_dis = tnicon_p->max_hostrpc_resp_time;
       tnicon_p->max_hostrpc_resp_time =0;

       tnicon_p->num_rpc_resps_sent_dis = tnicon_p->num_rpc_resps_sent;
       tnicon_p->num_rpc_resps_sent = 0;

       tnicon_p->num_rpc_rqsts_rcvd_dis = tnicon_p->num_rpc_rqsts_rcvd;
       tnicon_p->num_rpc_rqsts_rcvd = 0;

       tnicon_p->num_rpc_err_pdus_sent_dis = tnicon_p->num_rpc_err_pdus_sent;
       tnicon_p->num_rpc_err_pdus_sent = 0;

       tnicon_p->num_rpc_resps_wrt_lost_dis = tnicon_p->num_rpc_resps_wrt_lost;
       tnicon_p->num_rpc_resps_wrt_lost = 0;

       tnicon_p->num_hostrpc_resp_failures_dis = tnicon_p->num_hostrpc_resp_failures;
       tnicon_p->num_hostrpc_resp_failures = 0;

/*     Update time stamp                                                      */

       tnicon_p->last_stats_time = current_time;

/*     TEMP CODE to WRITE STATS TO TASK'S LOG FILE                            */

       if ((tnicon_p->dbg_state == DBG_ACTIVE) &&
           (tnicon_p->print_flag & STATS_LEVEL_DBG)) {

           Show_Current_Time ("Check_Statistics_Timers");

           fprintf (tnicon_p->dbg_p,
                    "Read attempts %ld , Actual reads %ld\n",
                    tnicon_p->read_select_count_dis,
                    tnicon_p->read_select_success_count_dis);

           fprintf (tnicon_p->dbg_p,
                    "Msg deque attempts(GTMS receives) %ld , Msg deque count %ld\n",
                    tnicon_p->gr_receive_count_dis,
                    tnicon_p->gr_receive_count_dis - tnicon_p->gr_nomsg_count_dis);

           fprintf (tnicon_p->dbg_p,
                    "Msgs sent to Clients %ld\n",
                    tnicon_p->oltp_msgs_from_game_dis);
       }

/*     END TEMP CODE to WRITE STATS TO TASK'S LOG FILE                        */
}
