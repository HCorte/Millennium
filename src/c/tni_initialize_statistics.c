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
/*====[TNI_INITIALIZE_STATISTICS.C]===========================================*/
/*                                                                            */
/* Initialize_Statistics (void )                                              */
/*                                                                            */
/* Purpose: This function initializes all statistics to 0                     */
/*                                                                            */
/* Input Arguments:     none                                                  */
/*                                                                            */
/* Output Arguments:    none                                                  */
/*                                                                            */
/* Return Value:        none                                                  */
/*                                                                            */
/* Assumptions:         none                                                  */
/* Update_Min_Max_Resp_Times(int responseTime, int *minimumRespTime, 
 *
 * Purpose:
 *
 * This function Tests whether the response time is the new minimum 
 * or maximum for  * the statistics  interval.
 *
 * Input Arguments :
 * 
 * responseTime    - most current response time
 * minimumRespTime - current minimum response time for interval
 * maximumRespTime - current maximum response time for interval
 *
 * Output Arguments : None  
 *
 * Returns Values: None
 *
 *                                                                           */
/*====[TNI_INITIALIZE_STATISTICS.C]===========================================*/
/*                                                                            */

#include "includes.h"

void Initialize_Statistics() {

    int conn;
    int index;
    int term_stats_idx;

    static int init_in_game_values = 1;

    TNICON *ptr;
    TERM_STATS *term_stats_p;              /* Pointer to the statistics       */

    ptr = tnicon_p;


/* initialize connection-based statistics                                     */

    for( conn = 1; conn <= MAX_CONN; conn++ ) {

/*     Statistics information for a connection which is kept for an ENTIRE    */
/*     DAY                                                                    */

/*     PDUs sent to a client                                                  */

        ptr->clt_param_resp_cnt_tot[conn]   = 0;
        ptr->srv_param_req_cnt_tot[conn]    = 0;
        ptr->oltp_srv_data_cnt_tot[conn]    = 0;
        ptr->rpc_srv_data_cnt_tot[conn]     = 0;
        ptr->srv_alive_cnt_tot[conn]        = 0;
        ptr->error_sent_cnt_tot[conn]       = 0;
        ptr->clt_cmd_resp_cnt_tot[conn]     = 0;                 
        ptr->srv_cmd_req_cnt_tot[conn]      = 0;                   
        ptr->clt_session_resp_cnt_tot[conn] = 0;
        ptr->srv_challenge_req_cnt_tot[conn] = 0;
        ptr->srv_challenge_notify_cnt_tot[conn] = 0;
        ptr->srv_enc_key_rqst_cnt_tot[conn] = 0;

/*     PDUs received from a client                                            */

        ptr->clt_param_req_cnt_tot[conn]    = 0;
        ptr->srv_param_resp_cnt_tot[conn]   = 0;
        ptr->oltp_clt_data_cnt_tot[conn]    = 0;
        ptr->rpc_clt_data_cnt_tot[conn]     = 0;
        ptr->clt_alive_cnt_tot[conn]        = 0;
        ptr->error_rcvd_cnt_tot[conn]       = 0;
        ptr->clt_cmd_req_cnt_tot[conn]      = 0;                  
        ptr->srv_cmd_resp_cnt_tot[conn]     = 0;
        ptr->clt_session_req_cnt_tot[conn]  = 0;
        ptr->srv_challenge_resp_cnt_tot[conn] = 0;
        ptr->srv_enc_key_resp_cnt_tot[conn] = 0;
        ptr->clt_enc_key_notify_cnt_tot[conn] = 0;

/*     total number of client messages received in Data PDUs      */

        ptr->clt_data_rqst_msgs_cnt_tot[conn] = 0;
        ptr->clt_data_resp_msgs_cnt_tot[conn] = 0;
        ptr->clt_data_unso_msgs_cnt_tot[conn] = 0;

/*     total number of server messages sent in Data PDUs      */

        ptr->srv_data_rqst_msgs_cnt_tot[conn] = 0;
        ptr->srv_data_resp_msgs_cnt_tot[conn] = 0;
        ptr->srv_data_unso_msgs_cnt_tot[conn] = 0;


/*     read/write byte and error counts                                       */

        ptr->conn_read_byte_count_tot[conn]   = 0;
        ptr->conn_write_byte_count_tot[conn]  = 0;
        ptr->conn_read_err_count_tot[conn]    = 0;
        ptr->conn_write_err_count_tot[conn]   = 0;


/*     Statistics information for a connection which is accumulated ONLY over */
/*     the statistics interval                                                */

/*     total number of client messages received in Data PDUs      */

        ptr->clt_data_rqst_msgs_cnt[conn] = 0;
        ptr->clt_data_resp_msgs_cnt[conn] = 0;
        ptr->clt_data_unso_msgs_cnt[conn] = 0;

        ptr->clt_data_rqst_msgs_cnt_dis[conn] = 0;
        ptr->clt_data_resp_msgs_cnt_dis[conn] = 0;
        ptr->clt_data_unso_msgs_cnt_dis[conn] = 0;


/*     total number of server messages sent in Data PDUs      */

        ptr->srv_data_rqst_msgs_cnt[conn] = 0;
        ptr->srv_data_resp_msgs_cnt[conn] = 0;
        ptr->srv_data_unso_msgs_cnt[conn] = 0;

        ptr->srv_data_rqst_msgs_cnt_dis[conn] = 0;       
        ptr->srv_data_resp_msgs_cnt_dis[conn] = 0;       
        ptr->srv_data_unso_msgs_cnt_dis[conn] = 0;       

/*     read/write byte and error counts                                       */
        ptr->conn_read_byte_count[conn] = 0;
        ptr->conn_read_byte_count_dis[conn] = 0;
        ptr->conn_write_byte_count[conn] = 0;
        ptr->conn_write_byte_count_dis[conn] = 0;
        ptr->conn_read_err_count[conn] = 0;
        ptr->conn_read_err_count_dis[conn] = 0;
        ptr->conn_write_err_count[conn] = 0;
        ptr->conn_write_err_count_dis[conn] = 0;

/*     number of times a connection's socket actually had something to be read*/

        ptr->conn_read_select_success_count[conn] = 0;
        ptr->conn_read_select_success_count_dis[conn] = 0;

/*     total number of server data messages the MX Server task could not      */
/*     send to the client, based on totwritemsgs                              */

        ptr->srv_data_rqst_msgs_lost_tot[conn] = 0;
        ptr->srv_data_resp_msgs_lost_tot[conn] = 0;
        ptr->srv_data_unso_msgs_lost_tot[conn] = 0;

    } /* end for conn = 1 */

/* Statistics information across ALL connections which is accumulated ONLY    */
/* over the statistics interval                                               */

/* number of times a select statement is executed when we intend to do a read */
/* operation                                                                  */

    ptr->read_select_count = 0;
    ptr->read_select_count_dis = 0;

/* number of times ANY socket actually had something to be read.  This        */
/* counter is only incremented once for a single read select, even if         */
/* multiple bits are set in the read mask returned by the select.             */

    ptr->read_select_success_count = 0;
    ptr->read_select_success_count_dis = 0;


/* Statistics information relating to GTMS which is accumulated ONLY over the */
/* statistics interval                                                        */

/* total number of oltp messages the MX Server has received from the game     */

    ptr->oltp_msgs_from_game = 0;
    ptr->oltp_msgs_from_game_dis = 0;

/* total number of RPC messages the MX Server has received from the game      */

    ptr->rpc_msgs_from_game = 0;
    ptr->rpc_msgs_from_game_dis = 0;

/* number of times the TNI main function executes gtms_receive                */

    ptr->gr_receive_count = 0;
    ptr->gr_receive_count_dis = 0;

/* number of times we read from GTMS and had no message to receive            */

    ptr->gr_nomsg_count = 0;
    ptr->gr_nomsg_count_dis = 0;

/* Clear ES RPC message processing statistics                                 */

    ptr->tot_rpc_rqsts_ack = 0;
    ptr->tot_rpc_rqsts_noack = 0;
    ptr->tot_rpc_rqsts_sent = 0;
    ptr->tot_rpc_resps = 0;
    ptr->tot_rpc_resps_rcvd = 0;
    ptr->tot_rpc_rqst_failures = 0;
    ptr->tot_rpc_rqst_timeouts = 0;
    ptr->tot_rpc_resp_failures = 0;
    ptr->tot_rpc_rqsts_wrt_lost = 0;
    ptr->tot_rpc_resp_late = 0;
    ptr->tot_rpc_err_pdus_rcvd = 0;

/* Clear HOST RPC message processing statistics                                 */

    ptr->tot_rpc_rqsts_rcvd = 0;
    ptr->tot_rpc_resps_sent = 0;
    ptr->tot_rpc_err_pdus_sent = 0;
    ptr->tot_rpc_resps_wrt_lost = 0;
    ptr->tot_hostrpc_resp_failures = 0;

    ptr->min_hostrpc_resp_time_dis = 99999;

/* Clear the network terminal-based statistics                                */

    for (term_stats_idx = 1; term_stats_idx <= max_terminal; term_stats_idx++)
    {

/*      Get pointer to this terminal's information in global section          */

        term_stats_p = &tstats_p->term_stats;
        term_stats_p = &term_stats_p[term_stats_idx];

        for( index = 0; index < MAX_APPS + MAX_NUM_CLIENT_IDS; index++ )
        {
            term_stats_p->app_request_tot[index] = 0;
            term_stats_p->app_response_tot[index] = 0;
            term_stats_p->app_unso_tot[index] = 0;
            term_stats_p->host_term_rqst_tot[index] = 0;
            term_stats_p->host_term_resp_tot[index] = 0;
        }

        if (init_in_game_values) {

          term_stats_p->trans_in_game_cnt = 0;
          term_stats_p->requesting_conn_num = 0;

          memset(term_stats_p->correlation_tag,
                 0x00,
                 sizeof(term_stats_p->correlation_tag));
        }
    }
    init_in_game_values = 0;
}

/* [Update_Min_Max_Resp_Times]
 *
 * Summary:
 *
 * Update_Min_Max_Resp_Times(int responseTime, int *minimumRespTime, int *maximumRespTime)
 *
 * responseTime    - most current response time
 * minimumRespTime - current minimum response time for interval
 * maximumRespTime - current maximum response time for interval
 *
 * Description:
 *
 * Tests whether the response time is the new minimum or maximum for the statistics
 * interval.
 *
 * Returns Values:
 *
 * None
 *
 */

void
Update_Min_Max_Resp_Times(int responseTime, int *minimumRespTime, int *maximumRespTime)
{
/* Test if less than minimum */

    if (responseTime < *minimumRespTime)
    {
        *minimumRespTime = responseTime;
    }

/* Test if greater than maximum */

    if (responseTime > *maximumRespTime)
    {
        *maximumRespTime = responseTime;
    }

    return;
}
