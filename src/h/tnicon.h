/* */

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
/*====[TNICON.H]==============================================================*/
/*                                                                            */
/* Purpose: This header file contains all data structures definitions which   */
/*          comprise the TNI global section.                                  */
/*                                                                            */
/*====[TNICON.H]==============================================================*/
/*                                                                            */

#ifndef TNICON_LOADED

#   define TNICON_LOADED 1

#   include <stdio.h>

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#       include "tni_global.h"
#       include "events.h"

#   elif defined(XOS_VMS)

#       include "tni_global.h"
#       include "events.h"

#   else

#       error - OS-specific logic not handled.

#   endif

#   define TNICON_SECT "tnicon"

/* structure defining all connections for all client */

    typedef struct {
        char application_name[MXSRV_MAX_APP_NAME_LEN + 1];
        char local_domain_name[MAX_CLIENT_NAME_LEN];
        char remote_node_name[MAX_CLIENT_NAME_LEN];
        char remote_domain_name[MAX_CLIENT_NAME_LEN];
        char session_tag[MAX_SESSION_TAG_LEN];
        unsigned char auth_hash_value[MAX_AUTH_HASH_LENGTH];
        unsigned char auth_state;
        unsigned char conn_state;
        unsigned char last_client_state;
        unsigned char term_id_method;
        unsigned char tni_proto_ver;
        short int max_messages_per_pdu;
        int auth_hash_len;
        int consec_auth_failure_cnt;
        int alt_conn;
        int app_idx;
        int client_id_idx;
        int client_id_val;
        int managed_by;
        int max_pdu_size;
        int messaging_type;
        int enc_key_x_method;
        int enc_key_type;
        int sock;
        int enc_state;

     /* It is understood that any address assigned to this pointer */
     /* is only valid for the task (mainpro) that assigned the address. */
     /* Any other task attempting to use this pointer will crash. */ 
        void *enc_key;

        struct timeb blocking_time;
        struct timeb keepalive_time;
        struct timeb conn_alive_timeout;
        struct timeb time_last_sent;
        struct timeb time_last_rcvd;
        struct timeb time_last_block_sent;
        struct timeb last_conn_atmpt_time;
        struct timeb last_auth_atmpt_time;
    }  CONNECTION_INFO;

    typedef struct {
        int state;
        int rpc_rqst_timeout;
        int oltp_unso_routing;
        int oltp_rqst_routing;
        int client_conn_cnt;
        int next_unso_client_idx;
        int auth_required;
        int auth_hash_algorithm;
        int auth_salt_length;
        int auth_lockout_state;
        int es_rpc_enc_mode;
        int es_rpc_enc_state;
        int client_conn[MAX_CONN_PER_APP];
        char name[MXSRV_MAX_APP_NAME_LEN + 1];
        struct timeb auth_lockout_release_time;
    }  APPLICATION_INFO;

    typedef struct {                  /* TNI global section structure         */

        CONNECTION_INFO connection[MAX_CONN_PLUS_ONE];

        APPLICATION_INFO app[MAX_APPS];

        char local_domain_name[MAX_INTERFACES_PER_TNI][MAX_CLIENT_NAME_LEN];

        struct timeb conn_atmpt_time;
        struct timeb check_conn_time;
        struct timeb read_atmpt_time;

        struct timeb takeover_time;
        struct timeb last_stats_time;
        struct timeb statistics_time;
        struct timeb check_rpc_timeout;
	struct timeval wrt_select_timeout;

#       if defined(XOS_VMS)

#           pragma member_alignment save
#           pragma nomember_alignment

            struct TIMER_VALUES rd_wrt_tout_time;

#           pragma member_alignment restore

#       endif

        int listening_port;
        int takeover_invl;              /* secs                               */
        int conn_atmpt_invl;            /* secs                               */
        int check_conn_invl;            /* secs                               */
        int read_atmpt_invl;            /* 10 msec tics                       */
        int statistics_invl;            /* secs                               */
        int def_rpc_rqst_timeout;       /* secs                               */
        int cycle_rate;                 /* main loop iterations per 10 msec   */
                                        /* tics                               */
        int tni_server_state;           /* V1.01 - state (Primary or          */
                                        /* non-Primary) of the machine on     */
                                        /* which we're running                */
        int def_term_id_meth;           /* default terminal identification    */
                                        /* method                             */

        int auth_failure_threshold;
        int auth_lockout_invl;          /* secs                               */
	int wrt_select_tout_invl;       /* milliseconds                       */
	int unso_failure_notify1;       /* boolean value enable/disable, used */
	                                /* for clients managed by client id   */
        int unso_failure_notify2;       /* boolean value enable/disable, used */
                                        /* for clients managed by application */

        char term_id_file[MAX_CLIENT_NAME_LEN*2];
        char term_tag_file[MAX_CLIENT_NAME_LEN*2];

/* client_conn maps connection numbers from a single client id                */

        int client_conns[MAX_NUM_CLIENT_IDS][MAX_CONNS_PER_CLIENT];

/* client_idx_tbl maps client id numbers to client id indexes                 */

        int client_idx_tbl[MAX_NUM_CLIENT_IDS];

/* contains a list of sorted by client id values                              */

        int sorted_client_ids[MAX_NUM_CLIENT_IDS];

/* last connection OLTP data was sent on per client id                        */

        int last_oltp_conn_sent[MAX_NUM_CLIENT_IDS];

/* TCP send and receive buffer sizes (bytes)                                  */

        int rcv_buf_size;
        int send_buf_size;

/* Socket read write timeout interval                                         */

        int rd_wrt_tout_invl;

/* MX Server communications mode                                              */

        unsigned int start_mode;   /* comm mode specified in the tni_server   */
        unsigned int comm_mode;    /* files current communications mode       */

/* Primary Server notification mode                                           */

        unsigned int prim_notify_mode;
        unsigned int prim_notify_overide;

/* Maximum outstanding request allowed to ES applications                     */

        int max_es_requests;

/* Default ES application name assigned to all terminal request messages,     */
/* which do not have a destination specified in the product request message   */

        char def_term_rqst_dest[MXSRV_MAX_APP_NAME_LEN + 1];

/* Keystore location */

        char keystore_file[MAX_FILE_PATH_NAME];

/* Keystore location */

        char publickey_file[MAX_FILE_PATH_NAME];

/* Exchange key state */

        int exchange_enc_key_state;

/* Exchange keys */
/* It is understood that any address assigned to exchange key pointers */
/* is only valid for the task (mainpro) that assigned the address. */
/* Any other task attempting to use this pointer will crash. */

        void *x_encrypt_key;
        void *x_decrypt_key;

/* TNI STATISTICS SECTION                                                     */
/*                                                                            */
/* Statistics may be kept over an entire GSWITCH day or only over the         */
/* statistics interval.  The following naming conventions are used;           */
/*                                                                            */
/* 1. Variables ending with "_tot" are the statistics kept for an entire day. */
/*    These are only initialized when the GSWITCH task is initialized and at  */
/*    day end.  If these statistics are displayed in TNI Vision, then their   */
/*    actual variables are displayed.                                         */
/* 2. Variables ending with "_dis" are the display versions of the statistics */
/*    which are kept only for the duration of the statistics interval.  The   */
/*    design of the statistics update is that the running statistics are      */
/*    updated by the TNI task itself.  When the statistics interval expires,  */
/*    the statistics display variables are updated with the running           */
/*    statistics and the running statistics are initialized for the start of  */
/*    the next statistics interval.  To prevent TNI Vision from constantly    */
/*    updating, only the display variables are displayed in TNI Vision.       */
/* 3. Variables not ending with either "_tot" or "_dis" are the running       */
/*    statistics which are constantly updating during the statistics interval.*/
/*    These are the running statistics which are initialized at the start of  */
/*    each statistics interval.                                               */
/*                                                                            */

/* Statistics information for a host id which is kept for an ENTIRE DAY       */

/* total number of terminal or RPC messages the MX Server could not send to   */
/* the client, based on totwritemsgs.                                         */

        ubyte_4 srv_data_rqst_msgs_lost_tot[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_resp_msgs_lost_tot[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_unso_msgs_lost_tot[MAX_CONN_PLUS_ONE];

/* total number of "bro to all" application messages the TNI task             */
/* sent to PX2XPRO for each host id                                           */

        unsigned long int app_bro_tot[MAX_CONN_PLUS_ONE];

/* Statistics information for a host id which is accumulated ONLY over the    */
/* statistics interval                                                        */

/* total number of terminal or RPC messages the MX Server could not send to   */
/* the client, based on totwritemsgs.                                         */

        ubyte_4 srv_data_rqst_msgs_lost[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_resp_msgs_lost[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_unso_msgs_lost[MAX_CONN_PLUS_ONE];

        /* display only */

        ubyte_4 srv_data_rqst_msgs_lost_dis[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_resp_msgs_lost_dis[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_unso_msgs_lost_dis[MAX_CONN_PLUS_ONE];

/* Statistics information for a connection which is kept for an ENTIRE DAY    */

/* PDUs sent to a Client                                                      */

        unsigned long int clt_param_resp_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_param_req_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int oltp_srv_data_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int rpc_srv_data_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_alive_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int error_sent_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int clt_cmd_resp_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_cmd_req_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int clt_session_resp_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_challenge_req_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_challenge_notify_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_enc_key_rqst_cnt_tot[MAX_CONN_PLUS_ONE];


/* PDUs received from a Client                                                */

        unsigned long int clt_param_req_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_param_resp_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int oltp_clt_data_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int rpc_clt_data_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int clt_alive_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int error_rcvd_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int clt_cmd_req_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_cmd_resp_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int clt_session_req_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_challenge_resp_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int srv_enc_key_resp_cnt_tot[MAX_CONN_PLUS_ONE];
        unsigned long int clt_enc_key_notify_cnt_tot[MAX_CONN_PLUS_ONE];

/* total number of terminal or RPC messages contained in terminal or RPC      */
/* Client Data PDUs                                                           */

        ubyte_4 clt_data_rqst_msgs_cnt_tot[MAX_CONN_PLUS_ONE];
        ubyte_4 clt_data_resp_msgs_cnt_tot[MAX_CONN_PLUS_ONE];
        ubyte_4 clt_data_unso_msgs_cnt_tot[MAX_CONN_PLUS_ONE];

/* total number of terminal or RPC messages sent in terminal or RPC Server    */
/* Data PDUs                                                                  */

        ubyte_4 srv_data_rqst_msgs_cnt_tot[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_resp_msgs_cnt_tot[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_unso_msgs_cnt_tot[MAX_CONN_PLUS_ONE];

/* read/write byte and error counts                                           */

        unsigned long int conn_read_byte_count_tot[MAX_CONN_PLUS_ONE];
        unsigned long int conn_write_byte_count_tot[MAX_CONN_PLUS_ONE];
        unsigned long int conn_read_err_count_tot[MAX_CONN_PLUS_ONE];
        unsigned long int conn_write_err_count_tot[MAX_CONN_PLUS_ONE];

/* Statistics information for a connection which is accumulated ONLY over the */
/* statistics interval                                                        */

/* total number of terminal or RPC messages contained in terminal or RPC      */
/* Client Data PDUs over the last statists interval                           */

        ubyte_4 clt_data_rqst_msgs_cnt[MAX_CONN_PLUS_ONE];
        ubyte_4 clt_data_resp_msgs_cnt[MAX_CONN_PLUS_ONE];
        ubyte_4 clt_data_unso_msgs_cnt[MAX_CONN_PLUS_ONE];

        /* display only */

        ubyte_4 clt_data_rqst_msgs_cnt_dis[MAX_CONN_PLUS_ONE];
        ubyte_4 clt_data_resp_msgs_cnt_dis[MAX_CONN_PLUS_ONE];
        ubyte_4 clt_data_unso_msgs_cnt_dis[MAX_CONN_PLUS_ONE];

/* total number of terminal or RPC messages sent in terminal or RPC Server    */
/* Data PDUs over the last statists interval                                  */

        ubyte_4 srv_data_rqst_msgs_cnt[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_resp_msgs_cnt[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_unso_msgs_cnt[MAX_CONN_PLUS_ONE];

        /* display only */

        ubyte_4 srv_data_rqst_msgs_cnt_dis[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_resp_msgs_cnt_dis[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_unso_msgs_cnt_dis[MAX_CONN_PLUS_ONE];

        /* Outstanding messages within a single PDU */

        ubyte_4 srv_data_rqsts_outstanding[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_resps_outstanding[MAX_CONN_PLUS_ONE];
        ubyte_4 srv_data_unsos_outstanding[MAX_CONN_PLUS_ONE];

/* read/write byte and error counts                                           */

        unsigned long int conn_read_byte_count[MAX_CONN_PLUS_ONE];
        unsigned long int conn_read_byte_count_dis[MAX_CONN_PLUS_ONE];
                                                              /* display only */
        unsigned long int conn_write_byte_count[MAX_CONN_PLUS_ONE];
        unsigned long int conn_write_byte_count_dis[MAX_CONN_PLUS_ONE];
                                                              /* display only */
        unsigned long int conn_read_err_count[MAX_CONN_PLUS_ONE];
        unsigned long int conn_read_err_count_dis[MAX_CONN_PLUS_ONE];
                                                              /* display only */
        unsigned long int conn_write_err_count[MAX_CONN_PLUS_ONE];
        unsigned long int conn_write_err_count_dis[MAX_CONN_PLUS_ONE];
                                                              /* display only */

/* number of times a connection's socket actually had something to be read    */

        unsigned long int conn_read_select_success_count[MAX_CONN_PLUS_ONE];
        unsigned long int conn_read_select_success_count_dis[MAX_CONN_PLUS_ONE];
                                                              /* display only */

/* Statistics information across ALL connections which is accumulated ONLY    */
/* over the statistics interval                                               */

/* number of times a select statement is executed when we intend to do a read */
/* operation                                                                  */

        unsigned long int read_select_count;
        unsigned long int read_select_count_dis;              /* display only */

/* number of times ANY socket actually had something to be read.  This        */
/* counter is only incremented once for a single read select, even if         */
/* multiple bits are set in the read mask returned by the select.             */

        unsigned long int read_select_success_count;
        unsigned long int read_select_success_count_dis;      /* display only */

/* Statistics information relating to GTMS which is accumulated ONLY over the */
/* statistics interval                                                        */


/* total number of oltp messages the MX Server has received from the game     */

        unsigned long int oltp_msgs_from_game;
        unsigned long int oltp_msgs_from_game_dis;            /* display only */

/* total number of RPC messages the MX Server has received from the game      */

        unsigned long int rpc_msgs_from_game;
        unsigned long int rpc_msgs_from_game_dis;             /* display only */

/* number of times the TNI main function executes gtms_receive                */

        unsigned long int gr_receive_count;
        unsigned long int gr_receive_count_dis;               /* display only */

/* number of times we read from GTMS and had no message to receive            */

        unsigned long int gr_nomsg_count;
        unsigned long int gr_nomsg_count_dis;                 /* display only */

        int print_flag;

/* accumulated times OLTP transactions were is game                           */

        int num_oltp_resps;
        int tot_oltp_resp_time;
        int min_oltp_resp_time;
        int max_oltp_resp_time;

        int ave_oltp_resp_time_dis;
        int min_oltp_resp_time_dis;
        int max_oltp_resp_time_dis;

/* ES RPC statistics totals for the day                                          */

        int tot_rpc_rqsts_ack;
        int tot_rpc_rqsts_noack;
        int tot_rpc_rqsts_sent;
        int tot_rpc_resps;
        int tot_rpc_resps_rcvd;

        int tot_rpc_rqst_failures;
        int tot_rpc_rqst_timeouts;
        int tot_rpc_resp_failures;
        int tot_rpc_rqsts_wrt_lost;
        int tot_rpc_resp_late;
        int tot_rpc_err_pdus_rcvd;

/* ES RPC statistics over the last interval                                      */

        int num_rpc_rqsts_ack;
        int num_rpc_rqsts_noack;
        int num_rpc_rqsts_sent;
        int num_rpc_resps;
        int num_rpc_resps_rcvd;

        int num_rpc_rqst_failures;
        int num_rpc_rqst_timeouts;
        int num_rpc_resp_failures;
        int num_rpc_resp_late;
        int num_rpc_err_pdus_rcvd;
        int num_rpc_rqsts_wrt_lost;

        int tot_rpc_resp_time;
        int min_rpc_resp_time;
        int max_rpc_resp_time;

/* ES RPC statistics over the last interval display variables                    */

        int num_rpc_rqsts_ack_dis;
        int num_rpc_rqsts_noack_dis;
        int num_rpc_rqsts_sent_dis;
        int num_rpc_resps_rcvd_dis;
        int num_rpc_resps_dis;

        int num_rpc_rqst_failures_dis;
        int num_rpc_rqst_timeouts_dis;
        int num_rpc_wrt_failures_dis;
        int num_rpc_resp_failures_dis;
        int num_rpc_resp_late_dis;
        int num_rpc_err_pdus_rcvd_dis;

        int ave_rpc_resp_time_dis;
        int min_rpc_resp_time_dis;
        int max_rpc_resp_time_dis;

/* HOST RPC statistics totals for the day                                          */

        int tot_rpc_rqsts_rcvd;
        int tot_rpc_resps_sent;
        int tot_rpc_resps_wrt_lost;
        int tot_rpc_err_pdus_sent;
        int tot_hostrpc_resp_failures;

/* HOST RPC statistics over the last interval                                      */

        int num_rpc_rqsts_rcvd;
        int num_rpc_resps_sent;
        int num_rpc_resps_wrt_lost;
        int num_rpc_err_pdus_sent;
        int num_hostrpc_resp_failures;

        int tot_hostrpc_resp_time;
        int min_hostrpc_resp_time;
        int max_hostrpc_resp_time;

/* HOST RPC statistics over the last interval display variables                    */

        int num_rpc_rqsts_rcvd_dis;
        int num_rpc_resps_sent_dis;
        int num_rpc_resps_wrt_lost_dis;
        int num_rpc_err_pdus_sent_dis;
        int num_hostrpc_resp_failures_dis;

        int ave_hostrpc_resp_time_dis;
        int min_hostrpc_resp_time_dis;
        int max_hostrpc_resp_time_dis;

/* debug information                                                          */

        FILE *dbg_p;
        int dbg_state;
        int dbg_term_num;

/* logging information                                                        */

        FILE *log_p;
        char log_file_name[64];
        int log_state;
        time_t excess_log_check_time; 
        unsigned int amount_logged;
        unsigned int log_threshold;

/* MJF or TMF (master file) logging information */

        int mf_log_state;

    } TNICON;

#endif                                /* TNI_GBLSEC_LOADED                    */
