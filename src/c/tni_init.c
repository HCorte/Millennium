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
/*====[TNI_INIT.C]============================================================*/
/*                                                                            */
/* Tni_Init()                                                                 */
/*                                                                            */
/* Purpose:  Initialize all necessary components of the TNI task.             */
/*           Read the configuration file.                                     */
/*           This fuction will create and initialize the needed               */
/*           pools of buffers required by the mbuf routines                   */
/*           Currently five (5) pools will be required                        */
/*             1.) mbhdrpool                                                  */
/*             2.) smallpool                                                  */
/*             3.) termpool                                                   */
/*             4.) ethpool                                                    */
/*             5.) ethlrgpool                                                 */
/*                                                                            */
/* Input Arguments:     none                                                  */
/*                                                                            */
/* Output Arguments:    none                                                  */
/*                                                                            */
/* Return Value:        status       1 - initialization complete              */
/*                                   0 - initialization failure               */
/* Assumptions:         none                                                  */
/*                                                                            */
/*====[TNI_INIT.C]============================================================*/
/*                                                                            */

#include "includes_mbuf.h"
#include "tnicfg.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include "bufpool.h"
#   include "gdata.h"

#elif defined(XOS_VMS)

#   include "bufpool.h"
#   include "gdata.h"

#else

#   error - OS-specific logic not handled.

#endif

#define ALLOCATE

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include "bufcfg.h"

#elif defined(XOS_VMS)

#   include "bufcfg.h"

#else

#   error - OS-specific logic not handled.

#endif

#undef ALLOCATE

int  Tni_Init() {
    int conn;
    int app_idx;
    int status;
    TNICON *ptr;

    err_string = null_err_string;

    /* initialize TNI protocol version list */

    tni_proto_versions[0] = TNI_VERSION_30;
    tni_proto_versions[1] = TNI_VERSION_23;
    tni_proto_versions[2] = TNI_VERSION_22;
    tni_proto_versions[3] = TNI_VERSION_21;
    tni_proto_versions[4] = TNI_VERSION_02;
    tni_proto_versions[5] = TNI_VERSION_01;

    /* initialize global connection variables */

    ptr = tnicon_p;

    memset(ptr->local_domain_name,
           0x00,
           sizeof(ptr->local_domain_name));

    memset(ptr->keystore_file,
           0x00,
           sizeof(ptr->keystore_file));

    memset(ptr->publickey_file,
           0x00,
           sizeof(ptr->publickey_file));

    ptr->x_encrypt_key = NULL;
    ptr->x_decrypt_key = NULL;

/* initialize connection variables */

    for( conn = 1; conn <= MAX_CONN; conn++ ) {

        memset(ptr->connection[conn].local_domain_name,
               0x00,
               sizeof(ptr->connection[conn].local_domain_name));

        memset(ptr->connection[conn].remote_node_name, 
               0x00, 
               sizeof(ptr->connection[conn].remote_node_name));

        memset(ptr->connection[conn].remote_domain_name, 
               0x00, 
               sizeof(ptr->connection[conn].remote_domain_name));

        memset(ptr->connection[conn].session_tag,
               '\0',
               sizeof(ptr->connection[conn].session_tag));

        ptr->connection[conn].client_id_val = -1;
        ptr->connection[conn].client_id_idx = -1;
        ptr->connection[conn].managed_by = -1;
        ptr->connection[conn].sock = -1;
        ptr->connection[conn].alt_conn = 0;
        ptr->connection[conn].app_idx = -1;
        ptr->connection[conn].conn_state = CONN_UNDEFINED;
        ptr->connection[conn].last_client_state = NOT_PRIMARY;
        ptr->connection[conn].tni_proto_ver = TNI_VERSION_UNKNOWN;
        ptr->connection[conn].max_messages_per_pdu = DEF_MAX_MGS_PER_PDU;
        ptr->connection[conn].max_pdu_size = DEF_PDU_SIZE;
        ptr->connection[conn].messaging_type = DEF_MESSAGING_TYPE;
        ptr->connection[conn].auth_state  = AUTH_NOTATTEMPTED;
        ptr->connection[conn].auth_hash_len = -1;
        ptr->connection[conn].consec_auth_failure_cnt = 0;
        ptr->connection[conn].enc_key_x_method = METH_ENC_KEY_X_NONE;
        ptr->connection[conn].enc_key_type = ENC_KEY_NONE;
        ptr->connection[conn].enc_state = ENC_NOT_CONFIGURED;
        ptr->connection[conn].enc_key = NULL;

        memset(ptr->connection[conn].auth_hash_value, 
                0x00, 
                sizeof(ptr->connection[conn].auth_hash_value));

        memset(&ptr->connection[conn].blocking_time,
               0x00,
               sizeof(ptr->connection[conn].blocking_time));

        memset(&ptr->connection[conn].keepalive_time,
               0x00,
               sizeof(ptr->connection[conn].keepalive_time));

        memset(&ptr->connection[conn].conn_alive_timeout,
               0x00,
               sizeof(ptr->connection[conn].conn_alive_timeout));

        memset(&ptr->connection[conn].time_last_sent, 
               0x00, 
               sizeof(ptr->connection[conn].time_last_sent));

        memset(&ptr->connection[conn].time_last_rcvd, 
               0x00, 
               sizeof(ptr->connection[conn].time_last_rcvd));

        memset(&ptr->connection[conn].time_last_block_sent,
               0x00,
               sizeof(ptr->connection[conn].time_last_block_sent));

        memset(&ptr->connection[conn].last_conn_atmpt_time, 
               0x00, 
               sizeof(ptr->connection[conn].last_conn_atmpt_time));

        memset(&ptr->connection[conn].last_auth_atmpt_time, 
               0x00, 
               sizeof(ptr->connection[conn].last_auth_atmpt_time));

    } /* end for conn = 0 */

/* initialize application variables */

    for (app_idx = 0; app_idx < MAX_APPS; app_idx++ ) {
 
        ptr->app[app_idx].state = APP_UNDEFINED;
        ptr->app[app_idx].rpc_rqst_timeout = 0;
        ptr->app[app_idx].oltp_unso_routing = OLTP_ROUTE_NONE;
        ptr->app[app_idx].oltp_rqst_routing = OLTP_ROUTE_NONE;
        ptr->app[app_idx].client_conn_cnt = 0;
        ptr->app[app_idx].next_unso_client_idx = 0;
        ptr->app[app_idx].auth_required = 0;
        ptr->app[app_idx].auth_hash_algorithm = DEF_AUTH_HASH_ALGORITHM;
        ptr->app[app_idx].auth_salt_length = DEF_AUTH_SALT_LENGTH;
        ptr->app[app_idx].auth_lockout_state = APP_AUTH_NOTLOCKED;
        ptr->app[app_idx].es_rpc_enc_mode = ENC_DISABLED;

        memset(&ptr->app[app_idx].auth_lockout_release_time,
                0x00,
                sizeof(ptr->app[app_idx].auth_lockout_release_time));

        for( conn = 0; conn < MAX_CONN_PER_APP; conn++ ) {

            ptr->app[app_idx].client_conn[conn] = 0;
        }

        memset(ptr->app[app_idx].name,
               0x00,
               sizeof(ptr->app[app_idx].name));

    }

/* initialize client id variables */

    memset(&ptr->client_conns[0][0],
           0x00,
           sizeof (ptr->client_conns));

    memset(&ptr->client_idx_tbl[0],
           0x00,
           sizeof (ptr->client_idx_tbl));

    memset(&ptr->sorted_client_ids[0],
           0x00,
           sizeof (ptr->sorted_client_ids));

    memset(&ptr->last_oltp_conn_sent[0],
           0x00,
           sizeof (ptr->last_oltp_conn_sent));

/* initialize maximum number of terminal for system */

#   if defined(PROSYS_ENV_ALL)

        max_terminal = volcom_p->sys.mxtrm;

#   endif

#   if defined(GOLS_ENV_ALL)

        GET_MAX_TERMS (&max_terminal);

#   endif

/* initialize mbuf pool parameters to their default values */

    pool1_buf_size  = DEF_POOL1_BUF_SIZE;
    pool1_buf_num   = DEF_POOL1_BUF_NUM;
    pool1_buf_thold = DEF_POOL1_BUF_THOLD;

    pool2_buf_size  = DEF_POOL2_BUF_SIZE;
    pool2_buf_num   = DEF_POOL2_BUF_NUM;
    pool2_buf_thold = DEF_POOL2_BUF_THOLD;

    pool3_buf_size  = DEF_POOL3_BUF_SIZE;
    pool3_buf_num   = DEF_POOL3_BUF_NUM;
    pool3_buf_thold = DEF_POOL3_BUF_THOLD;

    pool4_buf_size  = DEF_POOL4_BUF_SIZE;
    pool4_buf_num   = DEF_POOL4_BUF_NUM;
    pool4_buf_thold = DEF_POOL4_BUF_THOLD;

    pool5_buf_size  = DEF_POOL5_BUF_SIZE;
    pool5_buf_num   = DEF_POOL5_BUF_NUM;
    pool5_buf_thold = DEF_POOL5_BUF_THOLD;

    if(pool5_buf_size > 0)
    {
        max_pdu_size_val = pool5_buf_size;
    }
    else
    {
        max_pdu_size_val = DEF_MAX_PDU_SIZE_VAL;
    }

/* initialize statistics             */

    Initialize_Statistics();

    ptr->min_rpc_resp_time_dis = 99999;

    if ( (status = Tni_Setup()) )
    {
        Calc_Offsets();   
    } /* end if status */

    /* initialize debug and log functions */

    tnicon_p->print_flag = 0;
    tnicon_p->dbg_state = DBG_OFF;
    tnicon_p->dbg_term_num = 0;

    sprintf(tnicon_p->log_file_name, "Undefined");
    tnicon_p->log_threshold = LOGGING_THRESHOLD;

/* Initialize the buffer pools to be used by TNI         */

    gtx_debug = 0;
	
    sprintf(pool_1.name, "pool1");
    pool_1.nbuf  = pool1_buf_num;
    pool_1.dsize = pool1_buf_size;
    pool_1.thold = pool1_buf_thold;
    bufp_config (POOL1_BUF_POOL, &pool_1);

    sprintf(pool_2.name, "pool2");
    pool_2.nbuf  = pool2_buf_num;
    pool_2.dsize = pool2_buf_size;
    pool_2.thold = pool2_buf_thold;
    bufp_config (POOL2_BUF_POOL, &pool_2);

    sprintf(pool_3.name, "pool3");
    pool_3.nbuf  = pool3_buf_num;
    pool_3.dsize = pool3_buf_size;
    pool_3.thold = pool3_buf_thold;
    bufp_config (POOL3_BUF_POOL, &pool_3);

    sprintf(pool_4.name, "pool4");
    pool_4.nbuf  = pool4_buf_num;
    pool_4.dsize = pool4_buf_size;
    pool_4.thold = pool4_buf_thold;
    bufp_config (POOL4_BUF_POOL, &pool_4);

    sprintf(pool_5.name, "pool5");
    pool_5.nbuf  = pool5_buf_num;
    pool_5.dsize = pool5_buf_size;
    pool_5.thold = pool5_buf_thold;
    bufp_config (POOL5_BUF_POOL, &pool_5);

    bufp_init();

/* Only if Tni_Setup was successful */

    if (status)
    {
#    if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        if (P_FAILURE == Load_Exchange_Enc_Key())
        {
            for (app_idx = 0; app_idx < MAX_APPS; app_idx++ )
            {
                if (ptr->app[app_idx].es_rpc_enc_mode != ENC_DISABLED)
                {
                     ptr->app[app_idx].es_rpc_enc_state = ENC_DISABLED;
                }
            }
        }

#   endif

#   if defined(PROSYS_ENV_ALL)

        if (create_events_section(PROD_NBR,
                              tnicon_p->max_es_requests))

#   endif

#   if defined(GOLS_ENV_ALL)

        if (create_events_section(tnicon_p->max_es_requests))

#   endif
        {
/* initialize event queues */

            Initialize_Event_Queue(EVENT_FREE_QUEUE, event_free_queue);

            Initialize_Event_Queue(RPC_REQUEST_QUEUE, rpc_events_queue);

            status = Initialize_Request_Event_List(request_events);
        }
        else
        {
            status = 0;
        }
    }

    return(status);
} /* end Tni_init */
