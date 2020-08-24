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
/*====[TNI_CLIENT_REQUEST.C]==================================================*/
/*                                                                            */
/* Client_Request (struct PDU_STRUCT *pdu_struct,                             */
/*                 long int pdulen,                                           */
/*                 unsigned short *conn)                                      */
/*                                                                            */
/* Purpose: This function parses the Variable Data Part of a single Client    */
/*          Parameter Request PDU contained in pdu_struct, validates the      */
/*          parameters, loads their values into the global section, and sends */
/*          the Client Parameter Response PDU.  In addition, this function    */
/*          checks for a possible dual primary situation and changes the      */
/*          input connection state.                                           */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Client Parameter  */
/*                           Request PDU, i.e. PDU fixed header part followed */
/*                           by variable data part.                           */
/*                                                                            */
/*          pdulen           PDU length                                       */
/*                                                                            */
/* Output Arguments:                                                          */
/*                                                                            */
/*          conn             Connection number assigned to client             */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: The PDU Fixed Header has been validated by the calling        */
/*              function prior to entering this function.                     */
/*                                                                            */
/*====[TNI_CLIENT_REQUEST.C]==================================================*/
/*                                                                            */

#include <stdio.h>
#include <string.h>

#include "includes_mbuf.h"

void
Client_Request (struct PDU_STRUCT *pdu_struct,
                long int pdulen,
                unsigned short *conn)
{
    unsigned short assigned_conn = 0;
    int act_conn;                          /* Actual connection PDU was sent  */
    int app_num = -1;
    int offset;                            /* offset to each parameter        */
                                           /* code/length/value block         */
                                           /* contained in the var part of the*/
    int parm_good;                         /* status of the check_paramter    */
    int parm_count;
    int msg_type;                          /* messaging type                  */
    int client_id_val;                     /* client id as it appears in PDU  */
    int rc = P_SUCCESS;                    /* generic return code             */
    int enc_key_x_method;                  /* encryption key exchange method  */
    int enc_key_type;                      /* encryption key type             */

    long int length;                       /* number of bytes in a single     */
                                           /* parameter unit, including code, */
                                           /* length, value fields            */

    long int value;                        /* generic parameter value returned*/
                                           /* by Check_Parameter              */
    long int max_msgs;                     /* specific parameter values, based*/
    long int pdu_size;                     /* upon parameter code             */
    long int blocking_time_tics;           /* 10 millisecond tics             */
    long int keepalive_time_secs;          /* seconds                         */
    long int primary_status;
    long int term_method;                  /* terminal identification method  */
                                           /* client wants to us as it appears*/
                                           /* appearsin PDU                   */
    long int result_code;

    char next_state;                       /* next valid state                */
    char string_val[MAX_RPC_RQST_TAG_LEN + 1];
    char app_name[MXSRV_MAX_APP_NAME_LEN + 1];
    unsigned char param_code;              /* PDU parameter code              */

    struct TNI_DATA *param_data;           /* pointer to a parameter          */
                                           /* code/length/value block         */
    struct mbuf *mymbuf;                   /* pointer to mbuf containing      */
                                           /* CLient Parameter Response PDU   */
    struct TNI_PARAM_PAIR params[13];      /* sized by max number of PDU      */
                                           /* parameters allowed in TNI       */

    mymbuf = NULL;   

    err_string = null_err_string;

    Print_Pdu ("client request",pdu_struct,pdulen);

/* Initialize all valid parameters to -1 so we can determine which parameters */
/* are present in the PDU.  All parameters can be defaulted except for        */
/* application name.                                                          */

    parm_count = 0;
    client_id_val = -1;
    max_msgs = -1;
    pdu_size = -1;
    blocking_time_tics = -1;
    keepalive_time_secs = -1;
    primary_status = -1;
    term_method = -1;
    result_code = -1;
    msg_type = -1;
    enc_key_x_method = -1;
    enc_key_type = -1;
    app_name[0] = '\0';

/* Assume that first byte following fixed header is a parameter block         */

    for( offset = 0; offset < (pdulen - sizeof (struct TNI_FIXED_HDR)) ;
       offset+= length ) {

        param_data = (struct TNI_DATA *)&pdu_struct->msg_data[offset];

        memset(string_val, 0x00, MAX_RPC_RQST_TAG_LEN + 1);

        parm_good = Check_Parameter (*conn,
                                     param_data, 
                                     &param_code, 
                                     &length, 
                                     &value,
                                     string_val);

        if( !parm_good ) {

            LogPdu(pdu_struct, -1, *conn);

            rc = send_invalid_pdu_error (*conn);

            return;
        }

/*    length returned by check_parameter is only the parameter value length   */
/*    i.e. it excludes the size of the parameter code and parameter length    */
/*    fields                                                                  */

        length += sizeof(param_data->param_code) +
                  sizeof(param_data->param_length);

        switch( param_code ) {
        
        case CLIENT_ID:
            client_id_val = value;
            break;
        case MAX_MESSAGE_SIZE:
            max_msgs = value;
            break;
        case MAX_PDU_SIZE:
            pdu_size = value;
            break;
        case MSG_BLOCK_TIME:
            blocking_time_tics = value;
            break;
        case KEEP_ALIVE_TIME:
            keepalive_time_secs = value;
            break;
        case PRIMARY_STATUS:
            primary_status = value;
            break;
        case TERM_METHOD:
            term_method = value;
            break;
        case APPLICATION_NAME:
            strncpy(app_name, string_val, MXSRV_MAX_APP_NAME_LEN + 1);
            break;
        case MESSAGING_TYPE:
            msg_type = value;
            break;
        case ENC_KEY_X_METHOD:
            enc_key_x_method = value;
            break;
        case ENC_KEY_TYPE:
            enc_key_type = value;
            break;

        }                                          /* end switch (param_code) */
    }                                              /* end for                 */

/* Client Id is mandatory in TNI protocol versions 1 and 2.  Client Id is     */
/* optional in TNI protocol version 2.1 and above.                            */

    if (client_id_val != -1)
    {
        if (!((MIN_CLIENT_ID_VAL <= client_id_val) &&
              (MAX_CLIENT_ID_VAL >= client_id_val)))
        {
            result_code = INVALID_PDU;
        }

        params[parm_count].param_code = CLIENT_ID;
        params[parm_count].param_value = client_id_val;
        parm_count++;
    }
    else
    {
        if ((tnicon_p->connection[*conn].tni_proto_ver == TNI_VERSION_01) ||
            (tnicon_p->connection[*conn].tni_proto_ver == TNI_VERSION_02))
        {
            result_code = INVALID_PDU;
        }
    }

    if (result_code != INVALID_PDU)
    {
        assigned_conn = *conn;

        if( Assign_Connection(client_id_val, app_name, &assigned_conn) ) {

            if (assigned_conn != *conn)
            {
                *conn = assigned_conn;
            }
            tnicon_p->clt_param_req_cnt_tot[*conn]++;

/*    Initially set the result code to good status.  If a PDU is correctly    */
/*    formatted, then the only errors a client expects from the CLIENT        */
/*    RESPONSE PDU are APP_NO_CONFIG or INVALID TERMINAL METHOD.              */

            result_code = SUCCESS;

/*    client_id is valid so process remaining parameters.  A parameter is set */
/*    to its default value if it's either not specified or out of range       */

            if (max_msgs == -1) {

                tnicon_p->connection[*conn].max_messages_per_pdu = 
                    DEF_MAX_MGS_PER_PDU;

            } else if (!((MIN_MAX_MGS_PER_PDU_VAL <= max_msgs) &&
                  (max_msgs <= MAX_MAX_MGS_PER_PDU_VAL)) ) {

                result_code = INVALID_PDU;

            } else {
                tnicon_p->connection[*conn].max_messages_per_pdu = max_msgs;

                params[parm_count].param_code = MAX_MESSAGE_SIZE;
                params[parm_count].param_value = max_msgs;
                parm_count++;
            }

            if (pdu_size == -1) {

                tnicon_p->connection[*conn].max_pdu_size = DEF_PDU_SIZE;
                pdu_size = DEF_PDU_SIZE;

            } else if (!((MIN_PDU_SIZE_VAL <= pdu_size) && 
                  (pdu_size <= max_pdu_size_val)) ) {

                result_code = INVALID_PDU;

            } else {
                tnicon_p->connection[*conn].max_pdu_size = pdu_size;

                params[parm_count].param_code = MAX_PDU_SIZE;
                params[parm_count].param_value = pdu_size;
                parm_count++;
            }

            if (blocking_time_tics == -1) {

                tnicon_p->connection[*conn].blocking_time =
                Set_Timervalue (DEF_BLOCKING_TIME);
                blocking_time_tics = DEF_BLOCKING_TIME;

            } else if (!((MIN_BLOCKING_TIME_VAL <= blocking_time_tics) &&
                  (blocking_time_tics <= MAX_BLOCKING_TIME_VAL)) ) {

                result_code = INVALID_PDU;

            } else {

                tnicon_p->connection[*conn].blocking_time = 
                Set_Timervalue (blocking_time_tics);

                params[parm_count].param_code = MSG_BLOCK_TIME;
                params[parm_count].param_value = blocking_time_tics;
                parm_count++;
            }

            if (keepalive_time_secs == -1) {

                tnicon_p->connection[*conn].keepalive_time =
                    Set_Timervalue (DEF_KEEPALIVE_TIME *
                                    TICS_PER_SEC);
                keepalive_time_secs = DEF_KEEPALIVE_TIME;

                tnicon_p->connection[*conn].conn_alive_timeout = 
                Set_Timervalue (keepalive_time_secs * 3 * TICS_PER_SEC);

            } else if (!((MIN_KEEPALIVE_TIME_VAL <= keepalive_time_secs) &&
                  (keepalive_time_secs <= MAX_KEEPALIVE_TIME_VAL)) ) {

                result_code = INVALID_PDU;

            } else {
                tnicon_p->connection[*conn].keepalive_time = 
                Set_Timervalue (keepalive_time_secs *
                                TICS_PER_SEC);

                tnicon_p->connection[*conn].conn_alive_timeout = 
                Set_Timervalue (keepalive_time_secs * 3 * TICS_PER_SEC);

                params[parm_count].param_code = KEEP_ALIVE_TIME;
                params[parm_count].param_value = keepalive_time_secs;
                parm_count++;
            }

            if( term_method == -1 ) {

                term_method = tnicon_p->def_term_id_meth;
                tnicon_p->connection[*conn].term_id_method = 
                    tnicon_p->def_term_id_meth;
            } else {

                switch (term_method) 
                {

                    case METH_TERM_CLIENT_ID:
                    case METH_TERM_CLIENT_TAG:
                    case METH_TERM_SERVER_ID:

                        tnicon_p->connection[*conn].term_id_method =
                            (unsigned char) term_method;

                        break;

                    default:

                        result_code = INV_TERM_METHOD;
                        break;
                }

                params[parm_count].param_code = TERM_METHOD;
                params[parm_count].param_value = term_method;
                parm_count++;
            }

            if (tnicon_p->connection[*conn].tni_proto_ver != TNI_VERSION_01)
            {
                if (app_name != NULL)
                {
                    params[parm_count].param_code = APPLICATION_NAME;
                    strncpy(params[parm_count].param_value_char,
                            app_name,
                            MXSRV_MAX_APP_NAME_LEN + 1);
                    parm_count++;
                }

                if (msg_type == -1)
                {
                    tnicon_p->connection[*conn].messaging_type = 
                       DEF_MESSAGING_TYPE;
                }
                else if (!((MIN_MESSAGING_TYPE <= msg_type) &&
                          (msg_type <= MAX_MESSAGING_TYPE)) )
                {
                    result_code = INVALID_PDU;
                }
                else
                {
                    tnicon_p->connection[*conn].messaging_type =
                        msg_type;

                    params[parm_count].param_code = MESSAGING_TYPE;
                    params[parm_count].param_value = msg_type; 
                    parm_count++;
                }
            }

            if (tnicon_p->connection[*conn].tni_proto_ver >= TNI_VERSION_30)
            {
                if (enc_key_x_method == -1)
                {
                    tnicon_p->connection[*conn].enc_key_x_method =
                       DEF_ENC_KEY_X_METHOD;
                }
                else if (!((MIN_ENC_KEY_X_METHOD <= enc_key_x_method) &&
                          (enc_key_x_method <= MAX_ENC_KEY_X_METHOD)) )
                {
                    result_code = INVALID_PDU;
                }
                else
                {
                    tnicon_p->connection[*conn].enc_key_x_method =
                        enc_key_x_method;

                    params[parm_count].param_code = ENC_KEY_X_METHOD;
                    params[parm_count].param_value = enc_key_x_method;
                    parm_count++;
                }

                if (enc_key_type == -1)
                {
                    tnicon_p->connection[*conn].enc_key_type =
                       DEF_ENC_KEY_TYPE;
                }
                else if (!((MIN_ENC_KEY_TYPE <= enc_key_type) &&
                          (enc_key_type <= MAX_ENC_KEY_TYPE)) )
                {
                    result_code = INVALID_PDU;
                }
                else if ((enc_key_type != ENC_KEY_NONE) &&
                         (enc_key_type != ENC_KEY_AES_128_CBC) &&
                         (enc_key_type != ENC_KEY_AES_256_CBC))
                {
                    result_code = ENC_KEY_TYPE_NOSUP;
                }
                else
                {
                    tnicon_p->connection[*conn].enc_key_type =
                        enc_key_type;

                    params[parm_count].param_code = ENC_KEY_TYPE;
                    params[parm_count].param_value = enc_key_type;
                    parm_count++;
                }
            }

/* Always process the Primary Status parameter last because it changes the    */
/* state of the connection!  Also make sure all of the other parameters have  */
/* been processed successfully before attempting Primary Status processing.   */
/* Primary status value is out of range.  Don't change anything on the server */
/* just return a bad result code to the client.                               */

            if (result_code == SUCCESS)
            {
                if (primary_status == -1)
                {                         /* not specified, assume not primary*/
                    primary_status = DEF_PRIMARY_STATUS;
                }

                if (!((MIN_PRIMARY_STATUS_VAL <= primary_status) &&
                      (primary_status <= MAX_PRIMARY_STATUS_VAL)))
                {
                    result_code = INVALID_PDU;
                }
                else
                {
                    next_state = Get_Next_State (*conn,
                                                 tnicon_p->tni_server_state,
                                                 primary_status);
                    if( next_state == -1 )
                    {
                        output_err("Client_Request",
                                   MI_TNI_INV_STATE,
                                   MX_ERR_LVL_ERROR,
                                   err_string);

                    }
                    else if (next_state != 0)
                    {
                        tnicon_p->connection[*conn].conn_state = next_state;
                        tnicon_p->connection[*conn].last_client_state =
                            primary_status;
                    }

                    params[parm_count].param_code = PRIMARY_STATUS;
                    params[parm_count].param_value = primary_status;
                    parm_count++;
                }
            }

            if (result_code == SUCCESS)
            {
                if ((app_name != NULL) &&
                    (tnicon_p->connection[*conn].tni_proto_ver == 
                     TNI_VERSION_02))
                {
                    strcpy(tnicon_p->connection[*conn].application_name,
                           app_name);

                    if (tnicon_p->connection[*conn].messaging_type ==
                        MSG_TYPE_ES_RPC)
                    {
                        app_num = Find_Application_Number(app_name);

                        if (app_num == -1)
                        {
                            app_num = Allocate_Application ();

                            if (app_num != -1)
                            {
                                memcpy((void *)tnicon_p->app[app_num].name,
                                       (void *) app_name,
                           (size_t) _MIN(sizeof(tnicon_p->app[app_num].name)-1,
                                                strlen (app_name)));

                               tnicon_p->app[app_num].rpc_rqst_timeout =
                                   DEF_RPC_RQST_TIMEOUT;
                               tnicon_p->app[app_num].oltp_unso_routing =
                                   OLTP_ROUTE_NONE;
                               tnicon_p->app[app_num].oltp_rqst_routing =
                                   OLTP_ROUTE_NONE;
                            }
                            else
                            {
                                result_code = APP_NO_CONFIG;
                            }
                        }

                        if (Assign_Conn_To_App (*conn, app_num) == P_FAILURE)
                        {
                            result_code = APP_NO_CONFIG;
                        }
                        else
                        {
                            tnicon_p->connection[*conn].app_idx = app_num;
                        }
                    }
                }
            }
        }
        else
        {
            result_code = APP_NO_CONFIG;
        }
    }                                         /* end else valid client id     */

/* Now load all the parameters allowed on the response and build the response */

    if (result_code != -1) {

        params[parm_count].param_code = RESULT_CODE;
        params[parm_count].param_value = result_code;
        parm_count++;

        params[parm_count].param_code = 0;            /* NULL terminated list */
        params[parm_count].param_value = 0;

        mymbuf = Build_Pdu (*conn, CLIENT_RESPONSE_PDU, NULL, &params[0]);

        /* Send response on the same connection on which CLIENT REQUEST PDU   */
        /* was received                                                       */

        act_conn = Send_To_Conn (mymbuf, *conn, NO_ALT_CONN);

        if (act_conn == 0) {

            sprintf(err_string.par1,"CLIENT PARAMETER RESPONSE");
            sprintf(err_string.par2,"%d",*conn);

            output_err("Client_Request",
                       MI_TNI_ERR_SEND,
                       MX_ERR_LVL_ERROR,
                       err_string);
        } else {

            tnicon_p->clt_param_resp_cnt_tot[act_conn]++;

            if (result_code == APP_NO_CONFIG) {

                Close_Connection (*conn);
            }
        }
    } else {

        Close_Connection (*conn);
    }

/* Send Server Challenge Request */

    if (result_code == SUCCESS && 
        tnicon_p->connection[*conn].tni_proto_ver >= TNI_VERSION_23 &&
        tnicon_p->connection[*conn].auth_state != AUTH_ATTEMPTED &&
        tnicon_p->connection[*conn].app_idx != -1 )
    {
        Server_Challenge_Request(*conn);
    }

/* Send Server Encryption Key Request */

    if (result_code == SUCCESS &&
        tnicon_p->exchange_enc_key_state == ENC_AVAILABLE &&
        tnicon_p->app[tnicon_p->connection[*conn].app_idx].es_rpc_enc_state == ENC_CONFIGURED &&
        tnicon_p->connection[*conn].tni_proto_ver >= TNI_VERSION_30 &&
        tnicon_p->connection[*conn].enc_key_x_method != METH_ENC_KEY_X_NONE &&
        tnicon_p->connection[*conn].enc_key_type != ENC_KEY_NONE &&
        tnicon_p->connection[*conn].enc_state == ENC_NOT_CONFIGURED)
    {
        Send_Srv_Enc_Key_Rqst(*conn);
    }
}
