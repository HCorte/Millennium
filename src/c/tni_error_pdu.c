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
/*====[TNI_ERROR_PDU.C]=======================================================*/
/*                                                                            */
/* Error_Pdu (struct PDU_STRUCT *pdu_struct,                                  */
/*            long int pdulen,                                                */
/*            unsigned short conn)                                            */
/*                                                                            */
/* Purpose: This function parses the Variable Data Part of a single Error PDU */
/*          contained in pdu_struct and sends the error message to the error  */
/*          logging task                                                      */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Error PDU,        */
/*                           i.e. PDU fixed header part followed by variable  */
/*                           data part                                        */
/*                                                                            */
/*          pdulen           PDU length                                       */
/*                                                                            */
/*          conn             Connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: The PDU Fixed Header has been validated by the calling        */
/*              function prior to entering this function.                     */
/*                                                                            */
/*====[TNI_ERROR_PDU.C]=======================================================*/
/*                                                                            */

#include <string.h>

#include "includes.h"

void Error_Pdu (struct PDU_STRUCT *pdu_struct, long int pdulen,
                unsigned short conn) {
    unsigned char param_code;              /* PDU parameter code              */
    struct TNI_DATA *param_data;           /* pointer to a parameter          */
                                           /* code/length/value block         */
    char string_val[MAX_RPC_RQST_TAG_LEN + 1]; 
    char term_client_tag[MAX_TERM_CLIENT_TAG_LEN];
    char rpc_request_tag[MAX_RPC_RQST_TAG_LEN + 1];

    int app_num = 0;
    int offset;                            /* offset to each parameter        */
                                           /* code/length/value block         */
                                           /* contained in the var part of the*/
    int parm_good;                         /* PDU status of the check_paramter*/
    int rc = P_SUCCESS;                    /* generic return code             */

    long int length;                       /* number of bytes in a single     */
                                           /* parameter unit, including code, */
                                           /* length, value fields            */
    long int value;                        /* generic parameter value returned*/
                                           /* by Check_Parameter              */
    long int term_client_id;               /* specific parameter values, based*/
    long int term_server_id;
    long int err_code;

    struct ERR_STRING local_err_string;

    err_string = null_err_string;
    local_err_string = null_err_string;

    string_val[0] = '\0';
    term_client_tag[0] = '\0';
    rpc_request_tag[0] = '\0';

    Print_Pdu ("error data",pdu_struct,pdulen);

    tnicon_p->error_rcvd_cnt_tot[conn]++;

/* Assume that first byte following fixed header is a parameter block         */

    for( offset = 0; offset < (pdulen - sizeof (struct TNI_FIXED_HDR)) ;
       offset+= length ) {

        param_data = (struct TNI_DATA *)&pdu_struct->msg_data[offset];

        parm_good = Check_Parameter (conn,
                                     param_data, 
                                     &param_code, 
                                     &length, 
                                     &value,
                                     string_val);
        if( !parm_good ) {

            LogPdu(pdu_struct, -1, conn);

            rc = send_invalid_pdu_error (conn); 

            return;
        }

/*    length returned by check_parameter is only the parameter value length   */
/*    i.e. it excludes the size of the parameter code and parameter length    */
/*    fields                                                                  */

        length += sizeof(param_data->param_code) +
                  sizeof(param_data->param_length);

        switch( param_code ) {
        
        case TERM_CLIENT_ID:
            term_client_id = value;
            break;

        case TERM_CLIENT_TAG:
            strcpy(term_client_tag,string_val);
            break;

        case TERM_SERVER_ID:
            term_server_id = value;
            break;

        case RESULT_CODE:
            err_code = value;
            break;

        case RPC_REQUEST_TAG:
            strncpy(rpc_request_tag, string_val, MAX_RPC_RQST_TAG_LEN);
            break;

        }                                  /* end switch (param_code)         */
    }                                      /* end for                         */

    app_num = tnicon_p->connection[conn].app_idx;

    switch( err_code ) {
    
    case INVALID_PDU:

        sprintf(err_string.par1,"%d",conn);

        if (tnicon_p->connection[conn].managed_by == CONNS_APP_NAME)
        {
            sprintf(err_string.par2,"%s",
                    tnicon_p->app[app_num].name);
        }
        else
        {
            sprintf(err_string.par2,"client id %d",
                    tnicon_p->connection[conn].client_id_val);
        }

        output_err("Error_Pdu",
                   MI_TNI_INV_EPDU,
                   MX_ERR_LVL_WARNING,
                   err_string);

        LogPdu(pdu_struct, -1, conn); 

        break;
    case TERMINAL_UNKNOWN:

        sprintf(err_string.par1, "%ld", term_server_id);

        if (tnicon_p->connection[conn].managed_by == CONNS_APP_NAME)
        {
            sprintf(err_string.par2,"%s",
                    tnicon_p->app[app_num].name);
        }
        else
        {
            sprintf(err_string.par2,"client id %d",
                    tnicon_p->connection[conn].client_id_val);
        }

        output_err("Error_Pdu",
                   MI_TNI_UNKN_EPDU,
                   MX_ERR_LVL_WARNING,
                   err_string);

        break;
    case MSG_UNDELIVERABLE:

        if (tnicon_p->connection[conn].managed_by == CONNS_APP_NAME)
        {
            sprintf(err_string.par1,"%s",
                    tnicon_p->app[app_num].name);
        }
        else
        {
            sprintf(err_string.par1,"client id %d",
                    tnicon_p->connection[conn].client_id_val);
        }

        sprintf(err_string.par2, "%ld", term_server_id);

        output_err("Error_Pdu",
                   MI_TNI_UND_EPDU,
                   MX_ERR_LVL_WARNING,
                   err_string);

        break;
    case  DUAL_PRIMARY:

        if (tnicon_p->connection[conn].managed_by == CONNS_APP_NAME)
        {
            sprintf(err_string.par1,"%s",
                    tnicon_p->app[app_num].name);
        }
        else
        {
            sprintf(err_string.par1,"client id %d",
                    tnicon_p->connection[conn].client_id_val);
        }

        sprintf(err_string.par2,"%d", conn);

        output_err("Error_Pdu",
                   MI_TNI_DUAL_EPDU,
                   MX_ERR_LVL_WARNING,
                   err_string);

        break;

    case TRANSLET_ERROR:

        if (tnicon_p->connection[conn].messaging_type == MSG_TYPE_ES_RPC)
        {
            if (rpc_request_tag != NULL)
            {
                sprintf(local_err_string.par1,
                        "Translet parser error on RPC message");
                Send_Rpc_Response_To_Game(rpc_request_tag,
                                          MXSRV_RPC_ERR_TRANSLET,
                                          local_err_string.par1,
                                          0,
                                          NULL,
                                          NO_RESP_CALC);

                tnicon_p->tot_rpc_err_pdus_rcvd++;
                tnicon_p->num_rpc_err_pdus_rcvd++;
            }
        }
        break;

    case ES_CMD_SRV_DELIVERY:

        if (tnicon_p->connection[conn].messaging_type == MSG_TYPE_ES_RPC)
        {
            if (rpc_request_tag != NULL)
            {
                sprintf(local_err_string.par1,
                        "ES command server delivery error");

                Send_Rpc_Response_To_Game(rpc_request_tag,
                                          MXSRV_RPC_ERR_CMDSRV_DELV,
                                          local_err_string.par1,
                                          0,
                                          NULL,
                                          NO_RESP_CALC);

                tnicon_p->tot_rpc_err_pdus_rcvd++;
                tnicon_p->num_rpc_err_pdus_rcvd++;
            }
        }
        break;

    case NOT_AUTHENTICATED:

        sprintf(err_string.par1,"%d",conn);

        if (tnicon_p->connection[conn].managed_by == CONNS_APP_NAME)
        {
            sprintf(err_string.par2,"%s",
                    tnicon_p->app[app_num].name);
        }
        else
        {
            sprintf(err_string.par2,"client id %d",
                    tnicon_p->connection[conn].client_id_val);
        }

        output_err("Error_Pdu",
                   MI_TNI_NOT_AUTH_EPDU,
                   MX_ERR_LVL_WARNING,
                   err_string);
        LogPdu(pdu_struct, -1, conn); 

        break;

    case ENCRYPT_FAILURE:

        if (tnicon_p->connection[conn].messaging_type == MSG_TYPE_ES_RPC)
        {
            if (rpc_request_tag != NULL)
            {
                sprintf(local_err_string.par1,
                        "Encryption error encountered on RPC message");
                Send_Rpc_Response_To_Game(rpc_request_tag,
                                          MXSRV_RPC_ERR_ENCRYPT,
                                          local_err_string.par1,
                                          0,
                                          NULL,
                                          NO_RESP_CALC);

                tnicon_p->tot_rpc_err_pdus_rcvd++;
                tnicon_p->num_rpc_err_pdus_rcvd++;
            }
        }
        break;

    case DECRYPT_FAILURE:

        if (tnicon_p->connection[conn].messaging_type == MSG_TYPE_ES_RPC)
        {
            if (rpc_request_tag != NULL)
            {
                sprintf(local_err_string.par1,
                        "Decryption error encountered on RPC message");
                Send_Rpc_Response_To_Game(rpc_request_tag,
                                          MXSRV_RPC_ERR_DECRYPT,
                                          local_err_string.par1,
                                          0,
                                          NULL,
                                          NO_RESP_CALC);

                tnicon_p->tot_rpc_err_pdus_rcvd++;
                tnicon_p->num_rpc_err_pdus_rcvd++;
            }
        }
        break;
    }                                      /* end switch (err_code)           */
}
