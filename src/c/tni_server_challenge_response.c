
static const char  *fileid = "";

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
/* Copyright 2007 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[TNI_SERVER_CHALLENGE_RESPONSE.C]=======================================*/
/*                                                                            */
/* Server_Challenge_Response (struct PDU_STRUCT *pdu_struct,                  */
/*                  long int pdulen,                                          */
/*                  unsigned short conn)                                      */
/*                                                                            */
/*                                                                            */
/* Purpose: This function parses the Variable Data Part of a single Server    */
/*          Challenge Response PDU contained in pdu_struct.                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Server Challenge  */
/*                           Response PDU, i.e. PDU fixed header part         */
/*                           followed by variable data part                   */
/*                                                                            */
/*          pdulen           PDU length                                       */
/*                                                                            */
/*          conn             Connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_SERVER_CHALLENGE_RESPONSE.C]=======================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void
Server_Challenge_Response(struct PDU_STRUCT *pdu_struct, long int pdulen, unsigned short conn)
{
    int                 act_conn;                   /* Actual connection PDU was sent  */
    int                 offset;                     /* offset to each parameter        */
                                                    /* code/length/value block         */
                                                    /* contained in the var part of the */
    int                 parm_good;                  /* status of the check_paramter    */
    int                 param_count;

    long int            length;                     /* number of bytes in a single     */
                                                    /* parameter unit, including code, */
                                                    /* length, value fields            */

    long int            value;                      /* generic parameter value returned */
                                                    /* by Check_Parameter              */
    long int            result_code;

    char                string_val[MAX_RPC_RQST_TAG_LEN + 1];

    unsigned char       param_code;                 /* PDU parameter code              */

    struct TNI_DATA    *param_data;                 /* pointer to a parameter          */
                                                    /* code/length/value block         */
    struct mbuf        *mymbuf;                     /* pointer to mbuf containing      */
                                                    /* CLient Parameter Response PDU   */
    struct TNI_PARAM_PAIR params[3];
    int                 auth_result_code;
    int                 hash_length;
    int                 param_data_length;
    int                 rc;
    int                 app_idx;
    unsigned char       md_value[MAX_AUTH_HASH_LENGTH];

    mymbuf = NULL;

    err_string = null_err_string;
    auth_result_code = AUTHENTICATION_FAILURE;

    Print_Pdu("server challenge response", pdu_struct, pdulen);

    tnicon_p->srv_challenge_resp_cnt_tot[conn]++;

    /* Read the parameters */

    memset(md_value, 0x00, sizeof(md_value));
    hash_length = 0;
    result_code = -1;
    param_data_length = pdulen - sizeof (struct TNI_FIXED_HDR);

    /* We start with failure state */
    tnicon_p->connection[conn].auth_state = AUTH_FAILURE;

    for(offset=0; offset<param_data_length; offset+=length)
    {

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

        switch( param_code ) 
        {
        
        case RESULT_CODE:
            result_code = value;
            if(result_code != SUCCESS)
            {
            /* Server challenge request processing failed at Mxclient side */

                sprintf(err_string.par1, "Server Challenge Request");
                sprintf(err_string.par2, "%d", conn);
                output_err("Server_Challenge_Response", 
                            MI_TNI_CLIENT_REJECT, 
                            MX_ERR_LVL_WARNING, err_string);
                return;
            }
            break;
        
        case HASH_LENGTH:
            hash_length = value;
            if(hash_length < 0 || hash_length > MAX_AUTH_HASH_LENGTH)
            {
                sprintf(err_string.par1, "%d", hash_length);
                sprintf(err_string.par2, "%d", conn);
                output_err("Server_Challenge_Response",
                            MI_TNI_INV_HASH_LEN,
                            MX_ERR_LVL_ERROR,
                            err_string);
                return;
            }
            else if (offset + length + hash_length > param_data_length)
            {
                /* Not enough data sent from Mxclient */
                LogPdu(pdu_struct, -1, conn);

                rc = send_invalid_pdu_error (conn); 

                return;
            }
            else
            {
                memcpy(md_value, &pdu_struct->msg_data[offset+length], hash_length);
                length += hash_length;

                /* Compare with tnicon hash  and set the auth_state */
                if ((tnicon_p->connection[conn].auth_hash_len == hash_length) &&
                    (memcmp(md_value, tnicon_p->connection[conn].auth_hash_value, hash_length) == 0))
                {
                    auth_result_code = SUCCESS;
                    tnicon_p->connection[conn].auth_state = AUTH_SUCCESS;
                }
                else
                {
                    /* Dump the values for debug purpose */
                    if ((tnicon_p->dbg_state == DBG_ACTIVE) &&
                        (tnicon_p->print_flag & PDU_LEVEL_DBG))
                    {

                        DumpValue("Client Hash:", md_value, hash_length, tnicon_p->dbg_p);
                        DumpValue("Server Hash:", tnicon_p->connection[conn].auth_hash_value,
                                     tnicon_p->connection[conn].auth_hash_len, tnicon_p->dbg_p);
                    }
                }
            }
            break;
        default:

            LogPdu(pdu_struct, -1, conn);

            rc = send_invalid_pdu_error (conn); 

            return;

        }                                          /* end switch (param_code) */
    }                                              /* end for                 */


/* If we reach here then build notification */

    /* Check for authentication lockout */
    if(auth_result_code != SUCCESS)
    {
        tnicon_p->connection[conn].consec_auth_failure_cnt++;

        if(tnicon_p->connection[conn].consec_auth_failure_cnt >= 
            tnicon_p->auth_failure_threshold)
        {
            app_idx = tnicon_p->connection[conn].app_idx;
            tnicon_p->app[app_idx].auth_lockout_state = APP_AUTH_LOCKED;
            tnicon_p->app[app_idx].auth_lockout_release_time.time =
                    current_time.time +
                    tnicon_p->auth_lockout_invl;

            sprintf(err_string.par1, "%s", tnicon_p->app[app_idx].name);
            sprintf(err_string.par2, "%s", tnicon_p->connection[conn].remote_domain_name);
            output_err("Server_Challenge_Response", 
                            MI_TNI_MAX_AUTH_ATMPTS,
                            MX_ERR_LVL_WARNING, err_string);

            err_string = null_err_string;
            output_err("Server_Challenge_Response",
                            MI_TNI_WARN_AUTH_HACK,
                            MX_ERR_LVL_WARNING, err_string);

            err_string = null_err_string;
            sprintf(err_string.par1, "%s", tnicon_p->app[app_idx].name);
            sprintf(err_string.par2, "%d", tnicon_p->auth_lockout_invl/60);
            output_err("Server_Challenge_Response",
                            MI_TNI_AUTH_DISABLED,
                            MX_ERR_LVL_WARNING, err_string);
        }
    }
    else
    {
        tnicon_p->connection[conn].consec_auth_failure_cnt = 0;
    }


    param_count = 0;
    params[param_count].param_code = RESULT_CODE;
    params[param_count].param_value = auth_result_code;
    param_count++;

    params[param_count].param_code = 0;             /* NULL terminated list */
    params[param_count].param_value = 0;

    mymbuf = Build_Pdu(conn, SRV_CHALLENGE_NOTIFY_PDU, NULL, &params[0]);

    /* Send response on the same connection on which CLIENT SESSION REQUEST   */
    /* PDU was received                                                       */

    act_conn = Send_To_Conn(mymbuf, conn, NO_ALT_CONN);

    if (act_conn == 0)
    {
        sprintf(err_string.par1, "Server Challenge Notification");
        sprintf(err_string.par2, "%d", conn);

        output_err("Server_Challenge_Response", MI_TNI_ERR_SEND, MX_ERR_LVL_ERROR, err_string);
    }
    else
    {
        tnicon_p->srv_challenge_notify_cnt_tot[act_conn]++;
    }

    /* Log the attempt */
    log_authentication_attempts(conn, auth_result_code);
}
