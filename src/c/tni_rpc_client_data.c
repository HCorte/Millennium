static const char *fileid = "";

/*
 * ===[tni_rpc_client_data.c]============================================= 
 *
 * Description:
 *
 * Functions to process the RPC Client Data PDU.
 *
 * Functions:
 *
 * Rpc_Client_Data       - Parse RPC Client Data PDU
 * Process_Rpc_Msg_Block - Process each RPC message in RPC Client Data PDU
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
 * Copyright (c) 2004 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * =======================================================================
 */

#include "includes_mbuf.h"

/* [Rpc_Client_Data]
 *
 * Summary:
 * 
 * Rpc_Client_Data(struct PDU_STRUCT *pdu_struct,
 *                 long int pdu_len,
 *                 unsigned short conn)
 *
 * pdu_struct       - structure containing an entire Host Data PDU,    
 *                   i.e. PDU fixed header part followed by variable 
 *                   data part
 * pdu_len         - PDU length
 * conn            - connection number
 *
 * Description:
 *
 * This function parses the Variable Data Part of a single Host Data
 * PDU contained in pdu_struct and calls Process_Term_Msg to handle
 * each terminal message and it's header.  It is assumed the PDU Fixed
 * Header has been validated by the calling function prior to
 * entering this function.
 *
 * Returns Values: None
 *
 */

void
Rpc_Client_Data(struct PDU_STRUCT *pdu_struct,
                long int pdu_len,
                unsigned short conn)
{
                                           /* originated                      */
    unsigned char param_code;              /* PDU parameter code              */
    struct TNI_DATA *param_data;           /* pointer to a parameter          */
                                           /* code/length/value block         */
    int i;                                 /* terminal message counter        */
    int offset;                            /* offset to each parameter        */
                                           /* code/length/value block         */
                                           /* contained in the var part of the*/
                                           /* PDU also used for offset to each*/
                                           /* terminal message                */
    int block_len;                         /* length of block containing term */
                                           /* header/message pairs            */
    int parm_good;                         /* status of the check_paramter    */
                                           /* function                        */
    int rc = P_SUCCESS;                    /* generic return code             */

    long int length;                       /* number of bytes in a single     */
                                           /* parameter unit, including       */
                                           /* code, length, value fields      */
    long int num_msgs;                     /* number of terminal message in   */
                                           /* this PDU                        */
    long int msg_len;                      /* length of (terminal header +    */
                                           /* message)                        */

    char string_val[MAX_RPC_RQST_TAG_LEN + 1];
    int app_idx;

    err_string = null_err_string;

    Print_Pdu ("Rpc_Client_Data", pdu_struct,pdu_len);

/* Check authentication status */
    if (tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_23)
    {
        app_idx = tnicon_p->connection[conn].app_idx;
        if(tnicon_p->app[app_idx].auth_required == 1 && 
            tnicon_p->connection[conn].auth_state != AUTH_SUCCESS)
        {
            LogPdu (pdu_struct, -1, conn);
            rc = send_not_authenticated_error (conn);
            return;
        }
    }


/*  Throw RPC PDU away if connection state is not PRIMARY  And version is less than TNI_VERSION_23 */

    if (tnicon_p->connection[conn].conn_state != CONN_PRIMARY &&
        tnicon_p->connection[conn].tni_proto_ver < TNI_VERSION_23)
    {
        LogPdu (pdu_struct, -1, conn);
        rc = send_invalid_pdu_error (conn);
        return;
    }

/*  Check to make sure the PDU is not too big and is of messaging type RPC    */

    if ((pdu_len > tnicon_p->connection[conn].max_pdu_size) ||
        (tnicon_p->connection[conn].messaging_type != MSG_TYPE_ES_RPC))
    {
        LogPdu (pdu_struct, -1, conn);

        rc = send_invalid_pdu_error (conn);

        return;
    }

/* Assume that first byte following fixed header is a parameter block         */

    offset = 0;

    param_data = (struct TNI_DATA *)&pdu_struct->msg_data[offset];

/* Make sure the number of messages is in range                               */

    parm_good = Check_Parameter (conn,
                                 param_data,
                                 &param_code,
                                 &length,
                                 &num_msgs,
                                 string_val);

/* length returned by check_parameter is only the parameter value length i.e. */
/* it excludes the size of the parameter code and parameter length fields     */

    length += sizeof(param_data->param_code) + sizeof(param_data->param_length);

/* This is the only variable parameter allowed on this PDU                    */

    if ((param_code != NUMBER_MESSAGES) || (!parm_good))
    {
        LogPdu (pdu_struct, -1, conn);

        rc = send_invalid_pdu_error (conn);

        return;
    }
    else
    {
      tnicon_p->rpc_clt_data_cnt_tot[conn]++;

        if (tnicon_p->connection[conn].max_messages_per_pdu != 0)
        {
            if (!((0 < num_msgs) &&
                  (num_msgs <= tnicon_p->connection[conn].max_messages_per_pdu)))
            {
                LogPdu (pdu_struct, -1, conn);

                rc = send_invalid_pdu_error (conn);

                return;
            }
        }
        else if ( num_msgs <= 0 )
        {
            LogPdu (pdu_struct, -1, conn);

            rc = send_invalid_pdu_error (conn);
        }

/*    Determine the length of the RPC header/message block                    */

        block_len = pdu_len - sizeof (struct TNI_FIXED_HDR) - length;
        offset = length;

        Process_Rpc_Msg_Block (pdu_struct, offset, block_len,
                               num_msgs, conn);
    }
}

/* [Process_Rpc_Msg_Block]
 *
 * Summary:
 *
 * Process_Rpc_Msg_Block(struct PDU_STRUCT *pdu_struct,
 *                       int msg_offset, long int block_len,
 *                       long int num_msgs, unsigned short conn)
 *
 * pdu_struct      - pointer to start of the PDU
 * msg_offset      - byte offset to the start of the first RPC
 *                   message header 
 * block_len       - RPC message block length
 * num_msgs        - Number of RPC message header/application
 *                   pairs in rpc_data
 * conn            - connection number
 *
 * Description:
 *
 * This function parses a RPC message block which consists of RPC
 * header/application message(s).  It is assumed the maximum number
 * of parameters allowed in a single RPC message header is 3.  Per the
 * TNI Protocol Specification, the Message Data Length parameter must
 * be the last parameter in the header.  If the Message Data Length
 * parameter is not found within the first 3 parameters, then the RPC
 * header cannot be processed and any remaining RPC messages in the
 * block will be dropped. 
 *
 * Returns Values: None
 *
 */

void
Process_Rpc_Msg_Block(struct PDU_STRUCT *pdu_struct,
                      int msg_offset, long int block_len,
                      long int num_msgs,
                      unsigned short conn)
{
    unsigned char code;                    /* parameter code */

    char string_val[MAX_RPC_RQST_TAG_LEN + 1];
    char rpc_request_tag[MAX_RPC_RQST_TAG_LEN + 1];

    long int length;                       /* local length variable, used to  */
                                           /* calculate offset to next        */
                                           /* parameter or RPC message        */
    long int value;                        /* generic parameter value returned*/
                                           /* by Check_Parameter              */
    long int enc_mode;                     /* encryption mode                 */
    long int mode;                         /* delivery mode                   */
    long int rpc_msg_len;
    long int offset;                       /* byte offset into RPC header     */
                                           /* message                         */
    int msgs_processed;                    /* count of number of messages     */
                                           /* actually processed by this      */
                                           /* routine                         */
    int param_count;                       /* counter for number of parameters*/
                                           /* contained in a single RPC       */
                                           /* message header                  */
    int parm_good;                         /* status of the check_paramter    */
    byte_4 rc = P_SUCCESS;                 /* generic return code             */

    struct TNI_DATA *rpc_msg_struct;       /* local structure for handling    */
                                           /* param code/length/value block   */
    char *rpc_msg_ptr;                     /* pointer to RPC message          */
    unsigned char *rpc_data;
    char *decrypt_msg_ptr = NULL;
    long int decrypt_msg_len = 0;
    struct mbuf *decrypt_mbuf;
    struct ERR_STRING local_err_string;

    local_err_string = null_err_string;
    err_string = null_err_string;

    memset(rpc_request_tag, '\0', MAX_RPC_RQST_TAG_LEN + 1);
 
    rpc_msg_len = -1;
    mode = -1;
    enc_mode = -1;
    msgs_processed = 0;

    rpc_data = &pdu_struct->msg_data[msg_offset];

/* for each message header/message in the block, process the header, then the */
/* application message                                                        */

    param_count = 0;

    for (offset = 0;
         offset < block_len && param_count < 4;
         offset+= length)
    {
        rpc_msg_struct = (struct TNI_DATA *)(rpc_data+offset);

        parm_good = Check_Parameter (conn,
                                     rpc_msg_struct,
                                     &code,
                                     &length,
                                     &value,
                                     string_val);

        if (!parm_good) {
            LogPdu (pdu_struct, -1, conn);
            rc = send_invalid_pdu_error (conn);

            return;
        }

        param_count++;                     /* increment for the header we're  */
                                           /* currently processing            */

/*    length returned by check_parameter is only the parameter value          */
/*    length i.e. it excludes the size of the parameter code and parameter    */
/*    length fields                                                           */

        length += sizeof(rpc_msg_struct->param_code) +
                  sizeof(rpc_msg_struct->param_length);

        switch (code)
        {

            case RPC_REQUEST_TAG:
                strncpy (rpc_request_tag, string_val, MAX_RPC_RQST_TAG_LEN);
                break;

            case DELIVERY_MODE:
                mode = value;
                break;

            case ENC_MODE:
                if (tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_30)
                {
                    enc_mode = value;
                    switch (enc_mode)
                    {
                        case ENC_NONE:
                        case ENC_RQST:
                        case ENC_RESP:
                        case ENC_RQST_RESP:
                        case ENC_UNSO:
                             /* Parameter value valid */
                             break;

                        default:
                            LogPdu (pdu_struct, 0, conn);
                            rc = send_invalid_pdu_error (conn);
                            break;
                    }
                }
                else
                {
                    LogPdu (pdu_struct, 0, conn);
                    rc = send_invalid_pdu_error (conn);
                }
                break;

            case MSG_LENGTH:                 /* must be last parameter before msg */
                rpc_msg_len = value;

                /* Process message only if the connection state is primary or message is a request */
                if (tnicon_p->connection[conn].conn_state == CONN_PRIMARY ||
                    mode == REQUEST )
                {
                    switch( mode )
                    {
                        case REQUEST:
                            tnicon_p->tot_rpc_rqsts_rcvd++;
                            tnicon_p->num_rpc_rqsts_rcvd++;

                            tnicon_p->clt_data_rqst_msgs_cnt[conn]++;
                            tnicon_p->clt_data_rqst_msgs_cnt_tot[conn]++;

#                           if defined(PROSYS_ENV_PLATFORM)

                                /* An RPC Request processed only if version is >= TNI_VERSION_23 */
                                if ( tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_23)
                                {
                                    rpc_msg_ptr = (char *) (rpc_data+length+offset);

                                    Send_Rpc_Request_To_Gate_Task (conn,
                                                                   rpc_request_tag,
                                                                   rpc_msg_len,
                                                                   rpc_msg_ptr);
                                }

#                           endif

                            break;

                        case RESPONSE:

                            tnicon_p->tot_rpc_resps_rcvd++;
                            tnicon_p->num_rpc_resps_rcvd++;

                            tnicon_p->clt_data_resp_msgs_cnt[conn]++;
                            tnicon_p->clt_data_resp_msgs_cnt_tot[conn]++;

                            if (tnicon_p->connection[conn].conn_state == CONN_PRIMARY)
                            {
                                decrypt_msg_ptr = (char *) (rpc_data+length+offset);
                                decrypt_msg_len = rpc_msg_len;

                                decrypt_mbuf = Decrypt_Data(conn, mode, enc_mode,
                                                            decrypt_msg_len,
                                                            (ubyte_1 *) decrypt_msg_ptr,
                                                            &rc);

                                if (decrypt_mbuf != NULL)
                                {
                                    decrypt_msg_ptr = (char *) decrypt_mbuf->m_data;
                                    decrypt_msg_len = decrypt_mbuf->m_len;
                                }
                                else
                                {
                                    sprintf(local_err_string.par1,
                                            "Error decrypting Response message");
                                }

                                Send_Rpc_Response_To_Game (rpc_request_tag,
                                                           rc,
                                                           local_err_string.par1,
                                                           decrypt_msg_len,
                                                           decrypt_msg_ptr,
                                                           CALC_RESP_TIME);
                                mb_free_p(decrypt_mbuf);
                            }
                            break;

                        case BROADCAST:
                            tnicon_p->clt_data_unso_msgs_cnt[conn]++;
                            tnicon_p->clt_data_unso_msgs_cnt_tot[conn]++;

                             break;

                        case UNSOLICITED:
                            tnicon_p->clt_data_unso_msgs_cnt[conn]++;
                            tnicon_p->clt_data_unso_msgs_cnt_tot[conn]++;

                             break;

                        default:
                            LogPdu (pdu_struct, 0, conn);
                            rc = send_invalid_pdu_error (conn);

                            break;
                    }                                    /* end switch (mode)     */
                }

/*             Add terminal message length to length of data  processed to    */
/*             get offset to start of next terminal message's header          */

                length += rpc_msg_len;
                msgs_processed++;

                if ((offset + length) > block_len)
                {
                    LogPdu (pdu_struct, 0, conn);
                    rc = send_invalid_pdu_error (conn);
                }

                param_count =   0;                   /* init for next message */

/*             Also need to initalize these for each message, not just the    */
/*             first message in the block                                     */

                rpc_request_tag[0] = '\0';
                rpc_msg_len = -1;
                mode = -1;
                break;
        }                                            /* end switch (code)     */
    }                                                /* end for               */

    if (msgs_processed != num_msgs)                  /* tell client and elog */
    {
        LogPdu(pdu_struct, 0, conn);
        rc = send_invalid_pdu_error (conn);
    }
    return;
}
