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
/* Copyright 2003 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[TNI_PROCESS_TERM_MSG_BLOCK.C]==========================================*/
/*                                                                            */
/* Process_Term_Msg_Block (struct PDU_STRUCT *pdu_struct,                     */
/*                             int msg_offset, long int block_len,            */
/*                             long int num_msgs,                             */ 
/*                             unsigned short conn)                           */ 
/*                                                                            */
/* Purpose: This function parses a terminal message block which consists of   */
/*          terminal header/application message(s).                           */
/*                                                                            */
/* Input Arguments:                                                           */
/*          *pdu_struct      PDU data Structure                               */
/*                                                                            */
/*          block_len        Terminal message block length                    */
/*                                                                            */
/*          num_msgs         Number of terminal message header/application    */
/*                           pairs in term_data                               */
/*                                                                            */
/*          host_id_val      host id of the host sending this terminal        */
/*                           message block                                    */
/*                                                                            */
/*          host_id_idx      host id index                                    */
/*                                                                            */
/*          conn             connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: The maximum number of parameters allowed in a single terminal */
/*              message header is 5.  Per the TNI Protocol Specification, the */
/*              Message Data Length parameter must be the last parameter in   */
/*              the header.  If the Message Data Length parameter is not      */
/*              found within the first 5 parameters, then the terminal        */
/*              header cannot be processed and any remaining terminal         */
/*              messages in the block will be dropped.  X2X will time out     */
/*              the terminal(s) PROCOM buffer and the terminal will retry the */
/*              message.                                                      */
/*                                                                            */
/*====[TNI_PROCESS_TERM_MSG_BLOCK.C]==========================================*/
/*                                                                            */

#include <stdio.h>
#include <limits.h>
#include <string.h>

#include "includes.h"

void Process_Term_Msg_Block (struct PDU_STRUCT *pdu_struct, 
                             int msg_offset, long int block_len,
                             long int num_msgs, 
                             unsigned short conn) {

    unsigned char code;                    /* terminal message parameter code */

    char string_val[MAX_RPC_RQST_TAG_LEN + 1];
    char term_client_tag[MAX_TERM_CLIENT_TAG_LEN];
    char correlation_tag[MAX_CORRELATION_TAG_LEN + 1];

    long int length;                       /* local length variable, used to  */
                                           /* calculate offset to next        */
                                           /* parameter or terminal message   */
    long int value;                        /* generic parameter value returned*/
                                           /* by Check_Parameter              */
    long int mode;                         /* specific parameter values, based*/
    long int term_server_id;
    long int term_client_id;
    long int term_msg_len;
    long int offset;                       /* byte offset into terminal header*/
                                           /* message                         */
    int msgs_processed;                    /* count of number of messages     */
                                           /* actually processed by this      */
                                           /* routine                         */
    int param_count;                       /* counter for number of parameters*/
                                           /* contained in a single terminal  */
                                           /* message header                  */
    int parm_good;                         /* status of the check_paramter    */
    int rc = P_SUCCESS;                    /* generic return code             */

    struct TNI_DATA *term_msg_struct;      /* local structure for handling    */
                                           /* param code/length/value block   */
    char *app_msg_ptr;                     /* pointer to terminal message     */
    unsigned char *term_data;

    err_string = null_err_string;

    term_client_tag[0] = '\0';
    correlation_tag[0] = '\0';
    term_server_id  = -1;
    term_client_id = -1;
    term_msg_len = -1;
    mode = -1; 
    msgs_processed = 0;
    term_data = &pdu_struct->msg_data[msg_offset];

/* assign default delivery mode accounding to TNI protocol version            */

    if (tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_01)
    {
       mode = REQUEST;
    }
    else
    {
       mode = -1;
    } 

/* for each message header/message in the block, process the header, then the */
/* application message                                                        */
    param_count = 0;
    for( offset = 0;
       offset < block_len && param_count < 5;
       offset+= length ) {

        term_msg_struct = (struct TNI_DATA *)(term_data+offset);

        parm_good = Check_Parameter (conn,
                                     term_msg_struct, 
                                     &code, 
                                     &length, 
                                     &value,
                                     string_val);

        if( !parm_good ) {

            LogPdu(pdu_struct, -1, conn);

            rc = send_invalid_pdu_error (conn); 

            return;
        }

        param_count++;                     /* increment for the header we're  */
                                           /* currently processing            */

/*    length returned by check_parameter is only the parameter value          */
/*    length i.e. it excludes the size of the parameter code and parameter    */
/*    length fields                                                           */

        length += sizeof(term_msg_struct->param_code) +
                  sizeof(term_msg_struct->param_length);

        switch( code ) {
        
        case TERM_CLIENT_ID:
            term_client_id = value;
            break;

        case TERM_CLIENT_TAG:
            strcpy(term_client_tag,string_val);
            break;

        case TERM_SERVER_ID:
            term_server_id = value;
            break;

        case CORRELATION_TAG:
            strncpy(correlation_tag, string_val, MAX_CORRELATION_TAG_LEN + 1);
            break;

        case DELIVERY_MODE:
            mode = value;

/* if necessary convert TNI version 1 delivery modes into TNI version 2       */
/* delivery modes                                                             */

            if (tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_01)
            {
                switch (mode)
                {
                case RESPONSE_TNI_V1:
                    mode = RESPONSE;
                    break;

                case UNSOLICITED_TNI_V1:
                    mode = UNSOLICITED;
                    break;

                case BROADCAST_TNI_V1:
                    mode = BROADCAST;
                    break;
                }
            }
            break;

        case MSG_LENGTH:                 /* must be last parameter before msg */
            term_msg_len = value;

            switch( mode ) {
 
            case REQUEST:
                tnicon_p->clt_data_rqst_msgs_cnt[conn]++;
                tnicon_p->clt_data_rqst_msgs_cnt_tot[conn]++;

                Send_App_Rqst_Resp (conn,
                                    term_server_id, term_client_id,
                                    term_client_tag, correlation_tag,
                                    term_msg_len, pdu_struct, 
                                    length, offset, msg_offset, mode);
                                  
                break;

            case RESPONSE:
                if (tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_23)
                {
                    tnicon_p->clt_data_resp_msgs_cnt[conn]++;
                    tnicon_p->clt_data_resp_msgs_cnt_tot[conn]++;

                    Send_App_Rqst_Resp (conn,
                                        term_server_id, term_client_id,
                                        term_client_tag, correlation_tag,
                                        term_msg_len, pdu_struct,
                                        length, offset, msg_offset, mode);
                }
                break;

            case BROADCAST:
                tnicon_p->clt_data_unso_msgs_cnt[conn]++;
                tnicon_p->clt_data_unso_msgs_cnt_tot[conn]++;

                app_msg_ptr = (char *)                        
                              (term_data+length+offset);
                Send_App_Bro (term_msg_len, app_msg_ptr);
                break;

            case UNSOLICITED:
                tnicon_p->clt_data_unso_msgs_cnt[conn]++;
                tnicon_p->clt_data_unso_msgs_cnt_tot[conn]++;

                app_msg_ptr = (char *)                        
                              (term_data+length+offset);
                Send_App_Unso (term_server_id, term_client_id, 
                               term_msg_len, app_msg_ptr);

                break;

            default:
                LogPdu(pdu_struct, term_server_id, conn);

                rc = send_invalid_pdu_error (conn); 

                break;
            }                                        /* end switch (mode)     */

/*             Add terminal message length to length of data  processed to    */
/*             get offset to start of next terminal message's header          */

            length += term_msg_len;
            msgs_processed++;

            if ((offset + length) > block_len) {

                LogPdu(pdu_struct, term_server_id, conn);

                rc = send_invalid_pdu_error (conn); 
            }

            param_count =   0;                       /* init for next message */

/*          Also need to initalize these for each message, not just the       */
/*          first message in the block                                        */

            term_client_tag[0] = '\0';
            term_server_id  = -1;
            term_client_id = -1;
            term_msg_len = -1;

/* assign default delivery mode accounding to TNI protocol version            */

            if (tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_01)
            {
                mode = REQUEST;
            }
            else
            {
                mode = -1;
            }
        }                                            /* end switch (code)     */
    }                                                /* end for               */

    if( msgs_processed != num_msgs ) {               /* tell host and elog    */

        LogPdu(pdu_struct, term_server_id, conn);

        rc = send_invalid_pdu_error (conn); 
    }
}
