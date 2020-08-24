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
/* Copyright 2004 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[TNI_TERM_CLIENT_DATA.C]================================================*/
/*                                                                            */
/* Term_Client_Data (struct PDU_STRUCT *pdu_struct,                           */
/*                   long int pdulen,                                         */
/*                   unsigned short conn)                                     */
/*                                                                            */
/* Purpose: This function parses the Variable Data Part of a single Terminal  */
/*          Client Data PDU contained in pdu_struct and calls Process_Term_Msg*/
/*          to handle each terminal message and it's header.                  */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Term Client Data  */
/*                           PDU i.e. PDU fixed header part followed by       */
/*                           ivariable data part                              */
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
/*====[TNI_TERM_CLIENT_DATA.C]================================================*/
/*                                                                            */

#include <stdio.h>                       

#include "includes.h"

void
Term_Client_Data (struct PDU_STRUCT *pdu_struct,
                  long int pdulen,
                  unsigned short conn)
{
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

    Print_Pdu ("term client data",pdu_struct,pdulen); 

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

/*  Throw PDU away if connection state is not PRIMARY                         */

    if (tnicon_p->connection[conn].conn_state != CONN_PRIMARY)
    {
        return;
    }

/*  Check to make sure the PDU is not too big and is of messaging type OLTP   */

    if ((pdulen > tnicon_p->connection[conn].max_pdu_size) ||
        (tnicon_p->connection[conn].messaging_type != MSG_TYPE_OLTP)) {

        LogPdu(pdu_struct, -1, conn); 

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

    if( (param_code != NUMBER_MESSAGES) ||
        (!parm_good) ) {

        LogPdu(pdu_struct, -1, conn); 

        rc = send_invalid_pdu_error (conn);

        return;
    } else {

      tnicon_p->oltp_clt_data_cnt_tot[conn]++;

        if( tnicon_p->connection[conn].max_messages_per_pdu != 0 ) {

            if( !((0 < num_msgs) &&
                  (num_msgs <= tnicon_p->connection[conn].max_messages_per_pdu)) ) {

                LogPdu(pdu_struct, -1, conn);

                rc = send_invalid_pdu_error (conn); 

                return;
            }
        } else if( num_msgs <= 0 ) {

            LogPdu(pdu_struct, -1, conn);

            rc = send_invalid_pdu_error (conn); 
        }

/*    Determine the length of the terminal header/message block               */

        block_len = pdulen - sizeof(struct TNI_FIXED_HDR) - length;
        offset = length;

        Process_Term_Msg_Block(pdu_struct, offset, block_len, 
                               num_msgs, conn);
    }
}
