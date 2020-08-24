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
/*====[TNI_PROCESS_PDU.C]=====================================================*/
/*                                                                            */
/* Process_Pdu( unsigned short length,                                        */
/*              struct PDU_STRUCT *pdu_struct,                                */
/*              unsigned short *conn)                                         */
/*                                                                            */
/* Purpose: This function parses the fixed header of a single input PDU       */
/*          contained in pdu_struct and, based upon the PDU type, calls the   */
/*          appropriate function to process the Variable Data Part of the PDU.*/
/*                                                                            */
/* Input Arguments:                                                           */
/*          length           pdu_struct length in bytes                       */
/*                                                                            */
/*          pdu_struct       structure containing an entire PDU, i.e. PDU     */
/*                           fixed header part followed by variable data part */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/*          conn             Connection number (can only be changes by        */
/*                                              Client_Request () )           */
/*                                                                            */
/* Return Value:                                                              */
/*          int              0 Error                                          */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_PROCESS_PDU.C]=====================================================*/
/*                                                                            */

#include "includes_mbuf.h"

int Process_Pdu( unsigned short length, struct PDU_STRUCT *pdu_struct,
                 unsigned short *conn) {

    unsigned short pdulen;               /* PDU length in HOST order          */

    int invalid_pdu_type;                /* Invalid PDU type indicator        */    int rc = P_SUCCESS;                  /* generic return code               */

    err_string = null_err_string;
    invalid_pdu_type = 0;

/* Check PDU for inconsistencies.  None of these conditions should occur      */

    pdulen = ntohs (pdu_struct->fixed_header.pdu_length);

    if (( pdu_struct->fixed_header.proto_id != TNI_PROTOCOL) ||
        (pdu_struct->fixed_header.version_id != 
         tnicon_p->connection[*conn].tni_proto_ver) ||
        (pdulen != length))
    {
        return(0);
    }
    else
    {
/*     process the PDU based upon the PDU type as it appears in the input PDU */

        if ((tnicon_p->dbg_state ==DBG_ACTIVE) &&
            (tnicon_p->print_flag & PDU_LEVEL_DBG))
        {
            Show_Current_Time ("Process_Pdu");

            fprintf(tnicon_p->dbg_p,"Connection[%d] -- ", *conn);
        }

        switch (pdu_struct->fixed_header.pdu_type)
        {
 
             case CLIENT_REQUEST_PDU:
                 Client_Request (pdu_struct, (long int)pdulen, conn);
                 break;

             case SERVER_RESPONSE_PDU:
                 Server_Response (pdu_struct, (long int)pdulen, *conn);
                 break;

             case TERMINAL_CLIENT_DATA_PDU:
                 Term_Client_Data (pdu_struct, (long int)pdulen, *conn);
                 break;

             case CLIENT_ALIVE_PDU:
                 Client_Alive (pdu_struct, (long int)pdulen, *conn);
                 break;

             case ERROR_PDU:
                 Error_Pdu (pdu_struct, (long int)pdulen, *conn);
                 break;

             case CLIENT_CMD_REQUEST_PDU:
                 Client_Cmd_Request (pdu_struct, (long int)pdulen, *conn);
                 break;

             case SERVER_CMD_RESPONSE_PDU:
                 Server_Cmd_Response (pdu_struct, (long int)pdulen, *conn);
                 break;

             case RPC_CLIENT_DATA_PDU:
                 if (tnicon_p->connection[*conn].tni_proto_ver !=
                     TNI_VERSION_01)
                 {
                     Rpc_Client_Data (pdu_struct, (long int)pdulen, *conn);
                 }
                 else
                 {
                     invalid_pdu_type = 1;
                 } 
                 break;

             case CLT_SESSION_RQST_PDU:
                 switch (tnicon_p->connection[*conn].tni_proto_ver)
                 {
                     case TNI_VERSION_23:
                     case TNI_VERSION_22:
                     case TNI_VERSION_21:
                         Client_Session_Request (pdu_struct,
                                                 (long int)pdulen,
                                                  *conn);
                         break;

                     default:
                         invalid_pdu_type = 1;
                         break;
                 }
                 break;

             case SRV_CHALLENGE_RESP_PDU:
                 if (tnicon_p->connection[*conn].tni_proto_ver >=
                     TNI_VERSION_23)
                 {
                      Server_Challenge_Response (pdu_struct, (long int)pdulen, *conn);
                 }
                 else
                 {
                     invalid_pdu_type = 1;
                 } 
                 break;

             case SRV_ENC_KEY_RESP_PDU:
                 if (tnicon_p->connection[*conn].tni_proto_ver >=
                     TNI_VERSION_30)
                 {
                     Srv_Encryption_Key_Resp (pdu_struct, (long int)pdulen, *conn);
                 }
                 else
                 {
                     invalid_pdu_type = 1;
                 }
                 break;

             case CLT_ENC_KEY_NOTIFY_PDU:
                 if (tnicon_p->connection[*conn].tni_proto_ver >=
                     TNI_VERSION_30)
                 {
                      Clt_Encryption_Key_Notify (pdu_struct, (long int)pdulen, *conn);
                 }
                 else
                 {
                     invalid_pdu_type = 1;
                 }
                 break;

             default:

                 invalid_pdu_type = 1;
                 break;
        }                                           /* end of PDU type switch */

        if (invalid_pdu_type == 1)
        {
             sprintf(err_string.par1,"%d",
                     pdu_struct->fixed_header.pdu_type);
             sprintf(err_string.par2,"%d",*conn);

             output_err("Process_Pdu",
                        MI_TNI_INV_PDU_TYPE,
                        MX_ERR_LVL_ERROR,
                        err_string);

             LogPdu(pdu_struct, -1, *conn);

             rc = send_invalid_pdu_error (*conn);
        }
    }
    return(0);
}
