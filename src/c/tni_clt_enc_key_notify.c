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
/* Copyright 2010 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[TNI_CLT_ENC_KEY_NOTIFY.C]==============================================*/
/*                                                                            */
/* Clt_Encryption_Key_Notify (struct PDU_STRUCT *pdu_struct,                  */
/*                            long int pdulen,                                */
/*                            unsigned short conn)                            */
/*                                                                            */
/* Purpose: This function parses the Variable Data Part of a Client           */
/*          Encryption Key Notify PDU contained in pdu_struct.  In reponse    */
/*          the Server will issue a Server Encryption Key Request PDU to the  */
/*          sending client to obtain the new encryption key.                  */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Client Encryption */
/*                           Key Notify PDU, i.e. PDU fixed header part       */
/*                           followed by variable data part.                  */
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
/*====[TNI_CLT_ENC_KEY_NOTIFY.C]==============================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void
Clt_Encryption_Key_Notify (struct PDU_STRUCT *pdu_struct,
                           long int pdulen,
                           unsigned short conn)
{
    int rc = P_SUCCESS;                    /* generic return code             */

    err_string = null_err_string;
    sprintf(err_string.par1, "%d", conn);

    Print_Pdu ("Client Encryption Key Notification", pdu_struct,pdulen);

    tnicon_p->clt_enc_key_notify_cnt_tot[conn]++;

/*  This PDU contains no parameters.  If any are present, then send Error Pdu */
 
    if (pdulen > sizeof (struct TNI_FIXED_HDR))
    {
        LogPdu(pdu_struct, -1, conn);

        rc = send_invalid_pdu_error (conn);
    }
    else
    {
        if (tnicon_p->exchange_enc_key_state == ENC_AVAILABLE &&
            tnicon_p->app[tnicon_p->connection[conn].app_idx].es_rpc_enc_state == ENC_CONFIGURED &&
            tnicon_p->connection[conn].enc_key_x_method != METH_ENC_KEY_X_NONE &&
            tnicon_p->connection[conn].enc_key_type != ENC_KEY_NONE)
        { 
            Send_Srv_Enc_Key_Rqst(conn);
        }
    }
}
