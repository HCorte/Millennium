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
/*====[TNI_SEND_SRV_ENC_KEY_RQST.C]===========================================*/
/*                                                                            */
/* Send_Srv_Enc_Key_Rqst (unsigned short conn)                                */
/*                                                                            */
/* Purpose: This function send a Server Encryption Key Request PDU to the     */
/*          specified connection.                                             */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection the PDU is to be sent on              */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_SEND_SRV_ENC_KEY_RQST.C]===========================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void Send_Srv_Enc_Key_Rqst (unsigned short conn)
{
    int    act_conn;                       /* Actual connection PDU was sent  */

    struct TNI_PARAM_PAIR params[4];       /* sized by max number of PDU      */
                                           /* parameters used in request      */
    struct mbuf *mymbuf;                   /* pointer to mbuf containing      */
                                           /* Server Enc Key Request PDU      */
    struct mbuf *x_encrypt_key_mbuf;
    struct mbuf *public_key_mbuf;          /* pointer to mbuf containing      */
                                           /* exchange key for encrypting     */
    char        *key_data;

    err_string = null_err_string;

    if ((tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_30) &&
        (tnicon_p->exchange_enc_key_state == ENC_AVAILABLE))
    {
        params[0].param_code = ENC_KEY_X_METHOD;
        params[0].param_value = METH_ENC_KEY_X_RSA;

        params[1].param_code = ENC_KEY_TYPE;
        params[1].param_value = ENC_KEY_RSA_PUBLIC_DER;

        x_encrypt_key_mbuf = (struct mbuf *) tnicon_p->x_encrypt_key;

        if( (public_key_mbuf = mb_alloc (x_encrypt_key_mbuf->m_len)) == NULL )
        {
            sprintf(err_string.par1,"exchange key");
            sprintf(err_string.par2,"%d",x_encrypt_key_mbuf->m_len);
            sprintf(err_string.par3,"Send_Srv_Enc_Key_Rqst");

            output_err("Send_Srv_Enc_Key_Rqst",
                        MI_TNI_MB_ALLOC,
                        MX_ERR_LVL_ERROR,
                        err_string);

            return;
        }
        memcpy(public_key_mbuf->m_data, x_encrypt_key_mbuf->m_data, x_encrypt_key_mbuf->m_len);
        public_key_mbuf->m_len = x_encrypt_key_mbuf->m_len;

        params[2].param_code = ENC_KEY_LENGTH;
        params[2].param_value = mb_len_p (public_key_mbuf);

        params[3].param_code = 0;                     /* NULL terminated list */
        params[3].param_value = 0;

        mymbuf = Build_Pdu (conn, SRV_ENC_KEY_RQST_PDU, public_key_mbuf, &params[0]);

        act_conn = Send_To_Conn (mymbuf, conn, NO_ALT_CONN);
        if (act_conn == 0)
        {
            sprintf(err_string.par1,"ERROR");
            sprintf(err_string.par2,
                    "Unable to send PDU over Connection[%d]",
                    conn);

            output_err("Send_Srv_Enc_Key_Rqst",
                       MI_TNI_ERR_SEND,
                       MX_ERR_LVL_ERROR,
                       err_string);
        }
        else
        {
             tnicon_p->srv_enc_key_rqst_cnt_tot[act_conn]++;
        }
    }
}
