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
/*====[TNI_BUILD_SERVER_ALIVE_PDU.C]==========================================*/
/*                                                                            */
/* Build_Server_Alive_Pdu (unsigned short conn)                               */
/*                                                                            */
/* Purpose: This function constructs an mbuf chain containing a single        */
/*          ServerTNI Alive PDU.  Because this PDU has no variable part, a    */
/*          single mbuf is created containing the fixed header of the Server  */
/*          Alive PDU.  Therefore the returned mbuf chain consists of a single*/
/*          mbuf containing the fixed part of the PDU.                        */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection the PDU is to be sent on              */
/*                                                                            */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          (struct mbuf *)  Pointer to the mbuf chain containing the single  */
/*                           Error PDU                                        */
/*                                                                            */
/*                           ---------------------------------                */
/*       (struct mbuf *) --> | Fixed part of PDU             |                */
/*                           | fixed_hdr_mbuf->m_next = NULL |                */
/*                           ---------------------------------                */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_BUILD_SERVER_ALIVE_PDU.C]==========================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

struct mbuf *Build_Server_Alive_Pdu (unsigned short conn)
{
int mlen;                                /* length variable                   */
struct mbuf *fixed_hdr_mbuf;             /* pointer to mbuf containing fixed  */
                                         /* header part of Error PDU.         */
struct TNI_FIXED_HDR *fixed_buf;         /* pointer to data area of           */
                                         /* fixed_hdr_mbuf                    */

/* Determine the maximum size of the fixed Server Alive PDU header so we can  */
/* allocate an mbuf of the correct size.  The returned value is exact for     */
/* this PDU as it consists of a  fixed part only.                             */

   err_string = null_err_string;

   mlen = Calc_Pdu_Hdr_Size(SERVER_ALIVE_PDU);

   if ((fixed_hdr_mbuf = mb_alloc(mlen)) == NULL) {

      sprintf(err_string.par1,"fixed header");
      sprintf(err_string.par2,"%d",mlen);
      sprintf(err_string.par3,"Build_Server_Alive_Pdu");

      output_err("Build_Server_Alive_Pdu",
                 MI_TNI_MB_ALLOC,
                 MX_ERR_LVL_ERROR,
                 err_string);

      return (NULL);
   }

/* Build SERVER ALIVE PDU fixed header                                        */

   fixed_buf = (struct TNI_FIXED_HDR *)fixed_hdr_mbuf->m_data;
   fixed_buf->proto_id = TNI_PROTOCOL;   /* Protocol ID                       */

/* If the TNI protocol version is unknown then set the protocol version to    */
/* the default verson                                                         */

   if (tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_UNKNOWN)
   {
       fixed_buf->version_id  = DEFAULT_TNI_VERSION;
   }
   else
   {
       fixed_buf->version_id  = tnicon_p->connection[conn].tni_proto_ver;
   }

   fixed_buf->pdu_type = SERVER_ALIVE_PDU;  /* TNI ALIVE PDU                  */
   fixed_hdr_mbuf->m_len = sizeof(struct TNI_FIXED_HDR);

   fixed_buf->pdu_length  = sizeof(struct TNI_FIXED_HDR);
   fixed_buf->pdu_length = htons(fixed_buf->pdu_length);

   if ((tnicon_p->dbg_state ==DBG_ACTIVE) &&
       (tnicon_p->print_flag & PDU_LEVEL_DBG))
   {
       fprintf(tnicon_p->dbg_p,"\nBuilt server alive PDU");
   }

   return (fixed_hdr_mbuf);
}
