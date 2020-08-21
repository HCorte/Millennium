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
/*====[TNI_BUILD_CLT_SESSION_RESP_PDU.C]======================================*/
/*                                                                            */
/* Build_Client_Session_Resp_Pdu (unsigned short conn,                        */
/*                                struct TNI_PARAM_PAIR *params)              */
/*                                                                            */
/* Purpose: This function constructs an mbuf chain containing a single        */
/*          TNI Client Session Response PDU.  Two separate mbufs are          */
/*          constructed and chained together to form a single chain.  The     */
/*          first mbuf created contains the variable part of the PDU based    */
/*          upon the input parameters.  The second mbuf created contains the  */
/*          fixed header of the Client Session Response PDU.  The mbuf        */
/*          containing the variable part of the PDU is then appended to the   */
/*          mbuf containing the fixed part of the PDU to form the returned    */
/*          mbuf chain.                                                       */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection the PDU is to be sent on              */
/*                                                                            */
/*          params           Pointer to a structure containing parameter      */
/*                           code/value pairs.  The list is terminated with a */
/*                           parameter code of zero.                          */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          (struct mbuf *)  Pointer to the mbuf chain containing the single  */
/*                           Client Session Response PDU                      */
/*                                                                            */
/*                           ---------------------------------                */
/*       (struct mbuf *) --> | Fixed part of PDU             |                */
/*                           | fixed_hdr_mbuf->m_next        | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | Variable part of PDU          |                */
/*                           | var_hdr_mbuf->m_next = NULL   |                */
/*                           ---------------------------------                */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_BUILD_CLT_SESSION_RESP_PDU.C]======================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

struct mbuf *Build_Client_Session_Resp_Pdu (unsigned short conn,
                                            struct TNI_PARAM_PAIR *params)
{
int mlen;                                /* length variable                   */
struct mbuf *fixed_hdr_mbuf;             /* pointer to mbuf containing fixed  */
                                         /* header part of Client Session     */
                                         /* Response  PDU                     */
struct mbuf *var_hdr_mbuf;               /* pointer to mbuf containing        */
                                         /* variable part of Client Session   */
                                         /* Response  PDU                     */
struct TNI_FIXED_HDR *fixed_buf;         /* pointer to data area of           */
                                         /* fixed_hdr_mbuf                    */
char *var_buf;                           /* pointer to data area of           */
                                         /* var_hdr_mbuf                      */

/* Determine the maximum size of the fixed and variable CLIENT SESSION        */
/* RESPONSE PDU header so we can allocate an mbuf of the correct size.  The   */
/* returned value is larger than we need for the variable part only, but it's */
/* close enough.                                                              */

   err_string = null_err_string;

   mlen = Calc_Pdu_Hdr_Size(CLT_SESSION_RESP_PDU);

   if ((var_hdr_mbuf = mb_alloc(mlen)) == NULL)
   {
      sprintf(err_string.par1,"variable data");
      sprintf(err_string.par2,"%d",mlen);
      sprintf(err_string.par3,"Build_Client_Session_Resp_Pdu");

      output_err("Build_Client_Session_Resp_Pdu",
                 MI_TNI_MB_ALLOC,
                 MX_ERR_LVL_ERROR,
                 err_string);

      return (NULL);
   }

/* Build CLIENT SESSION RESPONSE PDU variable header first to make the total  */
/* PDU length easier to calculate for the CLIENT SESSION RESPONSE PDU fixed   */
/* header                                                                     */

   var_buf = (char *)var_hdr_mbuf->m_data;

   var_hdr_mbuf->m_len = 0;

   while (params->param_code)            /* parameter list is 0 terminated    */
   {
        mlen = Build_Parameter (params->param_code, 
                                params->param_value,
                                params->param_value_char,
                               (struct TNI_DATA *)var_buf);

        var_hdr_mbuf->m_len += mlen;
        var_buf += mlen;
        params++;
   }

/* Determine the maximum size of the fixed and variable CLIENT SESSION        */
/* RESPONSE PDU header so we can allocate an mbuf of the correct size.        */
/* The returned value is larger than we need for the fixed part only, but     */
/* it's close enough.                                                         */

   mlen = Calc_Pdu_Hdr_Size(CLT_SESSION_RESP_PDU);

   if ((fixed_hdr_mbuf = mb_alloc(mlen)) == NULL)
   {
      sprintf(err_string.par1,"fixed header");
      sprintf(err_string.par2,"%d",mlen);
      sprintf(err_string.par3,"Build_Client_Session_Resp_Pdu");

      output_err("Build_Client_Session_Resp_Pdu",
                 MI_TNI_MB_ALLOC,
                 MX_ERR_LVL_ERROR,
                 err_string);

      mb_free_p(var_hdr_mbuf);           /* free previously allocated mbuf    */
      return (NULL);
   }

/* Build CLIENT SESSION RESPONSE PDU fixed header                             */

   fixed_buf = (struct TNI_FIXED_HDR *)fixed_hdr_mbuf->m_data;
   fixed_buf->proto_id = TNI_PROTOCOL;          /* Protocol ID                */

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

   fixed_buf->pdu_type = CLT_SESSION_RESP_PDU; 
   fixed_hdr_mbuf->m_len = sizeof(struct TNI_FIXED_HDR);

   fixed_buf->pdu_length  = fixed_hdr_mbuf->m_len + var_hdr_mbuf->m_len;
   fixed_buf->pdu_length = htons(fixed_buf->pdu_length);

/* Now append the 2 mbuf chains - fixed header and variable header            */

   mb_append (&fixed_hdr_mbuf, var_hdr_mbuf);

   if ((tnicon_p->dbg_state ==DBG_ACTIVE) &&
       (tnicon_p->print_flag & PDU_LEVEL_DBG))
   {
       fprintf(tnicon_p->dbg_p,"\nBuilt client session response PDU");
   }

   return (fixed_hdr_mbuf);
}
