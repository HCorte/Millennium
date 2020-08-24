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
/*====[TNI_BUILD_TERM_PDU_HEADER.C]===========================================*/
/*                                                                            */
/* Build_Term_Pdu_Header (long int term_ndx,                                  */
/*                        long int host_term,                                 */
/*                        char *host_term_tag,                                */
/*                        long int msg_len,                                   */
/*                        long int del_mode,                                  */
/*                        int conn_num,                                       */
/*                        char *correlation_tag)                              */
/*                                                                            */
/* Purpose: This function builds the terminal message header for each         */
/*          terminal message to be included in a Terminal Data PDU.  Note     */
/*          that the terminal message header is NOT the same as the Terminal  */
/*          Data PDU header.                                                  */
/*                                                                            */
/* Input Arguments:                                                           */
/*          term_ndx         Terminal network identifier                      */
/*                                                                            */
/*          host_term        Client terminal identifier                       */
/*                                                                            */
/*          host_term_tag    Client terminal tag                              */
/*                                                                            */
/*          msg_len          Message length                                   */
/*                                                                            */
/*          del_mode         Delivery mode                                    */
/*                                                                            */
/*          conn_num         Connection number                                */
/*                                                                            */
/*          correlation_tag  Correlation tag received from the MX Client      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          (struct mbuf *)  Pointer to mbuf containing terminal message      */
/*                           header                                           */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_BUILD_TERM_PDU_HEADER.C]===========================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

struct mbuf *Build_Term_Pdu_Header (long int term_ndx, long int host_term,
                                    char *host_term_tag, long int msg_len, 
                                    long int del_mode, int conn_num,
                                    char *correlation_tag)
{

int offset;                              /* running sum of number of bytes in */
                                         /* terminal message header.  Same as */
                                         /* offset into mbuf data area        */
int len;                                 /* length of a single parameter      */
                                         /* code/value pair                   */
long int del_mode_tni_v1;                /* TNI version 1 delivery mode code  */
struct mbuf *mymbuf;                     /* returned mbuf pointer             */
unsigned char *mbuf_data_ptr;            /* pointer to mbuf data area         */

   err_string = null_err_string;

   len = Calc_Term_Msg_Hdr_Size(tnicon_p->connection[conn_num].term_id_method);

   if ((mymbuf = mb_alloc (len)) == NULL) {

      sprintf(err_string.par1,"terminal message header");
      sprintf(err_string.par2,"%d",len);
      sprintf(err_string.par3,"Build_Term_Pdu_Header");

      output_err("Build_Term_Pdu_Header",
                 MI_TNI_MB_ALLOC,
                 MX_ERR_LVL_ERROR,
                 err_string);

      return (NULL);
   }
   offset = 0;

/* Get pointer to the data area of the mbuf we just allocated                 */

   mbuf_data_ptr = mymbuf->m_data;

/* Determine which terminal identification method the host expects based      */
/* upon the parameter it sent in the Client Request PDU                       */

   switch (tnicon_p->connection[conn_num].term_id_method)
   {
      case METH_TERM_CLIENT_ID:

/*         If this host has told us in its Client Parameters Request PDU that */
/*         it is expecting Client IDs, then try to get a host ID for this     */
/*         terminal.                                                          */

           len = Build_Parameter (TERM_CLIENT_ID,
                                  host_term,
                                  NULL,
                                 (struct TNI_DATA *)(mbuf_data_ptr+offset));
           offset += len;
           break;

      case METH_TERM_CLIENT_TAG:

/*         If this host has told us in its Client Parameters Request PDU that */
/*         it is expecting Terminal Tags, then try to get a tag for this      */
/*         terminal                                                           */
/*         For the first release, this code has been commented out and is     */
/*         here only as a place holder.                                       */

           if (del_mode != BROADCAST) {

               len = Build_Parameter (TERM_CLIENT_TAG, 
                                      0,
                                      host_term_tag,
                                      (struct TNI_DATA *)(mbuf_data_ptr+offset));
               offset += len;
           }
           break;

      case METH_TERM_SERVER_ID:

/*         If this host has told us in its Client Parameters Request PDU that */
/*         it is expecting Network IDs, then try to get a Network ID for this */
/*         terminal.                                                          */

           len = Build_Parameter (TERM_SERVER_ID, 
                                  term_ndx,
                                  NULL,
                                 (struct TNI_DATA *)(mbuf_data_ptr+offset));
           offset += len;
           break;

   }                                                            /* end switch */

/* always put delivery mode in for TNI version 2 and above                    */

   if (tnicon_p->connection[conn_num].tni_proto_ver == TNI_VERSION_01)
   {

/* convert TNI version 2 delivery modes to TNI version 1 delivery modes       */

       switch (del_mode)
       {
          case RESPONSE:

               del_mode_tni_v1 = RESPONSE_TNI_V1;
               break;

          case UNSOLICITED:

               del_mode_tni_v1 = UNSOLICITED_TNI_V1; 
               break;

          case BROADCAST:

               del_mode_tni_v1 = BROADCAST_TNI_V1;
               break;

          default:

               mb_free_p(mymbuf);           /* free previously allocated mbuf */
               return (NULL);
               break;
       }

/* put delivery mode in only when the message is unsolicted or broadcast      */

       if (del_mode_tni_v1 != RESPONSE_TNI_V1)
       {
           len = Build_Parameter (DELIVERY_MODE, 
                                  del_mode_tni_v1,
                                  NULL,
                                  (struct TNI_DATA *)(mbuf_data_ptr+offset));
           offset += len;
       }
   }
   else
   {
       len = Build_Parameter (DELIVERY_MODE,
                              del_mode,
                              NULL,
                              (struct TNI_DATA *)(mbuf_data_ptr+offset));
       offset += len;
   }

/* return correlation tag if one was provided by the MX Client                */

   if (tnicon_p->connection[conn_num].tni_proto_ver >= TNI_VERSION_22)
   {
       if (correlation_tag != NULL)
       {
           if (correlation_tag[0] != '\0')
           {
               len = Build_Parameter (CORRELATION_TAG,
                                      0,
                                      correlation_tag,
                                      (struct TNI_DATA *)(mbuf_data_ptr+offset));
               offset += len;
           }
       } 
   }
 
/* message length is ALWAYS the last parameter                                */

   len = Build_Parameter (MSG_LENGTH, 
                          msg_len,
                          NULL,
                         (struct TNI_DATA *)(mbuf_data_ptr+offset));

   mymbuf->m_len = offset + len;

   return (mymbuf);
}
