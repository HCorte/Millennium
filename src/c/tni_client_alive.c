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
/*====[TNI_CLIENT_ALIVE.C]====================================================*/
/*                                                                            */
/* Client_Alive (struct PDU_STRUCT *pdu_struct,                               */
/*               long int pdulen,                                             */
/*               unsigned short conn)                                         */
/*                                                                            */
/* Purpose: This function updates the connection's last received time stamp   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Client Alive PDU  */
/*                           i.e. PDU fixed header part only                  */
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
/*====[TNI_CLIENT_ALIVE.C]====================================================*/
/*                                                                            */

#include <stdio.h>

#include "includes.h"

void
Client_Alive (struct PDU_STRUCT *pdu_struct,
              long int pdulen, 
              unsigned short conn)
{

   Print_Pdu ("client alive", pdu_struct,pdulen);

   tnicon_p->clt_alive_cnt_tot[conn]++;

/* Update time stamp for connection                                           */

   tnicon_p->connection[conn].time_last_rcvd = current_time;
   
   return;
}
