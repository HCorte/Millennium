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
/*====[TNI_CLIENT_CMD_REQUEST.C]==============================================*/
/*                                                                            */
/* Client_Cmd_Request (struct PDU_STRUCT *pdu_struct,                         */
/*                     long int pdulen,                                       */
/*                     unsigned short conn)                                   */
/*                                                                            */
/* Purpose: This function is a stub to handle a Client Command Request PDU.   */
/*          Currently, the MX task takes no action upon receiving this PDU.   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Client Command    */
/*                           Request PDU.  Currently, this PDU only consists  */
/*                           of a fixed header part.                          */
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
/*====[TNI_CLIENT_CMD_REQUEST.C]==============================================*/
/*                                                                            */

#include <stdio.h>

#include "includes.h"

void
Client_Cmd_Request (struct PDU_STRUCT *pdu_struct, long int pdulen, 
                    unsigned short conn)
{

   Print_Pdu ("client command request",pdu_struct,pdulen);
   
   return;
}
