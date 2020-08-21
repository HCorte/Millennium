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
/*====[TNI_PRINT_PDU.C]=======================================================*/
/*                                                                            */
/* Print_Pdu (char * pdu_string,                                              */
/*            struct PDU_STRUCT *pdu_struct,                                  */
/*            long int pdulen,                                                */
/*            int data_flag);                                                 */
/*                                                                            */
/* Purpose: This debugging function prints a single received TNI PDU.  The    */
/*          fixed header components are separately identified.  The Variable  */
/*          Data part of the PDU (if it exists) is displayed in hexadecimal,  */
/*          one byte per line.                                                */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_string       string identifying PDU type (as determined by    */
/*                           calling routine)                                 */
/*          pdu_struct       structure containing an entire PDU, i.e. PDU     */
/*                           fixed header part followed by variable data part */
/*          pdulen           Number of bytes in PDU                           */
/*          data_flag        flag indicating fixed header only or entire PDU  */
/*                           = 0, fixed header only                           */
/*                           = 1, full PDU, fixed header and variable part    */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: Input pdulen is in host order.                                */
/*                                                                            */
/*====[TNI_PRINT_PDU.C]=======================================================*/
/*                                                                            */

#include "includes.h"

void Print_Pdu (char * pdu_string, struct PDU_STRUCT *pdu_struct,
                long int pdulen) {
    TNICON *ptr;                         /* pointer to global section info    */

    ptr = tnicon_p;

    if( (tnicon_p->dbg_state == DBG_ACTIVE) &&
        (ptr->print_flag & PDU_LEVEL_DBG) ) {

        if( (pdulen >= sizeof (struct TNI_FIXED_HDR)) ) {

            fprintf(ptr->dbg_p,"%s PDU data: \n\n\t", pdu_string);

            DumpPdu(pdu_struct, pdulen, ptr->dbg_p);                    

        }
    }
}
