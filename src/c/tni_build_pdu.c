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
/*====[TNI_BUILD_PDU.C]=======================================================*/
/*                                                                            */
/* Build_Pdu (unsigned short conn,                                            */
/*            unsigned char pdu_type,                                         */
/*            struct mbuf *inmbuf,                                            */
/*            struct TNI_PARAM_PAIR *params,)                                 */
/*                                                                            */
/* Purpose: This function builds the input PDU buffer.  The input inmbuf will */
/*          be NULL except for the case when a host data PDU is built.        */
/*          Params is a pointer to a null terminated PDU-specific vector      */
/*          containing pairs of parameter codes and values which are used to  */
/*          construct the variable data part of the input PDU, i.e. PDU       */
/*          parameters.  The calling routine is responsible for correctly     */
/*          building the PDU params structure.                                */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection the PDU is to be sent on              */
/*                                                                            */
/*          pdu_type         PDU type to be build                             */
/*                                                                            */
/*          inmbuf           Pointer to an mbuf chain consisting of terminal  */
/*                           message header(s) and their corresponding        */
/*                           terminal message(s).  This argument is only used */
/*                           for the Terminal Data PDU.                       */
/*                                                                            */
/*                           ---------------------------------                */
/*          inmbuf -->       | terminal message 1 header     |                */
/*                           | inmbuf->m_next                | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | terminal message 1            |                */
/*                           | inmbuf->m_next->m_next        | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | terminal message 2 header     |                */
/*                           | inmbuf->m_next->m_next->m_next| --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | terminal message 2            |                */
/*                           | inmbuf->m_next->...->m_next   | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |                  .                                  */
/*                      |                  .                                  */
/*                      |-->               .                                  */
/*                                                             --|            */
/*                                                               |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |    | last terminal message         |                */
/*                      -->  | ... ->m_next = NULL           |                */
/*                           ---------------------------------                */
/*                                                                            */
/*          params           Pointer to a structure containing parameter      */
/*                           code/value pairs.  The list is terminated with a */
/*                           parameter code of zero.                          */
/*                                                                            */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          (struct mbuf *)  Pointer to the mbuf chain containing the single  */
/*                           input PDU.  The mbuf chain contents are PDU-     */
/*                           dependent.                                       */
/*                                                                            */
/*          Case 1:          TNI Alive PDU                                    */
/*                                                                            */
/*                           ---------------------------------                */
/*       (struct mbuf *) --> | Fixed part of PDU             |                */
/*                           | fixed_hdr_mbuf->m_next = NULL |                */
/*                           ---------------------------------                */
/*                                                                            */
/*          Case 2:          Host Param Response, Tni Request, and Error PDUs */
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
/*          Case 3:          Terminal Data PDU                                */
/*                                                                            */
/*                           ---------------------------------                */
/*       (struct mbuf *) --> | Fixed part of PDU             |                */
/*                           | fixed_hdr_mbuf->m_next        | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | Variable part of PDU          |                */
/*                           | var_hdr_mbuf->m_next          | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | terminal message 1 header     |                */
/*                           | var_hdr_mbuf->m_next->m_next  | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | terminal message 1            |                */
/*                           | inmbuf->m_next->m_next        | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | terminal message 2 header     |                */
/*                           | inmbuf->m_next->m_next->m_next| --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | terminal message 2            |                */
/*                           | inmbuf->m_next->...->m_next   | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |                  .                                  */
/*                      |                  .                                  */
/*                      |-->               .                                  */
/*                                                             --|            */
/*                                                               |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |    | last terminal message         |                */
/*                      -->  | ... ->m_next = NULL           |                */
/*                           ---------------------------------                */
/*                                                                            */
/* Assumptions: inmbuf is not NULL for the Terminal Data PDU                  */
/*                                                                            */
/*====[TNI_BUILD_PDU.C]=======================================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

struct mbuf *Build_Pdu (unsigned short conn,
                        unsigned char pdu_type,
                        struct mbuf *inmbuf,
                        struct TNI_PARAM_PAIR *params) {

    struct mbuf *mymbuf;                            /* returned mbuf chain    */

    switch( pdu_type ) {
        
        case CLIENT_RESPONSE_PDU:
            mymbuf = Build_Client_Param_Response_Pdu (conn, params);
            break;

        case SERVER_REQUEST_PDU:
            mymbuf = Build_Server_Request_Pdu (conn, params);
            break;

        case TERMINAL_SERVER_DATA_PDU:
            mymbuf = Build_Term_Data_Pdu(conn, inmbuf, params);
            break;

        case SERVER_ALIVE_PDU:
            mymbuf = Build_Server_Alive_Pdu (conn);
            break;

        case SERVER_CMD_REQUEST_PDU:
            mymbuf = Build_Server_Cmd_Request_Pdu(conn, params);
            break;

        case ERROR_PDU:
            mymbuf = Build_Error_Pdu(conn, params);
            break;

        case RPC_SERVER_DATA_PDU:
            mymbuf = Build_Rpc_Server_Data_Pdu(conn, inmbuf, params);
            break;

        case CLT_SESSION_RESP_PDU:
            mymbuf = Build_Client_Session_Resp_Pdu (conn, params);
            break;

        case SRV_CHALLENGE_RQST_PDU:
            mymbuf = Build_Serv_Challenge_Rqst_Pdu(conn, inmbuf, params);
            break;

        case SRV_CHALLENGE_NOTIFY_PDU:
            mymbuf = Build_Serv_Challenge_Notify_Pdu(conn, params);
            break;

        case SRV_ENC_KEY_RQST_PDU:
            mymbuf = Build_Srv_Enc_Key_Rqst_Pdu(conn, inmbuf, params);
            break;

    }                                              /* end of PDU type switch */
    return(mymbuf);
}
