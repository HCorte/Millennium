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
/*====[MX_DEBUG.C]============================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          AppMsgDbg(int terminal_num,                                       */
/*                    char *msg_desc,                                         */
/*                    long int app_msg_len,                                   */
/*                    unsigned char *app_msg_ptr)                             */
/*                                                                            */
/*====[MX_DEBUG.C]============================================================*/
/*                                                                            */

#include "includes_mbuf.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* AppMsgDbg(terminal_num,                                                    */
/*           *msg_desc,                                                       */
/*           app_msg_len,                                                     */
/*           *app_msg_ptr)                                                    */
/*                                                                            */
/* Purpose: This function writes application messages send and received to    */
/*          MX Sever debug file.  Application messages for a specific         */
/*          terminal or all terminal may be writen.                           */
/*                                                                            */
/* Input Arguments:                                                           */
/*          terminal_num     terminal number (GOLS) or device location idx    */
/*                           to record.                                       */
/*          msg_desc         string describing the type of application        */
/*                           message. I.E. response, unsolicited, or          */
/*                           broadcast.                                       */
/*          app_msg_len      length in bytes of application message           */
/*          app_msg_ptr      pointer to the start of the application message  */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void AppMsgDbg(int terminal_num,
               char *msg_desc,
               long int app_msg_len,
               unsigned char *app_msg_ptr) {

    int i;
    int char_cnt = 0;
    int amt_logged = 0;

    if ((tnicon_p->dbg_state == DBG_ACTIVE) &&
        (tnicon_p->print_flag & TERMINAL_LEVEL_DBG)) {

        if ((terminal_num == tnicon_p->dbg_term_num) ||
            (tnicon_p->dbg_term_num == ALL_TERMINALS)) {

            Show_Current_Time ("AppMsgDbg");
            fprintf(tnicon_p->dbg_p,
                    "Terminal: %d Message Length: %ld Message Type : %s \n\n\t",
                    terminal_num,
                    app_msg_len,
                    msg_desc);

            char_cnt = 0;

            for (i = 0; i < app_msg_len ; i++) {

                if (char_cnt < 22) {

                    amt_logged += fprintf(tnicon_p->dbg_p, 
                                          "%2x ", 
                                          *(app_msg_ptr+i));
                    char_cnt++;
                } else {

                    amt_logged += fprintf(tnicon_p->dbg_p,
                                          "%2x\n\t", 
                                          *(app_msg_ptr+i));
                    char_cnt = 0;
                }        
            }

            if (char_cnt != 0) {

                amt_logged += fprintf(tnicon_p->dbg_p, "\n");
            }

        }

    }
}
