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
/*====[MXVIS_COMMAND.C]=======================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Generate_Vision_Cmd(int cmd_type,                                 */
/*                              int arg_int1,                                 */
/*                              int arg_int2,                                 */
/*                              char *arg_str1,                               */
/*                              char *arg_str2)                               */
/*          Build_Vision_Cmd(int cmd_type,                                    */
/*                           int arg_int1,                                    */
/*                           int arg_int2,                                    */
/*                           char *arg_str1,                                  */
/*                           char *arg_str2)                                  */
/*          Send_Vision_Cmd(COMMAND_INFO *request)                            */
/*          Process_Vision_Cmd_Resp(GM_MESDSC *ctxd,                          */
/*                                  GM_MESDSC *msgdsc)                        */
/*          Display_Cmd_Result(int code)                                      */
/*                                                                            */
/*====[MXVIS_COMMAND.C]=======================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Generate_Vision_Cmd(int cmd_type,                                          */
/*                     int arg_int1,                                          */
/*                     int arg_int2,                                          */
/*                     char *arg_str1,                                        */
/*                     char *arg_str2)                                        */
/*                                                                            */
/* Purpose: This function initiates the bulding and sending of a vision       */
/*          command message.  In PRO:SYS MXVISION is registered as a task     */
/*          here, then deregistered when the vision command processing is     */
/*          complete.  The task is also deregistered if an error occurs       */
/*          before the command is send to MXSRV.                              */
/*                                                                            */
/* Input Arguments:                                                           */
/*          cmd_type         vision command type                              */
/*          arg_int1         integer command argument one                     */
/*          arg_int2         integer command argument two                     */
/*          arg_str1         string command argument one                      */
/*          arg_str1         string command argument two                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          status                                                            */
/*                           GOOD_CMD - command successfully processed        */
/*                           UNABLE_TO_SEND - vision command message was      */
/*                                            unable to be sent to MXSRV      */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Generate_Vision_Cmd(int cmd_type, 
                        int arg_int1,
                        int arg_int2,
                        char *arg_str1,
                        char *arg_str2)
{
    int status;

    COMMAND_INFO cmd_request;

#   if defined(PROSYS_ENV_ALL)

        G_ERROR error;

#   endif

    status = GOOD_CMD;

#   if defined(PROSYS_ENV_ALL)

    /* register task with GTMS */

        gtms_regtask(MX_VISION_ID, GW_NEVERWAKE, &error);

        if (error.errflg) {

            g_senderror(&error);
            status = UNABLE_TO_SEND;
        }

#   endif

    if (status == GOOD_CMD) {

        status = Build_Vision_Cmd(cmd_type,
                                  arg_int1,
                                  arg_int2,
                                  arg_str1,
                                  arg_str2,
                                  &cmd_request);

        if (status == GOOD_CMD) {

            status = Send_Vision_Cmd(&cmd_request);
        }

#       if defined(PROSYS_ENV_ALL)

            if (status != GOOD_CMD) {
 
                gtms_deregtask(&error);

                if (error.errflg) {
                    g_senderror(&error);
                }
            }

#       endif

    }
    return(status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Build_Vision_Cmd(int cmd_type,                                             */
/*                  int arg_int1,                                             */
/*                  int arg_int2,                                             */
/*                  char *arg_str1,                                           */
/*                  char *arg_str2)                                           */
/*                                                                            */
/* Purpose: This function takes the inputted command arguments and places     */
/*          into the generic command structure.                               */
/*                                                                            */
/* Input Arguments:                                                           */
/*          cmd_type         vision command type                              */
/*          arg_int1         integer command argument one                     */
/*          arg_int2         integer command argument two                     */
/*          arg_str1         string command argument one                      */
/*          arg_str1         string command argument two                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          command          generic command structure                        */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Build_Vision_Cmd(int cmd_type, 
                     int arg_int1,
                     int arg_int2,
                     char *arg_str1,
                     char *arg_str2,
                     COMMAND_INFO *command)
{
    int status = GOOD_CMD;
    int arg_len = 0;

    memset(command,0,sizeof(COMMAND_INFO));

    command->command_group = MXSRV_VISION_CMD;
    command->command_type = cmd_type;

    command->cmd_arg_int1 = arg_int1;
    command->cmd_arg_int2 = arg_int2;

    if (arg_str1 != NULL) {

        arg_len = strlen(arg_str1);

        if (arg_len <= sizeof(command->cmd_arg_str1)) {

            strncpy(command->cmd_arg_str1,
                    arg_str1,
                    arg_len);
        }
        else
        {
            status = INV_ARG_STR1;
        }
    }

    if (arg_str2 != NULL) {

        arg_len = strlen(arg_str2);

        if (arg_len <= sizeof(command->cmd_arg_str2))
        {
            strncpy(command->cmd_arg_str2,
                    arg_str2,
                    arg_len);
        }
        else
        {
            status = INV_ARG_STR2;
        }
    }

    return(status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Send_Vision_Cmd(request)                                                   */
/*                                                                            */
/* Purpose: This function sends the vision command message to MXSRV.  In      */
/*          PRO:SYS the inputted generic command structure is placed in a     */
/*          task-to-task message and sent to MXSRV via GTMS.  In AlphaGOLS    */
/*          the generic command structure is sent to a FORTRAN subroutine     */
/*          which allocates a PROCOM buffer and queues it to MXSRV.           */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          generic command structure                        */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions:                                                               */
/*          send_status                                                       */
/*                           GOOD_CMD - command successfully processed        */
/*                           UNABLE_TO_SEND - vision command message was      */
/*                                            unable to be sent to MXSRV      */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Send_Vision_Cmd(COMMAND_INFO *request)
{

    int send_status;

#   if defined(PROSYS_ENV_ALL)

        G_ERROR error;                  /* error message */
        GM_ADDRESS destadr;             /* routing information for a request */
        GM_MESDSC msg_dsc;              /* Message descriptor */
        GM_MESDSC cntxt_dsc;            /* context descriptor */
        struct GEN_CONTEXT cntxthdr;    /* Context header */
        struct MXSRV_TASK_RQST task_rqst_msg;

#   endif

    send_status = GOOD_CMD;

#   if defined(PROSYS_ENV_ALL)

    /* Setup destination */

        destadr.adrtyp = GAT_TASK;
        destadr.adrval.task.num = MX_MAINPRO_ID;

    /* Setup context descriptor */

        cntxt_dsc.mesbufp = &cntxthdr;
        cntxt_dsc.meslen = sizeof(cntxthdr);
        cntxthdr.nextfunc = Process_Vision_Cmd_Resp;

        task_rqst_msg.func = MXSRV_TASK_CMD;
        task_rqst_msg.reqlen = sizeof(COMMAND_INFO);

        task_rqst_msg.rqst.vis_cmd.command_group = 
            request->command_group;
        task_rqst_msg.rqst.vis_cmd.command_type = 
            request->command_type;
        task_rqst_msg.rqst.vis_cmd.cmd_arg_int1 = 
            request->cmd_arg_int1;
        task_rqst_msg.rqst.vis_cmd.cmd_arg_int2 = 
            request->cmd_arg_int2;
        strncpy(task_rqst_msg.rqst.vis_cmd.cmd_arg_str1,
                request->cmd_arg_str1,
                MAX_CMD_ARG_STR - 1);
        strncpy(task_rqst_msg.rqst.vis_cmd.cmd_arg_str2,
                request->cmd_arg_str2, 
                MAX_CMD_ARG_STR - 1);

        msg_dsc.mesbufp = &task_rqst_msg;
        msg_dsc.meslen = task_rqst_msg.reqlen +
                         sizeof(struct MXSRV_TASK_RQST);

        gtms_request(GRT_REGULAR, &destadr, &msg_dsc, &cntxt_dsc, &error);

        if (error.errflg) {

            g_senderror(&error);
            send_status = UNABLE_TO_SEND;
        } else {

            outstanding_cmd = 1;
        }

#   endif

#   if defined(GOLS_ENV_ALL)

        QUEUE_MXVIS_CMD (request,
                         &send_status);

#   endif

    return(send_status);
}

#if defined(PROSYS_ENV_ALL)

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Process_Vision_Cmd_Resp(struct GEN_CONTEXT *ctxd,                          */
/*                         GM_MESDSC *msgdsc)                                 */
/*                                                                            */
/* Purpose: This function is called when the task-to-task message response is */
/*          is received by MXVISION.  The function is only executed in        */
/*          PRO:SYS.                                                          */
/*                                                                            */
/* Input Arguments:                                                           */
/*          ctxd             pointer to context buffer set up is              */
/*                           Send_Vision_Cmd.                                 */
/*          msgdsc           pointer to messge descriptor containing response */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

    void Process_Vision_Cmd_Resp(void *ctxd,
                                 GM_MESDSC *msgdsc)
    {
        G_ERROR  error;
        COMMAND_RESP *resp_p;
        struct GEN_CONTEXT *ctxd_p;

        ctxd_p = (struct GEN_CONTEXT *)ctxd;
        resp_p = (COMMAND_RESP *)msgdsc->mesbufp;

        Display_Cmd_Result(resp_p->result_code);

        gtms_deregtask(&error);

        if (error.errflg) {
            g_senderror(&error);
        }
        outstanding_cmd = 0;

        return;
    }

#endif

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Display_Cmd_Result(int code)                                               */
/*                                                                            */
/* Purpose: This function displays in MXVISION error text messages associated */
/*          with vision command message processing.                           */
/*                                                                            */
/* Input Arguments:                                                           */
/*          code             vision command message error code                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Display_Cmd_Result(int code)
{
    int screen_x_pos;
    int screen_y_pos;

    char error_msg[80];

    sprintf(error_msg,"                                           "); 

    /* based of screen, determine where an error message can be displayed */

    switch (current_menu) {

    case (DEBUG):
    case (DEBUG_TERMINAL):
    case (LOG):
    case (CHNG_LOG_FILE):
    case (CHNG_LOG_THRESHOLD):
    case (SERVER_SUMMARY):
        
        screen_x_pos = 2;
        screen_y_pos = 19;
        break;

    case (CONNECTION):
        
        screen_x_pos = 2;
        screen_y_pos = 21;
        break;
    }

    switch (code) {

    case GOOD_CMD:
    case UNKNOWN_CMD_GROUP:
        break;

    case UNKNOWN_CMD:

        sprintf(error_msg,"Unknown command");
        break;

    case INV_ARG_INT1:
    case INV_ARG_INT2:
    case INV_ARG_STR1:
    case INV_ARG_STR2:

        sprintf(error_msg,"Invalid command parameter");
        break;

    case UNABLE_TO_SEND:

        sprintf(error_msg,"Unable to send command");
        break;
    }

    wmove(win,screen_y_pos,screen_x_pos);
    wprintw(win,"%s",error_msg);

    return;
}
