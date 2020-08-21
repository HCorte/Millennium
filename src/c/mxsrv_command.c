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
/*====[MXSRV_COMMAND.C]=======================================================*/
/*                                                                            */
/* Purpose: These functions provide time utilities.                           */
/*                                                                            */
/* Functions:                                                                 */
/*          Process_Command(COMMAND_INFO *request)                            */
/*          Parse_Vision_Cmd(COMMAND_INFO *request)                           */
/*          Parse_Comm_Mode_Cmd(COMMAND_INFO *request)                        */
/*          Parse_Close_Conn_Cmd(COMMAND_INFO *request)                       */
/*          Parse_Overide_Prim_Cmd(COMMAND_INFO *request)                     */
/*                                                                            */
/*====[MXSRV_COMMAND.C]=======================================================*/
/*                                                                            */

#include "includes.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Process_Command(COMMAND_INFO *request)                                     */
/*                                                                            */
/* Purpose: This function performs the first step in processing a command     */
/*          message, parsing of the command group.                            */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          cmd_status                                                        */
/*                           GOOD_CMD - command successfully processed        */
/*                           UNKNOWN_CMD_GROUP - command group is unknown     */
/*                           UNKNOWN_CMD - command type is unknown            */
/*                           INV_ARG_INT1 - command integer argument one is   */
/*                                          invalid                           */
/*                           INV_ARG_INT2 - command integer argument two is   */
/*                                          invalid                           */
/*                           INV_ARG_STR1 - command string argument one is    */
/*                                          invalid                           */
/*                           INV_ARG_STR2 - command string argument two is    */
/*                                          invalid                           */
/*                           UNABLE_TO_SEND - resulting Server Command Request*/
/*                                            PDU was unable to be sent to a  */
/*                                            client.                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Process_Command(COMMAND_INFO *request)
{
    int cmd_status;

    err_string = null_err_string;

    switch (request->command_group)
    {
        case MXSRV_VISION_CMD:
        case MXSRV_INTERNAL_CMD:
            cmd_status = Parse_Vision_Cmd(request);
            break;

        default:
            cmd_status = UNKNOWN_CMD_GROUP;

            sprintf(err_string.par1,"%d",request->command_group);

            output_err("Process_Command",
                       MI_TNI_CMD_INV_GRP,
                       MX_ERR_LVL_ERROR,
                       err_string);
            break;
    }

    return(cmd_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Vision_Cmd(COMMAND_INFO *request)                                    */
/*                                                                            */
/* Purpose: This function performs the parses of all commands generated       */
/*          MXVISION.                                                         */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          vis_cmd_status                                                    */
/*                           GOOD_CMD - command successfully processed        */
/*                           UNKNOWN_CMD - command type is unknown            */
/*                           INV_ARG_INT1 - command integer argument one is   */
/*                                          invalid                           */
/*                           INV_ARG_INT2 - command integer argument two is   */
/*                                          invalid                           */
/*                           INV_ARG_STR1 - command string argument one is    */
/*                                          invalid                           */
/*                           INV_ARG_STR2 - command string argument two is    */
/*                                          invalid                           */
/*                           UNABLE_TO_SEND - resulting Server Command Request*/
/*                                            PDU was unable to be sent to a  */
/*                                            client.                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Vision_Cmd(COMMAND_INFO *request)
{
    int vis_cmd_status;

    err_string = null_err_string;

    switch (request->command_type)
    {
        case CHANGE_COMM_MODE:
            vis_cmd_status = Parse_Comm_Mode_Cmd(request);
            break;

        case CLOSE_CLIENT_CONN:
            vis_cmd_status = Parse_Close_Conn_Cmd(request);
            break;

        case OVERIDE_PRIM_NOTIFY:
            vis_cmd_status = Parse_Overide_Prim_Cmd(request);
            break;

        case CHANGE_DEBUG_LEVEL:
            vis_cmd_status = Parse_Debug_Level_Cmd(request);
            break;

        case CHANGE_LOG_MODE:
            vis_cmd_status = Parse_Log_Mode_Cmd(request);
            break;

        case CHANGE_LOG_PATH:
            vis_cmd_status = Parse_Log_Path_Cmd(request);
            break;

        case CHANGE_LOG_THRESHOLD:
            vis_cmd_status = Parse_Log_Threshold_Cmd(request);
            break;

        default:
            vis_cmd_status = UNKNOWN_CMD;

            sprintf(err_string.par1,"%d",request->command_group);
            sprintf(err_string.par2,"%d",request->command_type);

            output_err("Parse_Vision_Cmd",
                       MI_TNI_CMD_INV_TYP,
                       MX_ERR_LVL_ERROR,
                       err_string);

            break;
    }

    return(vis_cmd_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Comm_Mode_Cmd(COMMAND_INFO *request)                                 */
/*                                                                            */
/* Purpose: This function parses the change communication mode command.       */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          comm_mode_status                                                  */
/*                           GOOD_CMD - command successfully processed        */
/*                           INV_ARG_INT1 - command integer argument one is   */
/*                                          invalid                           */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Comm_Mode_Cmd(COMMAND_INFO *request)
{
    int comm_mode_status;

    struct ERR_STRING local_err_string;

    err_string = null_err_string;
    local_err_string = null_err_string;

    comm_mode_status = GOOD_CMD;

    sprintf(local_err_string.par1,"Communication mode manually");

    switch (tnicon_p->comm_mode)
    {
    case COMM_DISABLED:

        sprintf(local_err_string.par2,"DISABLED");
        break;

    case COMM_ENABLE:
    case COMM_ENABLED:

        sprintf(local_err_string.par2,"ENABLED");
        break;

    case COMM_SYNC:

        sprintf(local_err_string.par2,"SYNCHRONIZING");
        break;
    }

    switch (request->cmd_arg_int1)
    {
    case 0:

        sprintf(local_err_string.par3,"DISABLED");

        if (tnicon_p->comm_mode != COMM_DISABLED) {

            Close_Listen_Sockets();
            Close_All_Connections();
            tnicon_p->comm_mode = COMM_DISABLED;
        }
        break;

    case 1:

        switch (tnicon_p->comm_mode)
        {
        case COMM_ENABLE:
        case COMM_ENABLED:

            sprintf(local_err_string.par3,"ENABLED");
        break;

        case COMM_DISABLED:

            if (tnicon_p->start_mode == COMM_SYNC) {

                tnicon_p->comm_mode = COMM_SYNC;
                sprintf(local_err_string.par3,"SYNCHRONIZE");
            }else {

                tnicon_p->comm_mode = COMM_ENABLE;
                sprintf(local_err_string.par3,"ENABLED");
            }
        break;

        case COMM_SYNC:

            tnicon_p->comm_mode = COMM_ENABLED;
            sprintf(local_err_string.par3,"ENABLED");

            output_err("Parse_Comm_Mode_Cmd",
                       MI_TNI_COM_SYNC_OVR,
                       MX_ERR_LVL_INFO,
                       err_string);
        break;
        }

        break;

    default:
        comm_mode_status = INV_ARG_INT1;

        sprintf(local_err_string.par1,"%d",request->command_group);
        sprintf(local_err_string.par2,"%d",request->command_type);
        sprintf(local_err_string.par3,"1");
        sprintf(local_err_string.par4,"%d",request->cmd_arg_int1);

        output_err("Parse_Comm_Mode_Cmd",
                   MI_TNI_CMD_INV_ARG,
                   MX_ERR_LVL_ERROR,
                   local_err_string);
        break;
    }

    if (comm_mode_status == GOOD_CMD) {

        output_err("Parse_Comm_Mode_Cmd",
                   MI_TNI_CMD_CHNG,
                   MX_ERR_LVL_INFO,
                   local_err_string);
    }
    return(comm_mode_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Close_Conn_Cmd(COMMAND_INFO *request)                                */
/*                                                                            */
/* Purpose: This function parses the close connection command.                */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          close_conn_status                                                 */
/*                           GOOD_CMD - command successfully processed        */
/*                           INV_ARG_INT1 - command integer argument one is   */
/*                                          invalid                           */
/*                           UNABLE_TO_SEND - resulting Server Command Request*/
/*                                            PDU was unable to be sent to a  */
/*                                            client.                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Close_Conn_Cmd(COMMAND_INFO *request)
{
    int act_conn;                          /* Actual connection PDU was sent  */
    int close_conn_status;
    int conn;

    struct mbuf *mymbuf;
    struct TNI_PARAM_PAIR params[2];     /* sized by max number of PDU        */
                                         /* parameters we may send in a       */
                                         /* TNI COMMAND REQUEST PDU           */
    err_string = null_err_string;
    conn = request->cmd_arg_int1;

    if ((conn > 0) && (conn <= MAX_CONN)) {

        params[0].param_code = COMMAND_CODE;
        params[0].param_value = CLOSE_CONNECTION;
        params[1].param_code = 0;        /* NULL terminated list              */
        params[1].param_value = 0;

        mymbuf = Build_Pdu (conn, SERVER_CMD_REQUEST_PDU, NULL, &params[0]);

        act_conn = Send_To_Conn (mymbuf, conn, NO_ALT_CONN);

        if (act_conn == 0) {

            sprintf(err_string.par1,"SERVER_CMD_REQUEST");
            sprintf(err_string.par2,"%d",conn);

            output_err("Parse_Close_Conn_Cmd",
                       MI_TNI_ERR_SEND,
                       MX_ERR_LVL_ERROR,
                       err_string);

            close_conn_status = UNABLE_TO_SEND;
        } else {

            sprintf(err_string.par1,"CLOSE");
            sprintf(err_string.par2,"%d",act_conn);

            output_err("Parse_Close_Conn_Cmd",
                       MI_TNI_CMD_CONN,
                       MX_ERR_LVL_INFO,
                       err_string);

            tnicon_p->srv_cmd_req_cnt_tot[act_conn]++;
            close_conn_status = GOOD_CMD;
        }
    } else {


        close_conn_status = INV_ARG_INT1;

        sprintf(err_string.par1,"%d",request->command_group);
        sprintf(err_string.par2,"%d",request->command_type);
        sprintf(err_string.par3,"1");
        sprintf(err_string.par4,"%d",request->cmd_arg_int1);

        output_err("Parse_Close_Conn_Cmd",
                   MI_TNI_CMD_INV_ARG,
                   MX_ERR_LVL_ERROR,
                   err_string);
    }
    return(close_conn_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Overide_Prim_Cmd(request)                                            */
/*                                                                            */
/* Purpose: This function parses the override primary notification mode       */
/*          command.                                                          */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          overide_prim_status                                               */
/*                           GOOD_CMD - command successfully processed        */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Overide_Prim_Cmd(COMMAND_INFO *request)
{
    int overide_prim_status;

    err_string = null_err_string;

    overide_prim_status = GOOD_CMD;

/*  This command can only be executed when the MX Server is not in the        */
/*  PRIMARY state.  Otherwise, is has no meaning.                             */

    if (tnicon_p->tni_server_state != PRIMARY) {
    
        tnicon_p->prim_notify_overide = 1;


        sprintf(err_string.par1,"Override Primary Notify");
        sprintf(err_string.par2,"DISABLE");
        sprintf(err_string.par3,"ENABLE");

        output_err("Parse_Overide_Prim_Cmd",
                   MI_TNI_CMD_CHNG,
                   MX_ERR_LVL_INFO,
                   err_string);
    } else {

        output_err("Parse_Overide_Prim_Cmd",
                   MI_TNI_NO_OVERRIDE,
                   MX_ERR_LVL_INFO,
                   err_string);

    }

    return(overide_prim_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Debug_Level_Cmd(request)                                             */
/*                                                                            */
/* Purpose: This function parses the change debug level command.              */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          debug_level_status                                                */
/*                           GOOD_CMD - command successfully processed        */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Debug_Level_Cmd(COMMAND_INFO *request)
{
    int debug_level_status;
    int temp_term_stats_idx = -1;

    err_string = null_err_string;

    debug_level_status = GOOD_CMD;

    switch (tnicon_p->dbg_state)
    {
    case DBG_OFF:

        if ((request->cmd_arg_int1 >= 0) &&
            (request->cmd_arg_int1 <= 8))
        {
            tnicon_p->dbg_state = DBG_INIT_FILE;
            Open_Dbg_file();

            sprintf(err_string.par1,"Debug level");
            sprintf(err_string.par2,"OFF");

            if (tnicon_p->dbg_state == DBG_ACTIVE)
            {
                sprintf(err_string.par3,"ON");
            }
            else
            {
                sprintf(err_string.par3,"DEBUG FILE FAILURE");
            }

            output_err("Parse_Debug_Level_Cmd",
                       MI_TNI_CMD_CHNG,
                       MX_ERR_LVL_INFO,
                       err_string);
        }
        break;

    case DBG_FILE_ERR:

        tnicon_p->print_flag = -1;
        break;
    }

    switch (request->cmd_arg_int1)
    {
    case 0:

        switch (tnicon_p->dbg_state) {

        case DBG_ACTIVE:

            tnicon_p->dbg_state = DBG_CLOSE;
            Close_Dbg_file();

            sprintf(err_string.par1,"Debug level");
            sprintf(err_string.par2,"ON");
            sprintf(err_string.par3,"OFF");

            output_err("Parse_Debug_Level_Cmd",
                       MI_TNI_CMD_CHNG,
                       MX_ERR_LVL_INFO,
                       err_string);

            break;

        case DBG_FILE_ERR:

            tnicon_p->dbg_state = DBG_OFF;
            break;
        }

        tnicon_p->print_flag = 0;
        tnicon_p->dbg_term_num = 0;
        break;

    case 1:

        tnicon_p->print_flag = SOCKET_LEVEL_DBG;
        break;

    case 2:

        tnicon_p->print_flag = PDU_LEVEL_DBG; 
        break;

    case 3:

        tnicon_p->print_flag = SOCKET_LEVEL_DBG + PDU_LEVEL_DBG;                 
        break;

    case 4:

        tnicon_p->print_flag = STATS_LEVEL_DBG;                 
        break;

    case 5:

        tnicon_p->print_flag = SOCKET_LEVEL_DBG + STATS_LEVEL_DBG;                 
        break;

    case 6:

        tnicon_p->print_flag = PDU_LEVEL_DBG + STATS_LEVEL_DBG;                 
        break;

    case 7:

        tnicon_p->print_flag = SOCKET_LEVEL_DBG + PDU_LEVEL_DBG + STATS_LEVEL_DBG;                 
        break;

    case 8:

        tnicon_p->print_flag = TERMINAL_LEVEL_DBG;

        switch(request->cmd_arg_int2) {

            case 0:

                tnicon_p->dbg_term_num = 0;
                break;

            case ALL_TERMINALS:

                tnicon_p->dbg_term_num = ALL_TERMINALS;
                break;

            default:

#               if defined(PROSYS_ENV_PLATFORM)

                    temp_term_stats_idx = retrieve_term_stats_idx((ubyte_4) request->cmd_arg_int2);

                    if (temp_term_stats_idx != -1)
                    {
                        tnicon_p->dbg_term_num = request->cmd_arg_int2;
                    }
                    else
                    {
                        tnicon_p->dbg_term_num = -2;
                    }

#               else

                    if ((request->cmd_arg_int2 > 0) && 
                        (request->cmd_arg_int2 <= max_terminal))
                    {
                        tnicon_p->dbg_term_num = request->cmd_arg_int2;
                    }
                    else
                    {
                        tnicon_p->dbg_term_num = -2;
                    }
                    break;

#               endif

        }
        break;

    default:

        debug_level_status = INV_ARG_INT1;
        break;
    }

    return(debug_level_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Log_Mode_Cmd(request)                                                */
/*                                                                            */
/* Purpose: This function parses the change log mode command.                 */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          log_mode_status                                                   */
/*                           GOOD_CMD - command successfully processed        */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Log_Mode_Cmd(COMMAND_INFO *request)
{
    int log_mode_status;

    err_string = null_err_string;

    log_mode_status = GOOD_CMD;

    switch (tnicon_p->log_state)
    {
    case LOG_OFF:
    case LOG_INIT_FILE:

        if (request->cmd_arg_int1 == 1)
        {
            tnicon_p->log_state = LOG_INIT_FILE;
            Open_Log_file();

            sprintf(err_string.par2,"DISABLED");

            if (tnicon_p->log_state == LOG_ACTIVE)
            {
                sprintf(err_string.par3,"ENABLED");
            }
            else
            {
                sprintf(err_string.par3,"LOG FILE FAILURE");
            }
        }
        else
        {
            log_mode_status = INV_ARG_INT1;
        }

        break;

    case LOG_ACTIVE:

        if (request->cmd_arg_int1 == 0)
        {
            tnicon_p->log_state = LOG_CLOSE;
            Close_Log_file();

            sprintf(err_string.par2,"ENABLED");
            sprintf(err_string.par3,"DISABLED");
        }
        else
        {
            log_mode_status = INV_ARG_INT1;
        }

        break;

    case LOG_FILE_ERR:

        sprintf(err_string.par2,"LOG FILE FAILURE");

        if (request->cmd_arg_int1 == 0)
        {
            tnicon_p->log_state = LOG_OFF;
            sprintf(err_string.par3,"DISABLED");
        }
        else
        {
            log_mode_status = INV_ARG_INT1;
        }

        break;
    }

    if (log_mode_status == GOOD_CMD)
    {
        sprintf(err_string.par1,"Logging mode");

        output_err("Parse_Log_Mode_Cmd",
                   MI_TNI_CMD_CHNG,
                   MX_ERR_LVL_INFO,
                   err_string);
    }

    return(log_mode_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Log_Path_Cmd(request)                                                */
/*                                                                            */
/* Purpose: This function parses the change log path command.                 */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          log_path_status                                                   */
/*                           GOOD_CMD - command successfully processed        */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Log_Path_Cmd(COMMAND_INFO *request)
{
    int current_cdc;
    int log_path_status;

    char *file_path;

    err_string = null_err_string;

    log_path_status = GOOD_CMD;

#   if defined(PROSYS_ENV_ALL)

        current_cdc = gblcom_p->hdr.cdc;

#    endif

#   if defined(GOLS_ENV_ALL)

        GET_CDC (&current_cdc);

#    endif

    sprintf(err_string.par1,"Log file");
    sprintf(err_string.par2,tnicon_p->log_file_name);

    file_path = request->cmd_arg_str1;

    sprintf(tnicon_p->log_file_name,
            "%smxsrv_event_%d.log",
            file_path,
            current_cdc);

    sprintf(err_string.par3,tnicon_p->log_file_name);

    /*
    ** Do not post message to ELOG when MX Server starts up
    */

    if (strcmp (err_string.par2, "Undefined") != 0)
    {
        output_err("Parse_Log_Path_Cmd",
                   MI_TNI_CMD_CHNG,
                   MX_ERR_LVL_INFO,
                   err_string);
    }

    return(log_path_status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Parse_Log_Threshold_Cmd(request)                                           */
/*                                                                            */
/* Purpose: This function parses the change log threshold command.            */
/*                                                                            */
/* Input Arguments:                                                           */
/*          request          structure containing command                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          log_threshold_status                                              */
/*                           GOOD_CMD - command successfully processed        */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Parse_Log_Threshold_Cmd(COMMAND_INFO *request)
{
    int log_threshold_status;

    err_string = null_err_string;

    log_threshold_status = GOOD_CMD;

    sprintf(err_string.par1, "Logging threshold");
    sprintf(err_string.par2, "%d", tnicon_p->log_threshold);

    if ((request->cmd_arg_int1 > 1) &&
        (request->cmd_arg_int1 <= 65535))
    {
        tnicon_p->log_threshold = request->cmd_arg_int1;

        sprintf(err_string.par3, "%d", tnicon_p->log_threshold);
    }
    else
    {
        log_threshold_status = INV_ARG_INT1;
    }

    if (log_threshold_status == GOOD_CMD)
    {
        output_err("Parse_Log_Threshold_Cmd",
                   MI_TNI_CMD_CHNG,
                   MX_ERR_LVL_INFO,
                   err_string);
    }

    return(log_threshold_status);
}
