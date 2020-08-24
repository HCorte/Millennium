static const char *fileid = "";

/*
 * ===[mx_event_log.c]====================================================
 *
 * Description:
 *
 * Functions to determine when the event log file is opened and closed.
 *
 * Functions:
 *
 * init_event_log
 * switch_event_log
 *
 * -----------------------------------------------------------------------
 * This item is the property of GTECH Corporation, Providence,
 * Rhode Island, and contains confidential and trade secret information.
 * It may not be transfered from the custody or control of GTECH except
 * as authorized in writing by an officer of GTECH.  Neither this item
 * nor the information it contains may be used, transfered, reproduced,
 * published, or disclosed, in whole or in part, and directly or
 * indirectly, except as expressly authorized by an officer of GTECH,
 * pursuant to written agreement.
 *
 * Any and all modifications to this item must have the prior written
 * authorization of GTECH's Enterprise Series Platform Team.  GTECH shall
 * not be liable in any way for any direct or indirect damages,  whatsoever,
 * as a result of any unauthorized modifications.  The Enterprise Series
 * Platform Team reserves the right to refuse support as a result of
 * unauthorized modification.
 *
 * Copyright (c) 2005 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * =======================================================================
 */

#include "includes.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include "tnicfg.h"

#elif defined(XOS_VMS)

#   include "tnicfg.h"

#else

#   error - OS-specific logic not handled.

#endif

/* [init_event_log]
 *
 * Summary:
 *
 * init_event_log(void)
 *
 * Description:
 *
 * This function creates the event log file name by executing a change log
 * path command.  If event logging has been enabled in the MX Server
 * configuration file, then the log files is opened.
 *
 * Returns Values: None
 *
 */

void
init_event_log(void)
{
    int                 rc = GOOD_CMD;

    COMMAND_INFO        command;

    memset(&command, 0, sizeof(COMMAND_INFO));

    /*
    ** Create and execute the change log file path command
    */

    command.command_group = MXSRV_INTERNAL_CMD;
    command.command_type = CHANGE_LOG_PATH;

    sprintf(command.cmd_arg_str1,
            "%s",
            LOG_FILE_PATH);

    rc = Process_Command(&command);

    /*
    ** If event logging is enabled, then open the log file
    */

    if ((tnicon_p->log_state == LOG_INIT_FILE) &&
        (rc == GOOD_CMD))
    {
        memset(&command, 0, sizeof(COMMAND_INFO));

        command.command_group = MXSRV_INTERNAL_CMD;
        command.command_type = CHANGE_LOG_MODE;

        command.cmd_arg_int1 = 1;

        rc = Process_Command(&command);

        if (rc != GOOD_CMD)
        {
            tnicon_p->log_state = LOG_OFF;
        }
    }

    return;
}

/* [switch_event_log]
 *
 * Summary:
 *
 * switch_event_log(void)
 *
 * Description:
 *
 * This function closes the event log file if it is open. The init_event_log 
 * is called to open the event log file for the next day if the event logging
 * is activated.
 *
 * Returns Values: None
 *
 */

void
switch_event_log(void)
{
    int                 rc = GOOD_CMD;

    COMMAND_INFO        command;

    /*
    ** If the event log file is open, then close it.  Set the log state
    ** to force a new log file to be opened.
    */

    if (tnicon_p->log_state == LOG_ACTIVE)
    {
        memset(&command, 0, sizeof(COMMAND_INFO));

        command.command_group = MXSRV_INTERNAL_CMD;
        command.command_type = CHANGE_LOG_MODE;

        command.cmd_arg_int1 = 0;

        rc = Process_Command(&command);

        if (rc == GOOD_CMD)
        {
            tnicon_p->log_state = LOG_INIT_FILE;
        }
    }

    init_event_log(); 

    return;
}
