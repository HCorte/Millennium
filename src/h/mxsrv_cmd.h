/*  */

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
/* Copyright 2003 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[MXSRV_CMD.H]===========================================================*/
/*                                                                            */
/* Purpose: This header file contains definitions for command function        */
/*          processing.                                                       */
/*                                                                            */
/*====[MXSRV_CMD.H]===========================================================*/
/*                                                                            */

#ifndef MXSRV_CMD_H
#define MXSRV_CMD_H

#define MAX_CMD_ARG_STR 32

/* Task request types */

#define MXSRV_TASK_CMD 1

/* Commands groups */

#define MXSRV_VISION_CMD   1
#define MXSRV_INTERNAL_CMD 2

/* Commands type */

#define CHANGE_COMM_MODE     1
#define CLOSE_CLIENT_CONN    2
#define OVERIDE_PRIM_NOTIFY  3
#define CHANGE_DEBUG_LEVEL   4
#define CHANGE_LOG_MODE      5
#define CHANGE_LOG_PATH      6
#define CHANGE_LOG_THRESHOLD 7


/* Commands result codes */

#define GOOD_CMD          0
#define UNKNOWN_CMD_GROUP 1
#define UNKNOWN_CMD       2
#define INV_ARG_INT1      3
#define INV_ARG_INT2      4
#define INV_ARG_STR1      5
#define INV_ARG_STR2      6
#define UNABLE_TO_SEND    7

#if defined(PROSYS_ENV_PLATFORM)

    XCC_ALIGN_SAVE
    XCC_ALIGN_BYTE_1

#else

#   if defined(XCC_XLC)

#       pragma options align=packed

#   elif defined(XCC_VMS) || defined(XCC_DECC)

#       pragma member_alignment save
#       pragma nomember_alignment

#   else

#       error - Compiler-specific logic not handled.

#   endif

#endif

typedef struct {

    unsigned int command_group;
    unsigned int command_type;
    int cmd_arg_int1;
    int cmd_arg_int2;
    char cmd_arg_str1[MAX_CMD_ARG_STR];
    char cmd_arg_str2[MAX_CMD_ARG_STR];

}  COMMAND_INFO;

struct MXSRV_TASK_RQST        /* other tasks to MXSRV request format  */
{
    int func;                 /* what is requested eg. */
    int reqlen;               /* request body length  */
    union rqst_tag {
        COMMAND_INFO vis_cmd;  
        char data[MXSRV_MAX_APP_MSG]; /* request body */
    } rqst;
};

typedef struct {

    int result_code;

}  COMMAND_RESP;

#if defined(PROSYS_ENV_PLATFORM)

    XCC_ALIGN_RESTORE

#else

#   if defined(XCC_XLC)

#       pragma options align=reset

#   elif defined(XCC_VMS) || defined(XCC_DECC)

#       pragma member_alignment restore

#   else

#       error - Compiler-specific logic not handled.

#   endif

#endif

#endif
