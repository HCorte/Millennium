/* */

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
/*====[MXVISION.H]============================================================*/
/*                                                                            */
/* Purpose: This header file contains MXVISION definitions                    */
/*                                                                            */
/*====[MXVISION.H]============================================================*/
/*                                                                            */

#include <curses.h>         

#ifndef MXVIS

#   define MXVIS
#   define VERSION MXSRV_RELEASE
 
#   if defined(PROSYS_ENV_ALL)

#      define TASK_NAME "MXVISION"

#   endif

#   define MAXCOL 80

    extern unsigned char tnivis_fmt[24][80];
    extern WINDOW *win;

#   define  MAIN_MENU          0x01
#   define  CONNECTION         0x02
#   define  HCONNECTIONS       0x03
#   define  APPLICATION        0x04 
#   define  RPC                0x05 
#   define  SERVER_SUMMARY     0x06 
#   define  TERMINAL           0x07 
#   define  DEBUG              0x08
#   define  TERM               0x09
#   define  MXVIS_STOP         0x00
#   define  LOG                0x0A
#   define  CHNG_LOG_FILE      0x0B
#   define  CHNG_LOG_THRESHOLD 0x0C
#   define  DEBUG_TERMINAL     0x0D
#   define  EVENT_QUEUES       0x0E
#   define  CLIENTID           0x0F

#   define  PAGE_NEUTRAL       0
#   define  PAGE_FORWARD       1
#   define  PAGE_BACKWARD      2

#   ifdef DECLARE_MXVISION_GLOBAL_DATA

#       define MXVIS_GBL

#   else

#       define MXVIS_GBL extern

#   endif

    MXVIS_GBL int current_menu;
    MXVIS_GBL int input_y;
    MXVIS_GBL int input_x;
    MXVIS_GBL int max_terminal;
    MXVIS_GBL int outstanding_cmd;

    static char *states[] = {
        "Undefined     ",
        "Defined       ",
        "Init Failure  ",
        "Down          ",
        "Conn Pending  ",
        "Closed        ",
        "Connected     ",
        "Not Primary   ",
        "Server Primary",
        "Client Primary",
        "Primary Pend  ",
        "Primary       "
    };

    static char *term_id_methods[] = {
        "                   ",
        "Terminal Client Id ",
        "Terminal Client Tag",
        "                   ",
        "Terminal Server Id "
    };

    static char *msg_types[] = {
        "OLTP",
        "RPC "
    };

    static char *queue_names[] = {
        "None   ",
        "FREE   ",
        "REQUEST"
    };

    static char *app_states[] = {
        "UNDEFINED",
        "DEFINED  "
    };

    static char *oltp_unso_route[] = {
        "ROUND_ROBIN",
        "ALL_CLIENTS",
        "OFF        "
    };

    static char *auth_states[] = {
        "Not Attempted         ",
        "Not Attempted (Locked)",
        "In Progress           ",
        "Successful            ",
        "Failed                "
    };

    static char *hash_algorithms[] = {
        "MD5   ",
        "SHA1  ",
        "SHA256",
        "SHA384",
        "SHA512"
    };

    static char *encryption_states[] = {
        "No Config ",
        "Configured",
        "Not Avail ",
        "Available ",
        "Disabled  ",
        "On Demand ",
        "All Msgs  ",
        "No Key    "
    };

    static char *exchange_methods[] = {
        "---------",
        "RSA      "
    };

    static char *encryption_key_types[] = {
        "---------",
        "3DES     ",
        "AES-128  ",
        "AES-256  ",
        "RSA-DER  ",
        "RSA-PEM  "
    };

#endif

#if defined(XOS_VMS)

#   define TIME_CVT_FLAG 1                /* time only, no date for conversion   */

#endif
