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
/* Copyright 2007 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[TNICFG.H]==============================================================*/
/*                                                                            */
/* Purpose: This header file contains TNI configuration - Internal            */
/*          Definitions                                                       */
/*                                                                            */
/*====[TNICFG.H]==============================================================*/
/*                                                                            */

#ifndef TNICFG_H

#   define TNICFG_H 

/* TNI configuration file name */

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#       define SERVER_CFG_NAME       "$MXSRV/files/mx_server.fil"
#       define TNI_SERVER_CFG_NAME   "$MXSRV/files/tni_server.fil"
#       define LOG_FILE_PATH         "$LOGDIR/"

#   elif defined(XOS_VMS)

#       if defined(PROSYS_ENV_V7)

#           define SERVER_CFG_NAME       "MXSRVFILES:MX_SERVER.FIL"
#           define TNI_SERVER_CFG_NAME   "MXSRVFILES:TNI_SERVER.FIL"
#           define LOG_FILE_PATH         "LOGDIR:"

#       else

#           define SERVER_CFG_NAME       "GXTSK:MX_SERVER.FIL"
#           define TNI_SERVER_CFG_NAME   "GXTSK:TNI_SERVER.FIL"
#           define LOG_FILE_PATH         "SYSX:"

#       endif

#   else

#       error - OS-specific logic not handled.

#   endif

/* [Connection] Configuration Parameters */

#   define TNI_CFGCONN        "CONNECTION"
#   define TNI_CFGHOSTID      "HOST_ID"
#   define TNI_CFGREMSYS      "REMOTE_SYSTEM_NAME"

/* [Control] Configuration Parameters  */

#   define TNI_CFGCTL         "CONTROL"
#   define TNI_CFGTAKOVR      "TAKEOVER_INVL"
#   define TNI_CFGCONNATMPT   "CONN_ATTEMPT_INVL"
#   define TNI_CFGCHKCONN     "CHK_CONN_TIMER"
#   define TNI_CFGRDATMPT     "READ_ATTEMPT_INVL"
#   define TNI_CFGCYCLE       "TARGET_CYCLE_RATE"
#   define TNI_CFGSTATS       "STATISTICS_INVL"
#   define TNI_CFGSNDBUF      "RECEIVE_BUF_SIZE"
#   define TNI_CFGRCVBUF      "SEND_BUF_SIZE"
#   define TNI_CFGRDWRTOUT    "RD_WRT_TIMEOUT_INVL"
#   define TNI_CFGSTARTMODE   "START_MODE"
#   define TNI_CFGNOTIFYMODE  "PRIM_NOTIFY_MODE"
#   define TNI_CFGEVENTLOG    "EVENT_LOGGING"
#   define TNI_CFGESRQSTS     "MAX_ES_REQUESTS"
#   define TNI_CFGDEFRPCRQST  "DEF_RPC_REQUEST_TIMEOUT"
#   define TNI_CFGLOGTOMJF    "MJF_LOGGING"
#   define TNI_CFGLOGTOTMF    "TMF_LOGGING"
#   define TNI_CFGCONCNT      "CONNECTION_COUNT"
#   define TNI_CFGAUTHTSHOLD  "AUTH_FAILURE_THRESHOLD"
#   define TNI_CFGAUTHLOINVL  "AUTH_LOCKOUT_INVL"
#   define TNI_CFGRQSTDEST    "DEF_TERM_RQST_DEST"
#   define TNI_CFGWRTSELECT   "WRT_SELECT_TIMEOUT_INVL"
#   define TNI_CFGUNSONOTIFY  "UNSO_FAILURE_NOTIFICATION"
#   define TNI_CFGKEYSTORE    "KEYSTORE"
#   define TNI_CFG_PUBLICKEY  "PUBLIC_KEY"

/* [Clustered_App] Configuration Parameters  */

#   define TNI_CFGCLUSTAPP    "CLUSTERED_APP"
#   define TNI_CFGAPPNAME     "APP_NAME"
#   define TNI_CFGREMNAM      "REMOTE_DOMAIN_NAME"
#   define TNI_CFGOLTPROUT    "OLTP_UNSO_ROUTING"
#   define TNI_CFGRQSTROUT    "OLTP_RQST_ROUTING"
#   define TNI_CFGRPCRQST     "RPC_REQUEST_TIMEOUT"
#   define TNI_CFGAUTHREQ     "AUTHENTICATION_REQUIRED"
#   define TNI_CFGHASHALG     "AUTH_HASH_ALGORITHM"
#   define TNI_CFGSALTLEN     "AUTH_SALT_LENGTH"
#   define TNI_CFGESRPCENC    "ES_RPC_ENC_MODE"

/* [TNI_config] Configuration Parameters  */

#   define TNI_CONFIG         "TNI_CONFIG"
#   define TNI_CFGLOCNAM      "LOCAL_DOMAIN_NAME"
#   define TNI_CFGDTIDMTHD    "DEF_TERM_ID_METHOD"
#   define TNI_CFGLISTENPORT  "LISTENING_PORT"

/* [BUFFER_PARAMETERS] Buffer Parameters */

#   define TNI_CFGBUFPARAMS   "BUFFER_PARAMETERS"

#   define TNI_CFGP1BSIZE     "POOL1_BUF_SIZE"
#   define TNI_CFGP1BNUM      "POOL1_BUF_NUM"
#   define TNI_CFGP1BTHOLD    "POOL1_BUF_THOLD"

#   define TNI_CFGP2BSIZE     "POOL2_BUF_SIZE"
#   define TNI_CFGP2BNUM      "POOL2_BUF_NUM"
#   define TNI_CFGP2BTHOLD    "POOL2_BUF_THOLD"

#   define TNI_CFGP3BSIZE     "POOL3_BUF_SIZE"
#   define TNI_CFGP3BNUM      "POOL3_BUF_NUM"
#   define TNI_CFGP3BTHOLD    "POOL3_BUF_THOLD"

#   define TNI_CFGP4BSIZE     "POOL4_BUF_SIZE"
#   define TNI_CFGP4BNUM      "POOL4_BUF_NUM"
#   define TNI_CFGP4BTHOLD    "POOL4_BUF_THOLD"

#   define TNI_CFGP5BSIZE     "POOL5_BUF_SIZE"
#   define TNI_CFGP5BNUM      "POOL5_BUF_NUM"
#   define TNI_CFGP5BTHOLD    "POOL5_BUF_THOLD"

#   define TNI_CFGBUFLEN     12000  /* System configuration buffer length     */

#   define CTLE             ('E'<<8)

#   define LOGGING_THRESHOLD 65535

#endif 

