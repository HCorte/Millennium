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
/*====[MXSRV_COMMON.H]========================================================*/
/*                                                                            */
/* Purpose: This header file contains definitions common to both ESTE,        */
/*          PRO:SYS, and AlphGOLS systems.                                    */
/*                                                                            */
/*====[MXSRV_COMMON.H]========================================================*/
/*                                                                            */

#ifndef MXSRV_COMMON_H

#define MXSRV_COMMON_H


#ifdef XCF_MXSRV_MAX_APP_MSG
#   define MXSRV_MAX_APP_MSG XCF_MXSRV_MAX_APP_MSG
#else
#   define MXSRV_MAX_APP_MSG 5000
#endif

#define MAX_APP_MSG MXSRV_MAX_APP_MSG

#define MXSRV_MAX_APP_NAME_LEN 32

/* Generic product response codes */

#define MXSRV_RESP_ERR_STATS_IDX     -1
#define MXSRV_RESP_ERR_APP_NOTDEF     MXSRV_RPC_ERR_APP_NOTDEF
#define MXSRV_RESP_ERR_MSGTYP_NOSUPP  MXSRV_RPC_ERR_MSGTYP_NOSUPP
#define MXSRV_RESP_ERR_NOCONN         MXSRV_RPC_ERR_NOCONN
#define MXSRV_RESP_ERR_NOBUILD        MXSRV_RPC_ERR_NOBUILD
#define MXSRV_RESP_ERR_MSG_TOBIG      MXSRV_RPC_ERR_MSG_TOBIG
#define MXSRV_RESP_ERR_BAD_TER       -8

/* RPC messaging constants */

#define ES_RPC_REQ      3                           /* send RPC request to an ES application  */

/* RPC request option flags */

#define RPC_ACK 1
#define RPC_ENC_RQST 2
#define RPC_ENC_RESP 4

#define RPC_NO_ACK      0

/* RPC product error codes */

#define MXSRV_RPC_ERR_NOSAVE_TRANS   -1
#define MXSRV_RPC_ERR_APP_NOTDEF     -2
#define MXSRV_RPC_ERR_MAXRQSTS_EXCD  -3
#define MXSRV_RPC_ERR_MSGTYP_NOSUPP  -4
#define MXSRV_RPC_ERR_NOCONN         -5
#define MXSRV_RPC_ERR_NOBUILD        -6
#define MXSRV_RPC_ERR_MSG_TOBIG      -7
#define MXSRV_RPC_ERR_TRANSLET       -8
#define MXSRV_RPC_ERR_RQST_TIMEOUT   -9
#define MXSRV_RPC_ERR_CMDSRV_DELV    -10
#define MXSRV_RPC_ERR_NOENC          -11
#define MXSRV_RPC_ERR_NOENCKEY       -12
#define MXSRV_RPC_ERR_ENCRYPT        -13
#define MXSRV_RPC_ERR_DECRYPT        -14

/* RPC trabuf error codes */

#define RPC_ERR_LATE_RESP       1                   /* received late RPC response */
#define RPC_ERR_NO_OUTSTND_RQST 2                   /* received RPC response when no */
                                                    /* outstanding request exits */
#define RPC_ERR_INV_RQST_TAG    3                   /* received RPC response with an */
                                                    /* invalid RPC request tag */

/* Macro to set a specific RPC request option flag */

#define MXRPC_RQST_SET(msg, flag) ((RPC_RQST_FLAGS *)msg)->opt_1 = \
                                  ((RPC_RQST_FLAGS *)msg)->opt_1 | flag;

#define MXRPC_RQST_ISSET(msg, flag) (((RPC_RQST_FLAGS *)msg)->opt_1 & flag) \
                                    ? 1 : 0

/*
 * This structure is used to cast onto the ES_RPC_RQST.rpcAckRequired field...
 */
typedef struct
{
#if G_LITTLE_ENDIAN
    ubyte_1       opt_1;
    ubyte_1       opt_2;
    ubyte_1       opt_3;
    ubyte_1       opt_4;
#else
    ubyte_1       opt_4;
    ubyte_1       opt_3;
    ubyte_1       opt_2;
    ubyte_1       opt_1;
#endif
} RPC_RQST_FLAGS;

typedef struct
{
    int           rpcAckRequired;
    long          bodyLength;
    char          applicationName[MXSRV_MAX_APP_NAME_LEN + 1];
    unsigned char body[MXSRV_MAX_APP_MSG];

} ES_RPC_RQST;

/* Terminal request messaging constants */

#define TERMINAL_REQ   4

typedef struct {

    ubyte_4       ternum;
    byte_1        applicationName[MXSRV_MAX_APP_NAME_LEN + 1];
    byte_4        bodyLength;
    ubyte_1       body[MXSRV_MAX_APP_MSG];

} TERM_RQST;

#endif
