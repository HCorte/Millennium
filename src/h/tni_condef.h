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
/*====[TNI_CONDEF.H]==========================================================*/
/*                                                                            */
/* Purpose: This header file contains all the TNI configurabile definitions   */
/*                                                                            */
/*====[TNI_CONDEF.H]==========================================================*/
/*                                                                            */

#define DEFAULT_LISTEN_PORT 53380     /* TNI's well known port number         */
#define LISTEN_WAIT_SEC 0             /* seconds for listening socket to wait */
                                      /* for new connections                  */
#define LISTEN_WAIT_USEC 1            /* microseconds for listening socket to */
                                      /* wait for new connections             */
#define MAX_NUM_CLIENT_IDS 64         /* count of max # of client ids         */

#define MAX_RESERVED_CONN 5

#ifdef XCF_MXSRV_MAX_CONN
#   if (XCF_MXSRV_MAX_CONN <= MAX_RESERVED_CONN)
#       error "XCF_MXSRV_MAX_CONN site configuration value is out of allowed range."
#   else
#       define MAX_CONN XCF_MXSRV_MAX_CONN
#   endif
#else
#   define MAX_CONN 64                /* count of max # of connections on TNI */
#endif

#define MAX_CONN_PLUS_ONE MAX_CONN + 1
#define MAX_CONFIGURABLE_CONN MAX_CONN - MAX_RESERVED_CONN /* count of max # of configurable  */
                                      /* connections                          */
#define MAX_CONNS_PER_CLIENT 10       /* max # of connections from a single   */
                                      /* client id                            */

#ifdef XCF_MXSRV_MAX_CONN_PER_APP
#   define MAX_CONN_PER_APP XCF_MXSRV_MAX_CONN_PER_APP
#else
#   define MAX_CONN_PER_APP 10        /* maximum number of connection for a   */
                                      /* single applicaton                    */
#endif

#ifdef XCF_MXSRV_MAX_INTERFACES_PER_TNI
#   define MAX_INTERFACES_PER_TNI XCF_MXSRV_MAX_INTERFACES_PER_TNI
#else
#   define MAX_INTERFACES_PER_TNI 2   /* max # of UCX devices on a single TNI */
#endif

#define MAX_INTERFACES_PER_CLIENT 2   /* max # of UCX devices on a single     */
                                      /* client machine                       */
#define MAX_TNI_BUF_LEN 5100          /* max # of bytes we can have in one    */
                                      /* read from or write to a host. Usually*/
                                      /* set to max number of bytes in an     */
                                      /* Ethernet frame                       */
#define MAX_CLIENT_NAME_LEN 64 
#define MAX_EXCLUSIVE_CONN 4          /* Number of exclusive connections per  */
                                      /* connection                           */
#define MAX_TERM_CLIENT_TAG_LEN 32    /* maximum  number of characters in a   */
                                      /* host terminal tag                    */
#define MAX_RESP_SAMPLE 50            /* max # of game response time to sample*/
                                      /* for min, max, and ave calculations   */
#define MAX_PARAM_DATA  64            /* max # of bytes in a TNI parameter    */
                                      /* value                                */
#define MAX_RPC_RQST_TAG_LEN 64

#ifdef XCF_MXSRV_MAX_APPS
#   define MAX_APPS XCF_MXSRV_MAX_APPS
#else
#   define MAX_APPS 10
#endif

#define MAX_SESSION_TAG_LEN MAX_RPC_RQST_TAG_LEN

#define MAX_CORRELATION_TAG_LEN MAX_RPC_RQST_TAG_LEN

#define MAX_AUTH_HASH_LENGTH  128
