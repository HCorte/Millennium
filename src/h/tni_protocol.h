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
/*====[TNI_PROTOCOL.H]========================================================*/
/*                                                                            */
/* Purpose: This header file contains all definitions and data structures     */
/*          which are required to implement the TNI protocol.                 */
/*                                                                            */
/*====[TNI_PROTOCOL.H]========================================================*/
/*                                                                            */

#ifndef TNI_PROTOCOL_H
#define TNI_PROTOCOL_H

#include <limits.h>

#define NUMBER_OF_TNI_VERSIONS 6

/*                                                                         */
/* list of TNI protocol version                                            */
/*                                                                         */

#define TNI_VERSION_UNKNOWN 0x00
#define TNI_VERSION_30      0x30
#define TNI_VERSION_23      0x23
#define TNI_VERSION_22      0x22
#define TNI_VERSION_21      0x21
#define TNI_VERSION_02      0x02
#define TNI_VERSION_01      0x01

#define DEFAULT_TNI_VERSION TNI_VERSION_30

/*                                                                         */
/* definitions of fixed header part of PDUs                                */
/*                                                                         */

#define TNI_PROTOCOL 0xCC

/*                                                                         */
/* definitions of TNI pdu types                                            */
/*                                                                         */

#define CLIENT_REQUEST_PDU       0x01
#define CLIENT_RESPONSE_PDU      0x81
#define SERVER_REQUEST_PDU       0x02
#define SERVER_RESPONSE_PDU      0x82
#define TERMINAL_CLIENT_DATA_PDU 0x03
#define TERMINAL_SERVER_DATA_PDU 0x04
#define CLIENT_ALIVE_PDU         0x05
#define SERVER_ALIVE_PDU         0x06
#define ERROR_PDU                0x07
#define CLIENT_CMD_REQUEST_PDU   0x08
#define CLIENT_CMD_RESPONSE_PDU  0x88
#define SERVER_CMD_REQUEST_PDU   0x09
#define SERVER_CMD_RESPONSE_PDU  0x89
#define RPC_CLIENT_DATA_PDU      0x0A
#define RPC_SERVER_DATA_PDU      0x0B
#define CLT_SESSION_RQST_PDU     0x0C
#define CLT_SESSION_RESP_PDU     0x8C
#define SRV_CHALLENGE_RQST_PDU   0x0D
#define SRV_CHALLENGE_RESP_PDU   0x8D
#define SRV_CHALLENGE_NOTIFY_PDU 0x0E
#define SRV_ENC_KEY_RQST_PDU     0x0F
#define SRV_ENC_KEY_RESP_PDU     0x8F
#define CLT_ENC_KEY_NOTIFY_PDU   0x10

/*                                                                         */
/* definitions of TNI parameter codes                                      */
/*                                                                         */

#define CLIENT_ID         0x01
#define MAX_MESSAGE_SIZE  0x02
#define MAX_PDU_SIZE      0x03
#define MSG_BLOCK_TIME    0x04
#define KEEP_ALIVE_TIME   0x05
#define NUMBER_MESSAGES   0x06
#define TERM_CLIENT_ID    0x07
#define TERM_CLIENT_TAG   0x08
#define TERM_SERVER_ID    0x09
#define DELIVERY_MODE     0x0A
#define MSG_LENGTH        0x0B
#define PRIMARY_STATUS    0x0C
#define TERM_METHOD       0x0D
#define COMMAND_CODE      0x0E
#define APPLICATION_NAME  0x0F
#define MESSAGING_TYPE    0x10
#define RPC_REQUEST_TAG   0x11
#define SESSION_TAG       0x12
#define CORRELATION_TAG   0x13
#define HASH_ALGORITHM    0x14
#define SALT_LENGTH       0x15
#define HASH_LENGTH       0x16
#define ENC_KEY_X_METHOD  0x17
#define ENC_KEY_TYPE      0x18
#define ENC_KEY_LENGTH    0x19
#define ENC_MODE          0x1A
#define RESULT_CODE       0x81

/*                                                                         */
/* total number of TNI parameter codes defined above                       */
/*                                                                         */

#define MAX_PARAM_CODE_COUNT  26

/*                                                                         */
/* definitions of primary status                                           */
/*                                                                         */

#define NOT_PRIMARY       0x00
#define PRIMARY           0x01

/*                                                                         */
/* definitions of delivery codes                                           */
/*                                                                         */

#define REQUEST           0x00
#define RESPONSE          0x01
#define UNSOLICITED       0x02
#define MX_COMMAND        0x03
#define GOLS_RPC_REQUEST  0x04
#define BROADCAST         0xFF

/*                                                                         */
/* definitions of TNI version 1 delivery codes                             */
/*                                                                         */

#define RESPONSE_TNI_V1     0x00
#define UNSOLICITED_TNI_V1  0x01
#define MX_COMMAND_TNI_V1   0x02
#define BROADCAST_TNI_V1    0xFF

/*                                                                         */
/* definitions of result codes                                             */
/*                                                                         */

#define SUCCESS              0x00
#define INVALID_PDU          0x01
#define TERMINAL_UNKNOWN     0x02
#define MSG_UNDELIVERABLE    0x03
#define DUAL_PRIMARY         0x04
#define INV_TERM_METHOD      0x05
#define APP_NO_CONFIG        0x06
#define SRV_CMD_FAILURE      0x07
#define TRANSLET_ERROR       0x08
#define ES_CMD_SRV_DELIVERY  0x09
#define INVALID_SESSION_TAG  0x0A
#define CLT_NOT_IDENTIFIED   0x0B
#define NOT_AUTHENTICATED    0x0C
#define HASHALG_NOT_SUPPORTED  0x0D
#define CREDENTIALS_NOTAVL     0x0E
#define AUTHENTICATION_FAILURE 0x0F
#define MSG_TOO_BIG          0x10
#define ENC_KEY_X_METH_NOSUP 0x11
#define ENC_KEY_TYPE_NOSUP   0x12
#define INVALID_ENC_KEY      0x13
#define ENCRYPT_FAILURE      0x14
#define DECRYPT_FAILURE      0x15

/*                                                                         */
/* definitions of command codes                                            */
/*                                                                         */

#define CLOSE_CONNECTION  0x00

/*                                                                         */
/* definitions of TNI Extended Terminal Message Header status codes        */
/*                                                                         */

#define EXT_SUCCESS        0x00
#define EXT_INV_CONN       0x01
#define EXT_NO_TERM_METHOD 0x02
#define EXT_NO_CONN_AVAIL  0x03
#define EXT_BAD_CONVERSION 0x04

/*                                                                         */
/* definitions of terminal identification methods                          */
/*                                                                         */

#define METH_TERM_CLIENT_ID  0x01
#define METH_TERM_CLIENT_TAG 0x02
#define METH_TERM_SERVER_ID  0x04

#define DEF_TERM_ID_METH   METH_TERM_SERVER_ID

/*                                                                         */
/* definition of messaging types values                                    */
/*                                                                         */

#define MSG_TYPE_OLTP     0x00
#define MSG_TYPE_ES_RPC   0x01

/*                                                                         */
/* definitions of authentication methods                                   */
/*                                                                         */

#define MD5_HASH            0x00
#define SHA1_HASH           0x01
#define SHA256_HASH         0x02
#define SHA384_HASH         0x03
#define SHA512_HASH         0x04

/*                                                                         */
/* definitions of encryption key exchange methods                          */
/*                                                                         */

#define METH_ENC_KEY_X_NONE  0x00
#define METH_ENC_KEY_X_RSA   0x01

/*                                                                         */
/* definitions of encryption key types                                     */
/*                                                                         */

#define ENC_KEY_NONE           0x00
#define ENC_KEY_3DES           0x01
#define ENC_KEY_AES_128_CBC    0x02
#define ENC_KEY_AES_256_CBC    0x03
#define ENC_KEY_RSA_PUBLIC_DER 0x04
#define ENC_KEY_RSA_PUBLIC_PEM 0x05

/*                                                                         */
/* definitions of encryption modes                                         */
/*                                                                         */

#define ENC_NONE            0x00
#define ENC_RQST            0x01
#define ENC_RESP            0x02
#define ENC_RQST_RESP       0x03
#define ENC_UNSO            0x04

/*                                                                         */
/* definitions of default parameter values                                 */
/*                                                                         */

#define DEF_MAX_MGS_PER_PDU 0
#define DEF_PDU_SIZE 1024
#define DEF_BLOCKING_TIME 10                     /* in 10 millisecond tics */
#define DEF_KEEPALIVE_TIME 10                    /* in seconds             */
#define DEF_DELIVERY_MODE RESPONSE
#define DEF_PRIMARY_STATUS NOT_PRIMARY
#define DEF_TAKEOVER_TIME 30                     /* in seconds             */
#define DEF_CONN_ATMPT_TIME 60                   /* in seconds             */
#define DEF_CHECK_CONN_TIME 2                    /* in seconds             */
#define DEF_READ_ATMPT_TIME 1                    /* in 10 millisecond tics */
#define DEF_CYCLE_RATE 5                         /* in milliseconds        */
#define DEF_STATS_TIME 10                        /* 10 seconds             */
#define DEF_TCP_BUF_SIZE 60000                   /* bytes                  */
#define DEF_RDWRT_TOUT_INVL 50                   /* in 10 msec tics        */
#define DEF_START_MODE COMM_DISABLED
#define DEF_PRIM_NOTIFY_MODE NOTIFY_WAIT
#define DEF_APPLICATION_NAME "Not Specified"
#define DEF_MESSAGING_TYPE MSG_TYPE_OLTP
#define DEF_RPC_RQST_TIMEOUT 10
#define DEF_MAX_PDU_SIZE_VAL 1024
#define DEF_AUTH_HASH_ALGORITHM   SHA256_HASH
#define DEF_AUTH_SALT_LENGTH 16
#define DEF_AUTH_LOCKOUT_INVL   15              /* in minutes             */
#define DEF_AUTH_FAIL_TSHOLD    3
#define DEF_AUTH_ATMPT_TIMEOUT  10              /* in seconds             */
#define DEF_WRT_SELECT_TIMEOUT 1                /* in microseconds         */
#define DEF_ENC_KEY_X_METHOD METH_ENC_KEY_X_NONE
#define DEF_ENC_KEY_TYPE ENC_KEY_NONE
#define DEF_ENC_MODE ENC_NONE

/*                                                                         */
/* definitions of maximum parameter values                                 */
/*                                                                         */

#define MAX_CLIENT_ID_VAL 254                  /* max value for client id  */
#define MAX_MAX_MGS_PER_PDU_VAL 100            
#define MAX_BLOCKING_TIME_VAL 1000             /* value is in 10 msec tics */
#define MAX_KEEPALIVE_TIME_VAL 1000            /* value is in 10 msec tics */
#define MAX_PRIMARY_STATUS_VAL PRIMARY
#define MAX_TERM_METHOD_VAL 4                  /* Host Terminal Tag        */
#define MAX_TAKEOVER_TIME_VAL 180              /* value is in seconds      */
#define MAX_CONN_ATMPT_TIME_VAL 300            /* in seconds               */
#define MAX_CHECK_CONN_TIME_VAL 30             /* value is in seconds      */
#define MAX_READ_ATMPT_TIME_VAL 10             /* value is in 10 msec tics */
#define MAX_CYCLE_RATE_VAL 500                 /* in milliseconds          */
#define MAX_STATS_TIME_VAL 300                 /* seconds                  */
#define MAX_TCP_BUF_SIZE_VAL 5000000           /* bytes                    */
#define MAX_RDWRT_TOUT_INVL 100                /* in 10 msec tics          */
#define MAX_AUTH_SALT_LENGTH    32             /* bytes                    */
#define MAX_AUTH_LOCKOUT_INVL   1440           /* in minutes               */
#define MAX_AUTH_FAIL_TSHOLD    25
#define MAX_MESSAGING_TYPE MSG_TYPE_ES_RPC
#define MAX_WRT_SELECT_TOUT_INVL 500           /* in milliseconds          */
#define MAX_ENC_KEY_X_METHOD METH_ENC_KEY_X_RSA
#define MAX_ENC_KEY_TYPE ENC_KEY_RSA_PUBLIC_PEM
#define MAX_ENC_MODE ENC_UNSO

/*                                                                         */
/* definitions of minimum parameter values                                 */
/*                                                                         */

#define MIN_CLIENT_ID_VAL 1
#define MIN_MAX_MGS_PER_PDU_VAL 0
#define MIN_PDU_SIZE_VAL sizeof (struct TNI_FIXED_HDR)
#define MIN_BLOCKING_TIME_VAL 1                /* value is in 10 msec tics */
#define MIN_KEEPALIVE_TIME_VAL 1               /* value is in 10 msec tics */
#define MIN_PRIMARY_STATUS_VAL NOT_PRIMARY
#define MIN_TERM_METHOD_VAL 1                  /* Host Terminal ID         */
#define MIN_TAKEOVER_TIME_VAL 1                /* value is in seconds      */
#define MIN_CONN_ATMPT_TIME_VAL 1              /* in seconds               */
#define MIN_CHECK_CONN_TIME_VAL 1              /* value is in seconds      */
#define MIN_READ_ATMPT_TIME_VAL 1              /* value is in 10 msec tics */
#define MIN_CYCLE_RATE_VAL 1                   /* in milliseconds          */
#define MIN_STATS_TIME_VAL 10                  /* seconds                  */
#define MIN_TCP_BUF_SIZE_VAL 4096              /* bytes                    */
#define MIN_RDWRT_TOUT_INVL 30                 /* in 10 msec tics          */
#define MIN_AUTH_SALT_LENGTH    16             /* bytes                    */
#define MIN_AUTH_LOCKOUT_INVL   5              /* in minutes               */
#define MIN_AUTH_FAIL_TSHOLD    3
#define MIN_MESSAGING_TYPE MSG_TYPE_OLTP
#define MIN_WRT_SELECT_TOUT_INVL 1         /* in milliseconds          */
#define MIN_ENC_KEY_X_METHOD METH_ENC_KEY_X_NONE
#define MIN_ENC_KEY_TYPE ENC_KEY_NONE
#define MIN_ENC_MODE ENC_NONE

/*                                                                         */
/* definitions of sending flags for TERMINAL DATA PDUs                     */
/*                                                                         */

#define APPEND_ONLY             0
#define APPEND_AND_SEND         1
#define SEND_IMMEDIATE          2
#define SEND_UNENC_AND_SEND_ENC 3

/*                                                                         */
/* definitions of flags for send_to_host routine                           */
/* These flags control whether the attempted connection's alternate        */
/* connection is to be tried if a write error occurs. Also define max # of */
/* alternate connections to be tried                                       */
/*                                                                         */

#define NO_ALT_CONN             0         /* don't try alternate connection                       */
#define ALT_CONN                1         /* try alternate connection                             */
#define CHKAUTH_NO_ALT_CONN     2         /* check authentication, don't try alternate connection */
#define CHKAUTH_ALT_CONN        3         /* check authentication, try alternate connection       */

#define MAX_SEND_RETRIES        1         /* max # of alternate            */

/*                                                                         */
/* definitions of connection states                                        */
/*                                                                         */

#define CONN_UNDEFINED            0x00
#define CONN_DEFINED              0x01
#define CONN_INIT_FAILURE         0x02
#define CONN_DOWN                 0x03
#define CONN_CONNECT_PENDING      0x04
#define CONN_CLOSED               0x05
#define CONN_CONNECTED            0x06
#define CONN_NOT_PRIMARY          0x07
#define CONN_SWITCH_PRIMARY       0x08
#define CONN_HOST_PRIMARY         0x09
#define CONN_PRIMARY_PENDING      0x0A
#define CONN_PRIMARY              0x0B

/*                                                                         */
/* definitions of TNI server and last host state combinations              */
/*                                                                         */

#define BOTH_PRIMARY              0x01     /* TNI and host are primary     */
#define TNI_PRIMARY               0x02     /* TNI primary, host not primary*/
#define HOST_PRIMARY              0x03     /* TNI not primary, host primary*/
#define NEITHER_PRIMARY           0x04     /* TNI and host are not primary */

/*                                                                         */
/* definitions of authentication states                                    */
/*                                                                         */

#define AUTH_NOTATTEMPTED         0x00
#define AUTH_NOTATTEMPTED_LOCKED  0x01
#define AUTH_ATTEMPTED            0x02
#define AUTH_SUCCESS              0x03
#define AUTH_FAILURE              0x04

/*                                                                         */
/* definitions for Application Authentication Lockout                      */
/*                                                                         */

#define APP_AUTH_NOTLOCKED  0x00
#define APP_AUTH_LOCKED     0x01

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

struct TNI_FIXED_HDR {
   unsigned char proto_id;
   unsigned char version_id;
   unsigned short pdu_length;
   unsigned char pdu_type;
   };

struct PDU_STRUCT {
   struct TNI_FIXED_HDR fixed_header;
   unsigned char msg_data[1];
};

union PARAM_DATA {
   unsigned int i_param_value;
   unsigned short int s_param_value;
   unsigned char param_value[MAX_PARAM_DATA];
   };

struct TNI_DATA {
   unsigned char param_code;
   unsigned char param_length;
   union PARAM_DATA param_data;
   };

struct TNI_PARAM_PAIR {
   unsigned char param_code;
   unsigned int param_value;
   char param_value_char[MAX_RPC_RQST_TAG_LEN + 1];
   };

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
