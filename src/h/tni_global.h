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
/*====[TNI_GLOBAL.H]==========================================================*/
/*                                                                            */
/* Purpose: This header file contains all data structures which are global to */
/*          the TNI server task.  They are not contained in the TNI global    */
/*          section.                                                          */
/*                                                                            */
/*====[TNI_GLOBAL.H]==========================================================*/
/*                                                                            */

#ifndef TNI_GBL_LOADED

#   define TNI_GBL_LOADED 1

/*
 * Version Identification...
 */
#   define VIP_NAME    "MXSRV Product"
#   define VIP_DESC    "MX Server Communications Product"

#   define VIP_MAJOR   3
#   define VIP_MINOR   0
#   define VIP_MICRO   2
#   define VIP_PATCH   ""

#   if !defined(PROSYS_ENV_PLATFORM)

#       define C_YEAR      "2010"

#   endif

#   ifdef LIB_DECLARE_DATA

#       define GLOBAL

#   else

#       define GLOBAL extern

#   endif

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#       include <time.h>
#       include <sys/time.h>
#       include <sys/timeb.h>
#       include <sys/socket.h>
#       include <netinet/in.h>

#       include "tni_condef.h"
#       include "tni_protocol.h"

#       if defined(PROSYS_ENV_ALL)

#           include "mxsrv_trabuf.h"

#       endif

#   elif defined(XOS_VMS)

#       include <time.h>
#       include <timeb.h>
#       include <socket.h>
#       include <in.h>

#       include "tni_condef.h"
#       include "tni_protocol.h"

#       if defined(PROSYS_ENV_ALL)

#           include "mxsrv_trabuf.h"

#       endif

#   else

#       error - OS-specific logic not handled.

#   endif

/*
 * Two levels of macros are needed for stringification.  Where a macro argument
 * occurs in the macro's body, that occurrence of that argument is stringified
 * on expansion if it is preceded by a single '#'...
 */
#   define _STRINGIFY(X) #X
#   define STRINGIFY(V) _STRINGIFY(V)

#   define MXSRV_RELEASE STRINGIFY(VIP_MAJOR) "." STRINGIFY(VIP_MINOR) "." STRINGIFY(VIP_MICRO) VIP_PATCH

#   define _MAX(a,b) ((a) > (b)) ? a : b
#   define _MIN(a,b) ((a) < (b)) ? a : b

#   define MS_IN_HUNDREDTHS 10           /* number of milliseconds in 1/100 sec  */
#   define MS_IN_SEC (MS_IN_HUNDREDTHS * 100) /* number of milliseconds in 1 sec */
#   define MS_IN_MIN (MS_IN_SEC * 60)    /* number of milliseconds in 1 min      */
#   define TICS_PER_SEC 100              /* number of 10 millisecond time tics   */

/* connection managment types                                                 */

#   define CONNS_CLIENT_ID 0
#   define CONNS_APP_NAME  1

/* application states                                                         */

#   define APP_UNDEFINED 0
#   define APP_DEFINED   1

/* debug levels                                                               */

#   define SOCKET_LEVEL_DBG    0x00000001   /* print socket level debug          */
#   define PDU_LEVEL_DBG       0x00000002   /* print protcol pdu level debug     */
#   define STATS_LEVEL_DBG     0x00000004   /* print statistics level debug      */
#   define TERMINAL_LEVEL_DBG  0x00000008   /* print terminal level debug        */

#   define  ALL_TERMINALS      999999

/* debug states                                                               */
#   define DBG_OFF 0                     /* debug not active                     */
#   define DBG_INIT_FILE 1               /* open debug report file               */
#   define DBG_ACTIVE 2                  /* debug is active                      */
#   define DBG_CLOSE 3                   /* terminate debug and close file       */
#   define DBG_FILE_ERR 4                /* error during file operation          */

/* logging states                                                             */
#   define LOG_OFF 0                     /* logging not active                   */
#   define LOG_INIT_FILE 1               /* open logging report file             */
#   define LOG_ACTIVE 2                  /* logging is active                    */
#   define LOG_CLOSE 3                   /* terminate logging and close file     */
#   define LOG_FILE_ERR 4                /* error during file operation          */

/* MX Server communication modes                                              */
#   define COMM_DISABLED  0              /* do not allow communictions           */
#   define COMM_ENABLE    1              /* request communications to be enabled */
#   define COMM_ENABLED   2              /* communicate normally                 */
#   define COMM_SYNC      3              /* synchronize MX communiction with     */
                                         /* other comm subsystems                */
#   define COMM_INSYNC    4              /* synchronization complete             */
#   define COMM_SHUTDOWN  5              /* stop the MX Server                   */

/* Primary Server notification modes                                          */
#   define NOTIFY_IMMEDIATELY  0         /* do not wait for all defined          */
                                         /* connections to connect to send       */
                                         /* primary notification to clients      */
#   define NOTIFY_WAIT         1         /* wait for all defined connections     */
                                         /* to connect to send before sending    */
                                         /* primary notification to clients      */

/* RPC response time calculation indicators */

#   define NO_RESP_CALC   0
#   define CALC_RESP_TIME 1

/* OLTP unsolicited message routing */

#   define OLTP_ROUTE_ROUND_ROBIN 0
#   define OLTP_ROUTE_ALL_CLIENTS 1
#   define OLTP_ROUTE_NONE 2


/* Generic enabled/disabled constants */
#   define MX_DISABLED 0
#   define MX_ENABLED 1

/* Encryption constants */
#   define ENC_NOT_CONFIGURED 0
#   define ENC_CONFIGURED     1
#   define ENC_NOT_AVAILABLE  2
#   define ENC_AVAILABLE      3
#   define ENC_DISABLED       4
#   define ENC_ON_DEMAND      5
#   define ENC_ALL_MSGS       6
#   define ENC_NO_KEY         7

#   if defined(XOS_VMS)

#       pragma member_alignment save
#       pragma nomember_alignment

        struct TIMER_VALUES {
            unsigned int time1, time2;
        };

#       pragma member_alignment restore

#   endif

#   if defined(PROSYS_ENV_ALL)

#       define COMMONS_CHECKPOINTED 2

        GLOBAL ubyte_1 *inp_msg_p;
        GLOBAL ubyte_1 *out_msg_p;
        GLOBAL ubyte_1 *cntxt_p;
        GLOBAL ubyte_1  glblog[MAX_TRABUF_SIZE];             /* log record area */

#       if defined(PROSYS_ENV_V7)

            GLOBAL struct CHKPNTMEM chkpntmem[COMMONS_CHECKPOINTED];

#       else

#           include "chkp_lib.h"
            GLOBAL CHKPNT_SEGMENT chkpntmem[COMMONS_CHECKPOINTED];

#       endif

#   endif

/* General function return codes */

#   define P_FILE_NOT_FOUND -1

#   if defined(GOLS_ENV_ALL)

#       define P_SUCCESS 1
#       define P_FAILURE 0

#   endif

    struct RELEASE_RESRC_BUFFER {
        int term_count;
        unsigned long int term_ndxes[MAX_MAX_MGS_PER_PDU_VAL];
    };
/* maximum number of terminal allowed by the system                           */

    GLOBAL int max_terminal;

/* socket management information                                              */

    GLOBAL int listen_socket[MAX_INTERFACES_PER_TNI];

    GLOBAL int addr_len;                  /* size of sockaddr_in structure        */
    GLOBAL struct sockaddr_in listen_sock_addr[MAX_INTERFACES_PER_TNI];
    GLOBAL struct sockaddr_in new_sock_addr_in;
    GLOBAL struct sockaddr new_sock_addr;
    GLOBAL struct timeval listen_timeout[MAX_INTERFACES_PER_TNI];
    GLOBAL struct timeval select_timeout;
    GLOBAL struct timeb current_time;

    GLOBAL struct mbuf *writechain[MAX_CONN_PLUS_ONE];   /* chain of mbufs to be  */
                                          /* written to a host id                 */
    GLOBAL int totwritelen[MAX_CONN_PLUS_ONE];  /* running sum of bytes to be sent*/
                                          /* to a connection, used for blocking   */
    GLOBAL int tothostmsgs[MAX_CONN_PLUS_ONE];     /* running sum of number of    */
                                          /* msgs connection, used for blocking   */

    GLOBAL unsigned short proto_off;      /* global offsets into mbuf             */
    GLOBAL unsigned short version_off;    /* data area which allow us             */
    GLOBAL unsigned short pdu_len_off;    /* to access TNI fixed header           */
    GLOBAL unsigned short pdu_type_off;   /* as a string of bytes                 */

    GLOBAL unsigned char tni_proto_versions[NUMBER_OF_TNI_VERSIONS];

GLOBAL unsigned int max_pdu_size_val;


#endif                                /* TNI_GBL_LOADED                       */
