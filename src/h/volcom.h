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
/*====[VOLCOM.H]==============================================================*/
/*                                                                            */
/* Purpose: This header file contains all the volatile common definitions.    */
/*                                                                            */
/*====[VOLCOM.H]==============================================================*/
/*                                                                            */

#ifndef VOLCOM_H
#define VOLCOM_H


#define MX_PROD_SNAME       16
#define MAX_FILE_PATH_NAME 256

typedef struct                                  /* product control area     */
{
    char short_name[MX_PROD_SNAME];             /* product short name       */
    int number;                                 /* product number           */
    unsigned short cdc;                         /* product cdc              */
    unsigned long serial;

    enum PROD_STATE
    {
        DEAD = 0,                               /* not initialized          */
        REGISTERED,                             /* registered               */
        REPROCESSING,                           /* reprocessing             */
        PTNILIVE,                               /* live                     */
        DAYENDING,                              /* dayending                */
        DAYENDED,                               /* dayended                 */
        STOPPING,                               /* stopping                 */
        DEREGISTERED,                           /* deregistered             */
        STOPPED                                 /* stopped                  */
    } state;                                    /* product status           */

    int prime_id;                               /* last received prime_id # */
    int back_id;                                /* last received back_id #  */

    TIMEDELAYS timedelays;
} PROD;

#define MAX_PROD_NUM         256
#define MAX_STRENC_PARM_LEN  32
#define DEF_MAX_RPC_EVENTS   100
#define DEF_MAX_MAP_MSG_SIZE 32768
#define DEF_RPC_TIMEOUT_INVL 60

typedef struct MXGATE_PARAMS
{
    int  mxgate_active;
    int  max_map_msg_size;
    
    int  trace_startup;
    int  trace_events;
    int  trace_rpcs;
    int  trace_rpc_hdr;
    int  trace_olpm_msgs;
    int  trace_smmp_msgs;

    int  rpc_timeout_invl;
    int  max_events;

    int  ignore_gtms;     

    char host_strenc_method[MAX_STRENC_PARM_LEN];
    char smmp_strenc_method[MAX_STRENC_PARM_LEN];

    char rpc_files_path[MAX_FILE_PATH_NAME];
    char passthru_names_file[MAX_FILE_PATH_NAME];

    char send_to_msgsrv[MAX_PROD_NUM];
} MXGATE_PARAMS;

typedef struct                                  /* system data              */
{
    int gtms_idn;                               /* gtms idn - from reg      */
    int dayend_time;                            /* dayend time              */
    int multiday_operations;                    /* multi day operations     */
    int mxtrm;                                  /* max terminal number      */
    int prod_meslen;                            /* max prod message size    */
    int term_meslen;                            /* max term message size    */
    int spare[20];                              /* filler */
} SYS;

typedef struct
{
    VOLATILE_COMMON_STANDARD_HEADER hdr;
    int prodbuf_size;
    PROD prod;                                  /* product control data     */
    SYS  sys;                                   /* system data              */
    MXGATE_PARAMS gateparams;
}VOLCOM;

#define SIZEOF_VOLCOM sizeof(VOLCOM)

#endif
