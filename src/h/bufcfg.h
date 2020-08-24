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
/*====[BUFCFG.H]==============================================================*/
/*                                                                            */
/* Purpose: This header file contains Buffer Pool Configuration Defaults      */
/*                                                                            */
/*====[BUFCFG.H]==============================================================*/
/*                                                                            */
#ifndef BUFCFG_H
#   define BUFCFG_H

#   ifdef  GLOBAL
#       undef  GLOBAL
#   endif

#   ifndef ALLOCATE
#       define GLOBAL extern
#   else
#       define GLOBAL
#   endif


/* MAX_BUFPOOLS is defined in gpsos.h  */

/* Default Buffer Pools - Override via BUFPOOL command - Note, these are for TNI and are NOT the same as SMMP pools */

#   define POOL1_BUF_POOL          0
#   define DEF_POOL1_BUF_SIZE      0
#   define DEF_POOL1_BUF_NUM       256
#   define DEF_POOL1_BUF_THOLD     32

#   define POOL2_BUF_POOL          1
#   define DEF_POOL2_BUF_SIZE      100
#   define DEF_POOL2_BUF_NUM       1024
#   define DEF_POOL2_BUF_THOLD     2*MAX_CONN

#   define POOL3_BUF_POOL          2
#   define DEF_POOL3_BUF_SIZE      256
#   define DEF_POOL3_BUF_NUM       1024
#   define DEF_POOL3_BUF_THOLD     32

#   define POOL4_BUF_POOL          3
#   define DEF_POOL4_BUF_SIZE      1500
#   define DEF_POOL4_BUF_NUM       256
#   define DEF_POOL4_BUF_THOLD     32

#   define POOL5_BUF_POOL          4
#   define DEF_POOL5_BUF_SIZE      16000
#   define DEF_POOL5_BUF_NUM       (MAX_CONN/2)*2
#   define DEF_POOL5_BUF_THOLD     (MAX_CONN/2)


    GLOBAL int pool1_buf_size;
    GLOBAL int pool1_buf_num;
    GLOBAL int pool1_buf_thold;

    GLOBAL int pool2_buf_size; 
    GLOBAL int pool2_buf_num; 
    GLOBAL int pool2_buf_thold; 

    GLOBAL int pool3_buf_size; 
    GLOBAL int pool3_buf_num; 
    GLOBAL int pool3_buf_thold; 

    GLOBAL int pool4_buf_size; 
    GLOBAL int pool4_buf_num; 
    GLOBAL int pool4_buf_thold; 

    GLOBAL int pool5_buf_size; 
    GLOBAL int pool5_buf_num; 
    GLOBAL int pool5_buf_thold; 

#   if defined(PROSYS_ENV_PLATFORM)

        XCC_ALIGN_SAVE
        XCC_ALIGN_BYTE_1

#   else

#       if defined(XCC_XLC)

#           pragma options align=packed

#       elif defined(XCC_VMS) || defined(XCC_DECC)

#           pragma member_alignment save
#           pragma nomember_alignment

#       else

#           error - Compiler-specific logic not handled.

#       endif

#   endif

    struct
    DFBUFPCFG {
       char             *tname;      /* Task name                              */
       unsigned short    bpmask;     /* Buffer pool priority mask              */
    };

#   if defined(PROSYS_ENV_PLATFORM)

        XCC_ALIGN_RESTORE

#   else

#       if defined(XCC_XLC)

#           pragma options align=reset

#       elif defined(XCC_VMS) || defined(XCC_DECC)

#           pragma member_alignment restore

#       else

#           error - Compiler-specific logic not handled.

#       endif

#   endif

/* X2X Primary Buffer Pools  */

    GLOBAL struct BUFPOOL pool_1;
/* Small Message Buffer Pool (like Keepalive PDUs)  */

    GLOBAL struct BUFPOOL pool_2;

/* Single Terminal Message Buffer Pool  */

    GLOBAL struct BUFPOOL pool_3;

/* Standard Ethernet Buffer Pool  */

    GLOBAL struct BUFPOOL pool_4;

/* Ethernet Buffer Pool for Worst Case large split PDUs */

    GLOBAL struct BUFPOOL pool_5;

#endif
