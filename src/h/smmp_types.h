/*
 * ===[ smmp_types.h ]=============================================
 *
 * Description:   SMMP Data Types
 *
 * -------------------------------------------------------------------
 * This item is the property of GTECH Corporation, West Greenwich,
 * Rhode Island, and contains confidential and trade secret
 * information.  It may not be transferred from the custody or control
 * of GTECH except as authorized in writing by an officer of GTECH.
 * Neither this item nor the information it contains may be used,
 * transferred, reproduced, published, or disclosed, in whole or in
 * part, and directly or indirectly, except as expressly authorized by
 * an officer of GTECH, pursuant to written agreement.
 *
 * Copyright (c) 2007 GTECH Corporation. All rights reserved.
 * -------------------------------------------------------------------
 *
 * ===================================================================
 */

#ifndef SMMP_TYPES_H
#define SMMP_TYPES_H 1

typedef unsigned char   SMMP_BOOLEAN;

typedef char            SMMP_BYTE_1;
typedef unsigned char   SMMP_UBYTE_1;

typedef short           SMMP_BYTE_2;
typedef unsigned short  SMMP_UBYTE_2;

typedef int             SMMP_BYTE_4;
typedef unsigned int    SMMP_UBYTE_4;

typedef long long       SMMP_BYTE_8;
typedef unsigned long long SMMP_UBYTE_8;

typedef unsigned char   SMMP_BINARY;
typedef unsigned char   SMMP_VARBIN;

typedef unsigned char   SMMP_ASC;
typedef unsigned char   SMMP_VARASC;

typedef unsigned char   SMMP_STR;
typedef unsigned char   SMMP_VARSTR;


typedef float           SMMP_REAL_4;
typedef double          SMMP_REAL_8;

typedef unsigned long long SMMP_DATETIME_8;
typedef unsigned char   SMMP_BIGDECIMAL;

#endif
