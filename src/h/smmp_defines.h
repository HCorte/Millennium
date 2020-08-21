/*
 * ===[ smmp_defines.h ]=============================================
 *
 * Description:   SMMP constant definitions 
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


#ifndef SMMP_DEFINES_H
#define SMMP_DEFINES_H  1


#define SMMP_TRUE   1
#define SMMP_FALSE  0

/*
 * Odd (success) return status codes...
 */
#define SMMP_SUCCESS    1
#define SMMP_ATEND      3

/*
 * Even (failure) return status codes...
 */
#define SMMP_FAILURE    0


/* Map Attributes */
#define SMMP_MAP_ELEMENTS   0x01
#define SMMP_MAP_INSTANCES  0x02
#define SMMP_MAP_TYPE       0x03
#define SMMP_MAP_ID         0x04
#define SMMP_MAP_ID_VER     0x05

/* Value Attributes */
#define SMMP_VAL_DATATYPE   0x01
#define SMMP_VAL_LENGTH     0x02
#define SMMP_VAL_NAME       0x03

/* Map Formats */
#define SMMP_MAP_STANDARD   0x01
#define SMMP_MAP_COMPACT    0x02

/* meta data record types */
#define NO_META_MAP_RCD         0x00
#define META_STANDARD_MAP_RCD   0x01

/* String encode methods */
#define SMMP_STRENC_ASCII       0x01
#define SMMP_STRENC_UTF_8       0x02
#define SMMP_STRENC_ISO_8859_1  0x03

/* Error codes */
#define SMMP_EC_INV_PDU    0x01

/* Error Pdu definitions */
#define SMMP_ERRPDU_IDENTIFIER  "Error Report"
#define SMMP_ERRPDU_ERRCODE     "Error Code"
#define SMMP_ERRPDU_ERRDESC     "Error Description"


/* SMMP data type codes */
#define SMMP_DTC_BYTE_1     0x01
#define SMMP_DTC_UBYTE_1    0x02
#define SMMP_DTC_BYTE_2     0x03
#define SMMP_DTC_UBYTE_2    0x04
#define SMMP_DTC_BYTE_4     0x05
#define SMMP_DTC_UBYTE_4    0x06
#define SMMP_DTC_BYTE_8     0x07
#define SMMP_DTC_UBYTE_8    0x08
#define SMMP_DTC_ASC        0x09
#define SMMP_DTC_VARASC     0x0A
#define SMMP_DTC_STR        0x0B
#define SMMP_DTC_VARSTR     0x0C
#define SMMP_DTC_BINARY     0x0D
#define SMMP_DTC_VARBIN     0x0E
#define SMMP_DTC_REAL_8     0x0F
#define SMMP_DTC_REAL_4     0x10
#define SMMP_DTC_DATETIME_8 0x11
#define SMMP_DTC_BIGDECIMAL 0x12
#define SMMP_DTC_NULL       0x13
#define SMMP_DTC_BOOLEAN    0x14

/*
 * SMMP BUF POOLS
 */

#define SMMP_POOL1_BUF_POOL         0x00
#define SMMP_POOL2_BUF_POOL         0x01
#define SMMP_POOL3_BUF_POOL         0x02
#define SMMP_POOL4_BUF_POOL         0x03
#define SMMP_POOL5_BUF_POOL         0x04

#define SMMP_POOL1_POOL_NAME        "Pool1"
#define SMMP_DEF_POOL1_BUF_SIZE     32
#define SMMP_DEF_POOL1_BUF_NUM      1000
#define SMMP_DEF_POOL1_BUF_THOLD    0

#define SMMP_POOL2_POOL_NAME        "Pool2"
#define SMMP_DEF_POOL2_BUF_SIZE     64
#define SMMP_DEF_POOL2_BUF_NUM      400
#define SMMP_DEF_POOL2_BUF_THOLD    0

#define SMMP_POOL3_POOL_NAME        "Pool3"
#define SMMP_DEF_POOL3_BUF_SIZE     256
#define SMMP_DEF_POOL3_BUF_NUM      200
#define SMMP_DEF_POOL3_BUF_THOLD    0

#define SMMP_POOL4_POOL_NAME        "Pool4"
#define SMMP_DEF_POOL4_BUF_SIZE     16*1024
#define SMMP_DEF_POOL4_BUF_NUM      4
#define SMMP_DEF_POOL4_BUF_THOLD    0

#define SMMP_POOL5_POOL_NAME        "Pool5"
#define SMMP_DEF_POOL5_BUF_SIZE     32*1024
#define SMMP_DEF_POOL5_BUF_NUM      4
#define SMMP_DEF_POOL5_BUF_THOLD    2

#endif
