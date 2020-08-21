/*  */

/*
 * ===[includes.h]========================================================
 *
 * Description: Include files used by all source files for the ALPHAGOLS
 *              platform.
 *
 * -----------------------------------------------------------------------
 * This item is the property of GTECH Corporation, Providence,
 * Rhode Island, and contains confidential and trade secret information.
 * It may not be transfered from the custody or control of GTECH except
 * as authorized in writing by an officer of GTECH.  Neither this item
 * nor the information it contains may be used, transfered, reproduced,
 * published, or disclosed, in whole or in part, and directly or
 * indirectly, except as expressly authorized by an officer of GTECH,
 * pursuant to written agreement.
 *
 * Any and all modifications to this item must have the prior written
 * authorization of GTECH's Enterprise Series Platform Team.  GTECH shall
 * not be liable in any way for any direct or indirect damages,  whatsoever,
 * as a result of any unauthorized modifications.  The Enterprise Series
 * Platform Team reserves the right to refuse support as a result of
 * unauthorized modification.
 *
 * Copyright (c) 2005 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * =======================================================================
 */

#ifndef INCLUDES_H

#   define INCLUDES_H 1

    typedef char              byte_1;
    typedef unsigned char    ubyte_1;

    typedef short             byte_2;
    typedef unsigned short   ubyte_2;
                          
    typedef long              byte_4;
    typedef unsigned long    ubyte_4;

#   define MAX_FILE_PATH_NAME 256
#   define P_TRUE             1
#   define P_FALSE            0

#   include "mxsrv_common.h"
#   include "mxsrv_proto.h"
#   include "mxsrv_platform_proto.h"

#endif
