/*
 * ===[ mbuf_includes.h ]=============================================
 *
 * Description:   mbuf include file
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


#ifndef MBUF_INCLUDES_H

#   define MBUF_INCLUDES_H

#   include <netinet/in.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <string.h>

#   if defined(PROSYS_ENV_PLATFORM)

#       include "g_macros.h"

#   endif

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#       include "mbuf.h"

#   elif defined(XOS_VMS)

#       include "mbuf.h"

#   else

#       error - OS-specific logic not handled.

#   endif

#   define _MAX(a,b) ((a) > (b)) ? a : b
#   define _MIN(a,b) ((a) < (b)) ? a : b

#   define NULLBUF (struct mbuf *)0
#   define NULLBUFP (struct mbuf **)0

    int
    disable_int(void);

    void
    tm_wkafter(int value);

    void
    UFATAL(int value);

    void
    FATAL(int value);

    int
    restore_int(void);

#endif

