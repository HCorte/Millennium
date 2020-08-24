/* ===[RMSIO.C]===================================================


   V01 13-dec-1999 uxn initial release for EuroGOLS

   --------------------------------------------------------------------
   This item is the property of GTECH Corporation, West Greewich, Rhode
   Island, and contains confidential and trade secret information. It
   may not be transferred from the custody or control of GTECH except
   as authorized in writing by an officer of GTECH. Neither this item
   nor the information it contains may be used, transferred,
   reproduced, published, or disclosed, in whole or in part, and
   directly or indirectly, except as expressly authorized by an
   officer of GTECH, pursuant to written agreement.

   Copyright 1999 GTECH Corporation. All rights reserved.
   --------------------------------------------------------------------
   ==================================================================== */


#include <string.h>
#include <starlet.h>
#include "inclib:rmsio.h"

int rms_open(char* file, struct FAB *fab, struct RAB *rab)
{
   *fab = cc$rms_fab;
   *rab = cc$rms_rab;

   fab->fab$l_fna = file;
   fab->fab$b_fns = strlen(file);
   rab->rab$l_fab = fab;

/*   rab->rab$v_rah = 1; */ /* read ahead */
   
   fab->fab$b_fac = FAB$M_GET | FAB$M_PUT | FAB$M_UPD;
   fab->fab$b_shr = FAB$M_SHRGET | FAB$M_SHRPUT | FAB$M_SHRUPD;

   fab->fab$b_fac |= FAB$M_BIO;
   fab->fab$b_shr |= FAB$M_UPI;

   if (sys$open(fab,0,0) != RMS$_NORMAL)
      return(0);

   if (sys$connect(rab,0,0) != RMS$_NORMAL)
      return(0);

   return 1;
}

int rms_read(struct RAB *rab,int rec, void *buffer, int size)
{
    rab->rab$w_usz = size;
    rab->rab$l_bkt = (rec-1)*((size+511)/512) + 2;
    rab->rab$l_ubf = buffer;

    rab->rab$l_rop &= ~RAB$M_ASY;

    return sys$read(rab,0,0);
}

int rms_write(struct RAB *rab,int rec, void *buffer, int size)
{
    rab->rab$w_rsz = size;
    rab->rab$l_bkt = (rec-1)*((size+511)/512) + 2;
    rab->rab$l_rbf = buffer;

    rab->rab$l_rop &= ~RAB$M_ASY;

    return sys$write(rab,0,0);
}

int rms_close(struct FAB *fab)
{
    return sys$close(fab);
}

