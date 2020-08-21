#ifndef RMSIO_H
#define RMSIO_H

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

#include <rms.h>

int rms_open(char* file, struct FAB *fab, struct RAB *rab);
int rms_read(struct RAB *rab,int rec, void *buffer, int size);
int rms_write(struct RAB *rab,int rec, void *buffer, int size);
int rms_close(struct FAB *fab);

#endif /* RMSIO_H */
