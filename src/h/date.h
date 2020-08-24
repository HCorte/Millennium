#ifndef _DATE_H
#define _DATE_H
/*
 * V01 23-MAY-2000 UXN Initial release.
 *
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * This item is the property of GTECH Corporation, Providence, Rhode
 * Island, and contains confidential and trade secret information. It
 * may not be transferred from the custody or control of GTECH except
 * as authorized in writing by an officer of GTECH. Neither this item
 * nor the information it contains may be used, transferred,
 * reproduced, published, or disclosed, in whole or in part, and
 * directly or indirectly, except as expressly authorized by an
 * officer of GTECH, pursuant to written agreement.
 *
 * Copyright 2000 GTECH Corporation. All rights reserved.
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

#pragma member_alignment save
#pragma nomember_alignment
struct DATE {
   short month;	
   short day;
   short year;	        /* year in 2 digits */
   short julian;
   short cdc;
   short day_of_week;
   char  long_name[14]; /* 'Tu 23.05.2000 ' */
   short year4;         /* year in 4 digits */
};    
#pragma member_alignment restore

/* VCALEN subroutine prototypes */
extern void LCDATE(struct DATE*);
extern void LBDATE(struct DATE*);
#endif /* _DATE_H */
