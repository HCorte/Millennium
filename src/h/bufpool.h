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
/*====[BUFPOOL.H]=============================================================*/
/*                                                                            */
/* Purpose: This header file contains Buffer Management - Pool Definitions    */
/*                                                                            */
/*====[BUFPOOL.H]=============================================================*/
/*                                                                            */
#ifndef BUFPOOL_H
#define BUFPOOL_H

#ifdef  GLOBAL
#undef  GLOBAL
#endif

#ifndef ALLOCATE
#define GLOBAL extern
#else
#define GLOBAL
#endif


/* Buffer Pool  */

/* MAX_BUFPOOLS is defined in gpsos.h  */
#define MAX_BUFPOOLS 5       /* define here SCD */

struct 
BUFPOOL {
   char           name[16];      /* Buffer pool name                       */

   unsigned long  nbuf;          /* Number of buffers                      */
   unsigned long  dsize;         /* Size of buffer data                    */
   unsigned long  thold;         /* Priority threshold, minimum reserve    */

   unsigned long  pool;          /* Pool number                            */
   unsigned long  bsize;         /* Size of buffer, including header       */
   
   unsigned long  thexc;         /* Priority threshold exceeded count      */
   unsigned long  nfree;         /* Current number of free buffers         */

   unsigned long  nalloc;        /* Current number of allocated buffers    */
   unsigned long  maxalloc;      /* High watermark of allocated buffers    */
   unsigned long  nallocfail;    /* Number of allocation fit-but-fails     */

   int            dmin;          /* Minimum data size satisfied            */
   int            dmax;          /* Maximum data size satisfied            */

   struct mbuf    *block;        /* Buffer memory block                    */
   struct mbuf    *free;         /* Buffer free list                       */
};


GLOBAL
struct BUFPOOL 
#ifndef ALLOCATE
        bufpool [MAX_BUFPOOLS];
#else
        bufpool [MAX_BUFPOOLS] = { { {0} } };
#endif


GLOBAL
struct BUFPOOL *
#ifndef ALLOCATE
        bufplist [MAX_BUFPOOLS + 1];
#else
        bufplist [MAX_BUFPOOLS + 1] = { 0 };
#endif


/* Function Declarations  */

extern void bufp_mem (void);
extern int  bufp_config (int pool, struct BUFPOOL *bfp);
extern void bufp_init (void);
extern void bufp_stats (int pool);

/* Original GTX code, removed when ported to the host

extern void bufp_defmask (TDESCTYP *tdesc);

*/

extern struct mbuf *__buf_alloc (int len, int priority, char *file, int line);
extern struct mbuf *__buf_allocmax (char *file, int line);
extern void __buf_free (struct mbuf *bp, char *file, int line);

#define buf_alloc(l,p)  __buf_alloc ((l), (p), __FILE__, __LINE__)
#define buf_allocmax    __buf_allocmax (__FILE__, __LINE__)
#define buf_free(b)     __buf_free ((b), __FILE__, __LINE__)

#endif  /* BUFPOOL_H  */
