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
/*====[MBUF.H]================================================================*/
/*                                                                            */
/* Purpose: This header file contains Buffer Management - Definitions         */
/*                                                                            */
/*====[MBUF.H]================================================================*/
/*                                                                            */

#ifndef MBUF_H
#define MBUF_H

#define MB_ALIGN        4        /* Align pushdowns on quadword boundaries */
#define STDMBLEN        464      /* "Standard" mbuf length to allocate     */

#define BUFPRI(p)       (1 << (p))


/* Buffer Header  */

#define BUFFOOT         0x66467542     /* Buffer footprint ("BuFf")        */
#define BUFTAIL         0x62556646     /* Buffer tailprint ("FfUb")        */

#define BUFSCRUB        0x00           /* Buffer scrub byte value          */

/*
** User definable data area (UDA) stack
**
** Implemented as a stack, where a caller can "push" a value on the
** stack and later "pop" it.  A caller must make sure pushes and pops
** are balanced, to restore the stack in the mbuf to the state it was
** in when the message was received, before releasing the message to
** another task.
**
** Note that the stack grows from offset 0 upwards.
*/
#define MB_UDASTKSIZE   4              /* UDA stack size                   */

union mb_uda {                         /* User definable area (UDA) entry  */
   void                *vp;
   unsigned long        lw;
   unsigned short       sw;
   unsigned char        ch;
};


/* start mbuf modification for mx usage */
#if defined(XCC_GCC)

    XCC_ALIGN_SAVE
    XCC_ALIGN_BYTE_1

#else

#   if defined(XCC_XLC)

#       pragma options align=packed

#   elif defined(XCC_VMS) || defined(XCC_DECC)

#       pragma member_alignment save
#       pragma nomember_alignment

#   else

#       error - Compiler-specific logic not handled.

#   endif

#endif

/* end mbuf modification for mx usage */

struct mb_udastk {
   int                  off;           /* Offset to top of UDA stack       */
   union mb_uda         stk [MB_UDASTKSIZE];
};

struct mbuf {
   unsigned long        m_foot;        /* Buffer footprint                 */
                                       
   unsigned char        m_pool;        /* Buffer pool number               */
   unsigned char        m_alloc;       /* Allocated = 1; Free = 0          */
                                       
   unsigned short       m_mxlen;       /* Maximum data this mbuf can hold  */
 
   struct mbuf         *m_head;        /* Head of buffer chain             */
   struct mbuf         *m_next;        /* Next buffer in chain             */

   struct mbuf         *m_anext;       /* Links packets on queues          */
                                       
   unsigned short       m_len;         /* Amount of data in this mbuf      */
   void                *m_data;        /* Location of usable data          */

/* Original GTX code, removed when ported to the host

   TDESCTYP            *mdbg_ftask;    Last free     - task 

*/

   char                *mdbg_ffile;    /*                 file name        */
   int                  mdbg_fline;    /*                 line number      */

/* Original GTX code, removed when ported to the host

   TDESCTYP            *mdbg_atask;    Last allocate - task

*/

   char                *mdbg_afile;    /*                 file name        */
   int                  mdbg_aline;    /*                 line number      */
                                    
   struct mbuf         *m_dup;         /* Pointer to duplicated mbuf       */
   int                  m_refcnt;      /* Reference count                  */

   struct mb_udastk     m_uda;         /* User definable area (UDA) stack  */
};

/* start mbuf modification for mx usage */
#if defined(XCC_GCC)

    XCC_ALIGN_RESTORE

#else

#   if defined(XCC_XLC)

#       pragma options align=reset

#   elif defined(XCC_VMS) || defined(XCC_DECC)

#       pragma member_alignment restore

#   else

#       error - Compiler-specific logic not handled.

#   endif

#endif
/* end mbuf modification for mx usage */

/* Macro to fetch value of buffer tailprint  */

#define VBUFTAIL(bp)    (*(unsigned long *)((char *)(bp) + \
                           sizeof (struct mbuf) + (bp)->m_mxlen))


/*
** Macros for type conversion
** mtod(m,t) -    convert mbuf pointer to data pointer of correct type
*/
#define MTOD(m,t)    ((t)((m)->m_data))
#define MBFOOT(m)    ((m)->m_foot)
#define MBPOOL(m)    ((m)->m_pool)
#define MBALLOC(m)   ((m)->m_alloc)
#define MBPREV(m)    ((m)->m_prev)
#define MBNEXT(m)    ((m)->m_next)
#define MBANEXT(m)   ((m)->m_anext)
#define MBDUP(m)     ((m)->m_dup)
#define MBLEN(m)     ((m)->m_len)
#define MBMXLEN(m)   ((m)->m_mxlen)
#define MBDATA(m)    ((m)->m_data)
#define MDREFCNT(m)  ((m)->m_refcnt)


/* User definable data area (UDA) stack macros  */

#define MBUDA_CHKOFF(m)    { if (((m)->m_uda.off < 0) || \
                                 ((m)->m_uda.off >= MB_UDASTKSIZE))  { \
            logger (LOGFATAL, "Invalid UDA stack offset %d, mbuf 0x%X",  \
                     (m)->m_uda.off, (m));   \
                                 UFATAL (0);    \
                              } \
                           }

#define MBUDA_GETVP(m,v)   { MBUDA_CHKOFF (m);  \
                             (v) = (m)->m_uda.stk[(m)->m_uda.off].vp; }
#define MBUDA_SETVP(m,v)   { MBUDA_CHKOFF (m);  \
                             (m)->m_uda.stk[(m)->m_uda.off].vp = (v); }

#define MBUDA_GETLW(m,v)   { MBUDA_CHKOFF (m);  \
                             (v) = (m)->m_uda.stk[(m)->m_uda.off].lw; }
#define MBUDA_SETLW(m,v)   { MBUDA_CHKOFF (m);  \
                             (m)->m_uda.stk[(m)->m_uda.off].lw = (v); }

#define MBUDA_GETSW(m,v)   { MBUDA_CHKOFF (m);  \
                             (v) = (m)->m_uda.stk[(m)->m_uda.off].sw; }
#define MBUDA_SETSW(m,v)   { MBUDA_CHKOFF (m);  \
                             (m)->m_uda.stk[(m)->m_uda.off].sw = (v); }

#define MBUDA_GETCH(m,v)   { MBUDA_CHKOFF (m);  \
                             (v) = (m)->m_uda.stk[(m)->m_uda.off].ch; }
#define MBUDA_SETCH(m,v)   { MBUDA_CHKOFF (m);  \
                             (m)->m_uda.stk[(m)->m_uda.off].ch = (v); }

#define MBUDA_PUSH(m)      { (m)->m_uda.off++; MBUDA_CHKOFF (m); }
#define MBUDA_POP(m)       { MBUDA_CHKOFF (m); (m)->m_uda.off--; }

#define MBUDA_CLR(m)       { (m)->m_uda.off = -1; \
   memset ((m)->m_uda.stk, 0, MB_UDASTKSIZE * sizeof (union mb_uda));  }

#define MBUDA_CPY(d,s)     { (d)->off = (s)->off; \
   memcpy ((d)->stk, (s)->stk, MB_UDASTKSIZE * sizeof (union mb_uda)); }


/* The following macros require including <gxdr.h> */
#define GET32(cp)    (ntohl (*(long *)(cp)))
#define GET16(cp)    (ntohs (*(unsigned short *)(cp)))
#define PUT32(cp,x)  (void *)((long *)(cp) + 1) ;\
                        (*(long *)(cp) = htonl ((x)))
#define PUT16(cp,x)  (void *)((unsigned short *)(cp) + 1) ;\
                        (*(unsigned short *)(cp) = htonus ((x)))

#define PULLCHAR(bpp) \
 ((((bpp) != NULL) && ((*bpp) != NULL) && ((*bpp)->m_len > 1))? \
  ((*bpp)->m_len--, *((unsigned char *)(*bpp)->m_data)++) : mb_pullchar (bpp))


/* Function Declarations */

/* mballoc.c */
extern struct mbuf *__mb_alloc (unsigned short size, char *file, int line);
extern struct mbuf *__mb_alloc_wait (unsigned short size,char *file,int line);
extern struct mbuf *__mb_alloc_pri (unsigned short size, char *file,int line);
extern struct mbuf *__mb_alloc_max (char *file, int line);
extern struct mbuf *__mb_free (struct mbuf *bp, char *file, int line);

#define mb_alloc(s)           __mb_alloc ((s),__FILE__,__LINE__)
#define mb_alloc_wait(s)      __mb_alloc_wait ((s),__FILE__,__LINE__)
#define mb_alloc_pri(s)       __mb_alloc_pri ((s),__FILE__,__LINE__)
#define mb_alloc_max          __mb_alloc_max (__FILE__,__LINE__)
#define mb_free(b)            __mb_free ((b),__FILE__,__LINE__)

extern unsigned short __mb_pullup (struct mbuf **bpp, void *bufp, 
                     unsigned short cnt, char *file, int line);
extern struct mbuf   *__mb_pushdown (struct mbuf *bp, 
                     unsigned short size, char *file, int line);
extern struct mbuf   *__mb_pushcpy (register struct mbuf *bp, 
                     unsigned short size, void *bufp, char *file, int line);

#define mb_pullup(b,p,c)      __mb_pullup ((b),(p),(c),__FILE__,__LINE__)
#define mb_pushdown(b,s)      __mb_pushdown ((b),(s),__FILE__,__LINE__)
#define mb_pushcpy(b,s,p)     __mb_pushcpy ((b),(s),(p),__FILE__,__LINE__)

extern int    __mb_sanitychk (struct mbuf *bp);
extern int    __mb_sanitychk_p (struct mbuf *bp);

#define mb_sanitychk(b)       __mb_sanitychk (b)
#define mb_sanitychk_p(b)     __mb_sanitychk_p (b)

extern void   __mb_assert (struct mbuf *bp, char *file, int line);
extern void   __mb_assert_p (struct mbuf *bp, char *file, int line);

#define mb_assert(b)          __mb_assert ((b),__FILE__,__LINE__)
#define mb_assert_p(b)        __mb_assert_p ((b),__FILE__,__LINE__)

/* mbpacket.c */
extern struct mbuf *__mb_free_p (struct mbuf *bp, char *file, int line);
extern void __mb_append (struct mbuf **bpp, struct mbuf *bp, 
                     char *file, int line);
extern void __mb_crunch (struct mbuf **bpp, char *file, int line);
extern struct mbuf *__mb_gather (struct mbuf *bp, char *file, int line);
extern void __mb_pad_p (struct mbuf **bpp, unsigned short len, char c, 
                     char *file, int line);
extern unsigned short __mb_len_p (struct mbuf *bp, char *file, int line);
extern struct mbuf *__mb_copy_p (struct mbuf *bp, unsigned short cnt, 
                     char *file, int line);
extern unsigned short __mb_dup_p (struct mbuf **bpp, struct mbuf *bp, 
                     unsigned short offset, unsigned short cnt, 
                     char *file, int line);

#define mb_free_p(b)          __mb_free_p ((b),__FILE__,__LINE__)
#define mb_append(b,p)        __mb_append ((b),(p),__FILE__,__LINE__)
#define mb_crunch(b)          __mb_crunch ((b),__FILE__,__LINE__)
#define mb_gather(b)          __mb_gather ((b),__FILE__,__LINE__)
#define mb_pad_p(b,l,c)       __mb_pad_p ((b),(l),(c),__FILE__,__LINE__)
#define mb_len_p(b)           __mb_len_p ((b),__FILE__,__LINE__)
#define mb_copy_p(b,c)        __mb_copy_p ((b),(c),__FILE__,__LINE__) 
#define mb_dup_p(b,p,o,c)     __mb_dup_p ((b),(p),(o),(c),__FILE__,__LINE__)

/* mbqueue.c */
extern void __mb_free_q (struct mbuf **q, char *file, int line);
extern unsigned short __mb_len_q (struct mbuf *bp);
extern void __mb_enqueue (struct mbuf **q, struct mbuf *bp);
extern struct mbuf *__mb_dequeue (struct mbuf **q);
extern struct mbuf *__mb_qdata (char *data, unsigned short cnt, 
                     char *file, int line);
extern unsigned short __mb_dqdata (struct mbuf *bp, char *buf, 
                     unsigned short cnt, char *file, int line);

#define mb_free_q(q)          __mb_free_q ((q),__FILE__,__LINE__)
#define mb_len_q              __mb_len_q
#define mb_enqueue            __mb_enqueue
#define mb_dequeue            __mb_dequeue
#define mb_qdata(d,c)         __mb_qdata ((d),(c),__FILE__,__LINE__)
#define mb_dqdata(b,f,c)      __mb_dqdata ((b),(f),(c),__FILE__,__LINE__)

/* mbuf.c */
extern void __mb_trim (struct mbuf **bpp, unsigned short length, 
                     char *file, int line);
extern int  __mb_pullchar (struct mbuf **bpp, char *file, int line);
extern long __mb_pull16 (struct mbuf **bpp, char *file, int line);
extern long __mb_pull32 (struct mbuf **bpp, char *file, int line);
extern unsigned short __mb_get16 (char *cp);
extern long __mb_get32 (char *cp);
extern char *__mb_put16 (char *cp, unsigned short x);
extern char *__mb_put32 (char *cp, long x);
extern void __mb_dump (unsigned char sev, char *str, struct mbuf *bp);
extern void __mb_hexdump (unsigned char sev,
                     char *str, struct mbuf *pbuf, char *file, int line);
extern char __mb_getchar (struct mbuf *bp, unsigned int index);
extern void __mb_putchar (struct mbuf *bp, char ch, unsigned int index);

#define mb_trim(b,l)          __mb_trim ((b),(l),__FILE__,__LINE__)
#define mb_pullchar(b)        __mb_pullchar ((b),__FILE__,__LINE__)
#define mb_pull16(b)          __mb_pull16 ((b),__FILE__,__LINE__)
#define mb_pull32(b)          __mb_pull32 ((b),__FILE__,__LINE__)
#define mb_get16              __mb_get16
#define mb_get32              __mb_get32
#define mb_put16              __mb_put16
#define mb_put32              __mb_put32
#define mb_dump               __mb_dump
#define mb_hexdump(v,s,p)     __mb_hexdump ((v),(s),(p),__FILE__,__LINE__)
#define mb_getchar            __mb_getchar
#define mb_putchar            __mb_putchar

#endif  /* MBUF_H  */
