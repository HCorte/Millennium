static const char *fileid = "";

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
/* Copyright 2005 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[MBALLOC.C]=============================================================*/
/*                                                                            */
/* Purpose: Buffer Management - Allocation/Deallocation Functions             */
/*                                                                            */
/* Functions:                                                                 */
/*          __mb_assert ()                                                    */
/*          __mb_assert_p ()                                                  */
/*          __mb_sanitychk ()                                                 */
/*          __mb_sanitychk_p ()                                               */
/*          __mb_alloc ()                                                     */
/*          __mb_alloc_wait ()                                                */
/*          __mb_alloc_pri ()                                                 */
/*          __mb_alloc_max ()                                                 */
/*          __mb_free ()                                                      */
/*          __mb_pullup ()                                                    */
/*          __mb_pushdown ()                                                  */
/*          __mb_pushcpy ()                                                   */
/*                                                                            */
/*====[MBALLOC.C]=============================================================*/
/*                                                                            */

#include "mbuf_includes.h"

#include <string.h>
#include <ctype.h>


#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)
#   include "gdata.h"
#   include "bufpool.h"

#   include "logger.h"

#elif defined(XOS_VMS)

#   include "gdata.h"
#   include "bufpool.h"

#   include "logger.h"

#else

#   error - OS-specific logic not handled.

#endif

static struct mbuf *local_mb_alloc (unsigned short size, 
               int mode, char *file, int line);
static void local_mb_init (struct mbuf *bp);


/* ---[ __mb_sanitychk ]--------------------------------------------------

   Summary:

      __mb_sanitychk (struct mbuf *bp)
      __mb_assert (struct mbuf *bp)

   Description:

      Verify buffer internal integrity

   Input:

      bp             Pointer to buffer

   Output:

      None

   Return Value:  

      int
         __mb_sanitychk() returns -1 if buffer check fails, 0 if OK.
         __mb_assert() causes a fatal error if the buffer check fails.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
int
__mb_sanitychk (struct mbuf *bp)
{
   int pool, doff;
   struct mbuf *bpp;

   /* Verify mbuf and duplicated mbuf link  */
   for (bpp = bp; bpp != NULL; bpp = bpp->m_dup) {
      /* Inlined sanity check  */
      if (bpp == NULL) {
         logger (LOGFATAL, "mb_sanitychk: Null buffer pointer");
         return (-1);
      }

      /* Doesn't smell like a buffer  */
      else if (bpp->m_foot != BUFFOOT) {
         logger (LOGFATAL, 
            "mb_sanitychk: Invalid buffer footprint, bufp 0x%X (0x%X)", 
                  bpp, bp);

         return (-1);
      }

      /* Verify buffer tailprint, to see if we've overrun the buffer  */
      else if (VBUFTAIL (bpp) != BUFTAIL) {
         logger (LOGFATAL, 
            "mb_sanitychk: Invalid buffer tailprint, pool %d, bufp 0x%X (0x%X)\n",
                  bpp->m_pool, bpp, bp);

/* Code modification for host port */

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s", 
                 bpp->mdbg_afile, bpp->mdbg_aline);

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s",
	         bpp->mdbg_ffile, bpp->mdbg_fline);
	 
/* Original GTX code, removed when ported to the host

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s",
                 bpp->mdbg_afile, bpp->mdbg_aline,
		 (bpp->mdbg_atask)? bpp->mdbg_atask->tkey : "????");

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
                 bpp->mdbg_ffile, bpp->mdbg_fline,
                 (bpp->mdbg_ftask)? bpp->mdbg_ftask->tkey : "????");
*/

         return (-1);
      }

      /* Verify buffer is from a valid pool  */
      else if ((pool = bpp->m_pool) >= MAX_BUFPOOLS) {
         logger (LOGFATAL, 
            "mb_sanitychk: Invalid pool, pool %d, bufp 0x%X (0x%X)", 
                  pool, bpp, bp);

/* Code modification for host port */

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s", 
                 bpp->mdbg_afile, bpp->mdbg_aline);

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
	         bpp->mdbg_ffile, bpp->mdbg_fline);

/* Original GTX code, removed when ported to the host

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s",
                 bpp->mdbg_afile, bpp->mdbg_aline,
                 (bpp->mdbg_atask)? bpp->mdbg_atask->tkey : "????");

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
                 bpp->mdbg_ffile, bpp->mdbg_fline,
                 (bpp->mdbg_ftask)? bpp->mdbg_ftask->tkey : "????");
*/

         return (-1);
      }

      /* Verify buffer was properly allocated  */
      else if (bpp->m_alloc == 0) {
         logger (LOGFATAL, 
            "mb_sanitychk: Unallocated buffer, pool %d, bufp 0x%X (0x%X)",
                  pool, bpp, bp);

/* Code modification for host port */

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s", 
                 bpp->mdbg_afile, bpp->mdbg_aline);

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
	         bpp->mdbg_ffile, bpp->mdbg_fline);

/* Original GTX code, removed when ported to the host

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s",
                 bpp->mdbg_afile, bpp->mdbg_aline,
                 (bpp->mdbg_atask)? bpp->mdbg_atask->tkey : "????");

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
                 bpp->mdbg_ffile, bpp->mdbg_fline,
                 (bpp->mdbg_ftask)? bpp->mdbg_ftask->tkey : "????");
*/

         return (-1);
      }

      /* Verify buffer data pointer is sane  */
      else if ((doff = 
               (int)((char *)bpp->m_data - (char *)(bpp+1))) > bpp->m_mxlen) {

         logger (LOGFATAL, 
            "mb_sanitychk: Invalid buffer data pointer, pool %d, bufp 0x%X (0x%X), data 0x%X",
                  pool, bpp, bp, bpp->m_data);

/* Code modification for host port */

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s", 
                 bpp->mdbg_afile, bpp->mdbg_aline);

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
	         bpp->mdbg_ffile, bpp->mdbg_fline);

/* Original GTX code, removed when ported to the host

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s",
                 bpp->mdbg_afile, bpp->mdbg_aline,
                 (bpp->mdbg_atask)? bpp->mdbg_atask->tkey : "????");

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
                 bpp->mdbg_ffile, bpp->mdbg_fline,
                 (bpp->mdbg_ftask)? bpp->mdbg_ftask->tkey : "????");
*/

         return (-1);
      }

      /* Verify buffer data length  */
      else if ((doff + bpp->m_len) > bpp->m_mxlen) {

         logger (LOGFATAL, 
            "mb_sanitychk: Invalid buffer data length, pool %d, bufp 0x%X (0x%X), data 0x%X, len %d",
                  pool, bpp, bp, bpp->m_data, bpp->m_len);

/* Code modification for host port */

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s", 
                 bpp->mdbg_afile, bpp->mdbg_aline);

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s",
		 bpp->mdbg_ffile, bpp->mdbg_fline);

/* Original GTX code, removed when ported to the host

         logger (LOGFATAL, "Last alloc: module %s, line %d, task %s",
                 bpp->mdbg_afile, bpp->mdbg_aline,
                 (bpp->mdbg_atask)? bpp->mdbg_atask->tkey : "????");

         logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
                 bpp->mdbg_ffile, bpp->mdbg_fline,
                 (bpp->mdbg_ftask)? bpp->mdbg_ftask->tkey : "????");
*/

         return (-1);
      }
   }

   return (0);
}

void
__mb_assert (struct mbuf *bp, char *file, int line)
{
   if (__mb_sanitychk (bp) < 0) {
      logger (LOGFATAL, "mb_assert - Caller: module %s, line %d", file, line);
      UFATAL (0);
   }
}


/* ---[ __mb_sanitychk_p ]------------------------------------------------

   Summary:

      __mb_sanitychk_p (struct mbuf *bp)
      __mb_assert_p (struct mbuf *bp)

   Description:

      Verify buffer chain internal integrity

   Input:

      bp             Pointer to buffer chain

   Output:

      None

   Return Value:  

      int
         __mb_sanitychk_p() returns -1 if buffer check fails, 0 if OK.
         __mb_assert_p() causes a fatal error if the buffer check fails.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
int
__mb_sanitychk_p (struct mbuf *bp)
{
   int i, ret;
   register struct mbuf *abp;

   if (bp == NULL) {
      logger (LOGFATAL, "mb_sanitychk_p: Null buffer pointer");
      return (-1);
   }

   /*
   ** i     Used to determine failed buffer index from pROBE
   ** abp   Used to determine buffer head from pROBE
   */
   for (i = 0, abp = bp; abp != NULL; i++, abp = abp->m_next) {
      if ((ret = __mb_sanitychk (abp)) < 0)
         return (ret);
   }

   return (0);
}

void
__mb_assert_p (struct mbuf *bp, char *file, int line)
{
   if (__mb_sanitychk_p (bp) < 0) {
      logger (LOGFATAL, "mb_assert_p - Caller: module %s, line %d", 
               file, line);

      UFATAL (0);
   }
}



/* Original GTX code, removed when ported to the host

    INLINE

*/

static struct mbuf *
local_mb_alloc (register unsigned short size, int mode, char *file, int line)
{
   register struct mbuf *bp;

   if ((bp = __buf_alloc (size, mode, file, line)) == NULL)
      return (NULL);

   local_mb_init (bp);

   if (GDEBUG (DBGBANAL))
      __mb_assert (bp, file, line);

   return (bp);
}



/* Original GTX code, removed when ported to the host

    INLINE

*/

static void
local_mb_init (register struct mbuf *bp)
{
   if (bp == NULL)
      return;

   bp->m_head = NULL;
   bp->m_next = NULL;

   bp->m_anext = NULL;

   bp->m_len = 0;
   bp->m_data = (char *)bp + sizeof (struct mbuf);

   bp->m_dup = NULL;
   bp->m_refcnt = 1;

   /* Initialise UDA stack  */
   MBUDA_CLR (bp);

   /* Initialize data part */
   memset(bp->m_data,0x00,bp->m_mxlen);
}


/* ---[ __mb_alloc ]------------------------------------------------------

   Summary:

      mb_alloc (unsigned short size)

   Description:

      Get buffer of specified size from free queue

   Input:

      size           Requested buffer length

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to allocated buffer, or NULL if failure.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_alloc (register unsigned short size, char *file, int line)
{
   return (local_mb_alloc (size, 0, file, line));
}


/* ---[ __mb_alloc_wait ]-------------------------------------------------

   Summary:

      mb_alloc_wait (unsigned short size)

   Description:

      Get buffer of specified size from free queue

   Input:

      size           Requested buffer length

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to allocated buffer

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_alloc_wait (register unsigned short size, char *file, int line)
{
   register struct mbuf *bp;

   /* Allocate an mbuf, retry in one tick if failure */
   while ((bp = local_mb_alloc (size, 0, file, line)) == NULL)
      tm_wkafter (1);

   return (bp);
}


/* ---[ __mb_alloc_pri ]--------------------------------------------------

   Summary:

      mb_alloc_pri (unsigned short size)

   Description:

      Get buffer of specified size from free queue with priority privilege

   Input:

      size           Requested buffer length

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to allocated buffer, or NULL if failure.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_alloc_pri (register unsigned short size, char *file, int line)
{
   return (local_mb_alloc (size, 1, file, line));
}


/* ---[ __mb_alloc_max ]--------------------------------------------------

   Summary:

      mb_alloc_max (void)
      
   Description:

      Get maximum size buffer available

   Input:

      None

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to allocated buffer, or NULL if failure.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_alloc_max (char *file, int line)
{
   register struct mbuf *bp;

   if ((bp = __buf_allocmax (file, line)) == NULL)
      return (NULL);

   local_mb_init (bp);

   if (GDEBUG (DBGBANAL))
      __mb_assert (bp, file, line);

   return (bp);
}


/* ---[ __mb_free ]-------------------------------------------------------

   Summary:

      mb_free (register struct mbuf *bp)

   Description:

      Free buffer

      Decrements the reference pointer in a buffer.  If it goes to zero,
      all resources associated with the buffer are freed.

      Note that this only frees the given buffer, NOT the entire chain.

      USE THIS FUNCTION WITH CARE TO AVOID ANY ACCIDENTAL FREEING OF
      AN INDIVIDUAL BUFFER, INSTEAD OF THE WHOLE BUFFER CHAIN!

   Input:

      bp             Pointer to buffer

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to next buffer in chain, or NULL if the entire 
         chain has been freed.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_free (register struct mbuf *bp, char *file, int line)
{
   struct mbuf *bpnext;

   if (bp == NULL)
      return (NULL);

   bpnext = bp->m_next;

   /* Follow indirection  */
   if (bp->m_dup != NULL) {
      __mb_free (bp->m_dup, file, line);

      bp->m_dup = NULL;
   }

   /* Decrement reference count. If it has gone to zero, free it. */
   if (--bp->m_refcnt <= 0) {
      if (GDEBUG (DBGBANAL))
         __mb_assert (bp, file, line);

      __buf_free (bp, file, line);
   }

   /* Update head pointer of remaining elements in chain  */
   for (bp = bpnext; bp != NULL; bp = bp->m_next)
      bp->m_head = bpnext;

   return (bpnext);
}


/* ---[ __mb_pullup ]-----------------------------------------------------

   Summary:

      mb_pullup (struct mbuf **bpp, void *bufp, unsigned short cnt)

   Description:

      Copy and delete 'cnt' bytes from beginning of buffer chain

   Input:

      bpp            Pointer to pointer to buffer chain

      bufp           Buffer to contain pulled off data

      cnt            Number of bytes to pull off

   Output:

      bpp            Pointer to new first buffer in chain (ie. if some 
                     leading buffers in the chain were pulled off 
                     entirely, the first buffer in the chain will
                     have changed).

   Return Value:  

      unsigned short
         Returns number of bytes actually pulled off

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
unsigned short
__mb_pullup (struct mbuf **bpp, void *bufp, unsigned short cnt,
               char *file, int line)
{
   register struct mbuf *bp;
   unsigned short n, tot;
   struct mb_udastk savuda;

   tot = 0;

   if (bpp == NULL)
      return (0);

   if (GDEBUG (DBGBANAL) && *bpp)
      __mb_assert_p (*bpp, file, line);

   while ((cnt != 0) && ((bp = *bpp) != NULL)) {
      n = _MIN (cnt, bp->m_len);

      if (bufp != NULL) {
         /* Common case optimization */
         if (n == 1)
            *(char *)bufp = *(char *)bp->m_data;

         else if (n > 1)
            memcpy (bufp, bp->m_data, n);

         bufp = (char *)bufp + n;
      }

      tot += n;
      cnt -= n;
#     if 0 /* this would not compile */
          (char *)bp->m_data += n;
#     endif
      bp->m_data = (void *)((unsigned long) bp->m_data
                          + (unsigned long) n);

      bp->m_len -= n;      

      if (bp->m_len == 0) {
         /* 
          * If this is the last mbuf of a packet but there are others on 
          * the queue, return a pointer to the next on the queue.  This 
          * allows pullups to work on a packet queue
          */
         MBUDA_CPY (&savuda, &bp->m_uda);

         if ((bp->m_next == NULL) && (bp->m_anext != NULL)) {
            *bpp = bp->m_anext;
            MBUDA_CPY (&(*bpp)->m_uda, &savuda);

            __mb_free (bp, file, line);
         } 
         
         else {
            *bpp = __mb_free (bp, file, line);

            if (*bpp != NULL)
               MBUDA_CPY (&(*bpp)->m_uda, &savuda);
         }
      }
   }

   /* Update head pointer of elements in chain  */
   if (*bpp) {
      for (bp = *bpp; bp != NULL; bp = bp->m_next)
         bp->m_head = *bpp;

      if (GDEBUG (DBGBANAL))
         __mb_assert_p (*bpp, file, line);
   }

   return (tot);
}


/* ---[ __mb_pushdown ]---------------------------------------------------

   Summary:

      mb_pushdown (register struct mbuf *bp, unsigned short size)

   Description:

      Inserts specified amount of contiguous new space at the beginning 
      of a buffer chain.  If enough space is available in the first 
      buffer, no new space is allocated.  Otherwise, a buffer of the 
      appropriate size is allocated and tacked on the front of the chain.

      This operation is the logical inverse of pullup (), hence the name.

   Input:

      bp             Pointer to buffer chain

      size           Number of bytes to allocate at the beginning
                     of the chain

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to new first buffer in the chain, if a new
         buffer was needed, or NULL if failure.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_pushdown (register struct mbuf *bp, unsigned short size,
               char *file, int line)
{
char *val1;
char *val2;
   register struct mbuf *nbp;

   if (GDEBUG (DBGBANAL) && bp)
      __mb_assert_p (bp, file, line);

   /* 
    * Check that bp is real, that it hasn't been duplicated, and
    * that it itself isn't a duplicate before checking to see if
    * there's enough space at its front.
    */
   if ((bp != NULL) && (bp->m_refcnt == 1) && (bp->m_dup == NULL)) {
      /* If buffer is empty, use this one if there's room  */
      if ((bp->m_len == 0) && (bp->m_mxlen >= size)) {
         bp->m_len += size;
         return (bp);
      }

      /* Just adjust this one, if there's room  */
      else if (((char *)bp->m_data - (char *)(bp+1)) >= size) {
         val1 = (char *)bp->m_data;
         val2 =  (char *)(bp+1);
         printf ("mb_pushdown, (char *)bp->m_data) = %p \n", val1);
         printf ("mb_pushdown, (char *)(bp+1)) %p \n" , val2);
#        if 0
             (char *)bp->m_data -= size;
#        endif
         bp->m_data = (void *)((unsigned long) bp->m_data
                          - (unsigned long) size);
         bp->m_len += size;

         return (bp);
      }
   }

   /* Insufficient room - Allocate a new buffer on the head  */
   if ((nbp = __mb_alloc_pri (size, file, line)) == NULL)
      return (NULL);

   nbp->m_next = bp;
   nbp->m_len  = size;

   if (bp != NULL)
   MBUDA_CPY (&nbp->m_uda, &bp->m_uda);

   /*
   ** Put data as far down the buffer as possible while retaining alignment
   */

#  if 0 /* this would not compile */
       (char *)(nbp->m_data) += nbp->m_mxlen
                          - (((size + (MB_ALIGN-1)) / MB_ALIGN) * MB_ALIGN);
#   endif
    nbp->m_data = (void *)((unsigned long) nbp->m_data
                         + (unsigned long) nbp->m_mxlen 
                         - (unsigned long) 
                           (((size + (MB_ALIGN-1)) / MB_ALIGN) * MB_ALIGN));

   /* Update head pointer of elements in chain  */
   for (bp = nbp; bp != NULL; bp = bp->m_next)
      bp->m_head = nbp;

   if (GDEBUG (DBGBANAL))
      __mb_assert_p (nbp, file, line);

   return (nbp);
}


/* ---[ __mb_pushcpy ]----------------------------------------------------

   Summary:

      mb_pushcpy (register struct mbuf *bp, 
                     unsigned short size, void *bufp)

   Description:

      Inserts the specified amount of new space at the beginning of a
      buffer chain and copies the data pointed to by bufp into the new
      space.  If enough space is available in the first buffer, no new
      space is allocated.  Otherwise, a buffers of the appropriate size
      are allocated and tacked on the front of the chain.

   Input:

      bp             Pointer to buffer chain

      size           Number of bytes to allocate at the beginning
                     of the chain

      bufp           Pointer to used data to be copied into the chain

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to new first buffer in the chain, if a new
         buffer was needed, or NULL if failure.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_pushcpy (register struct mbuf *bp, unsigned short size, void *bufp,
               char *file, int line)
{
   register struct mbuf *nbp;
   struct mbuf *bpchain;
   unsigned short len;

   if (GDEBUG (DBGBANAL) && bp)
      __mb_assert_p (bp, file, line);

   /* 
    * Check that bp is real, that it hasn't been duplicated, and
    * that it itself isn't a duplicate before checking to see if
    * there's enough space at its front.
    */
   if ((bp != NULL) && (bp->m_refcnt == 1) && (bp->m_dup == NULL)) {
      /* If buffer is empty, use this one if there's room  */
      if ((bp->m_len == 0) && (bp->m_mxlen >= size)) {
         bp->m_len += size;

         memcpy (MTOD (bp, void *), bufp, size);
         return (bp);
      }

      /* Just adjust this one, if there's room  */
      else if (((char *)bp->m_data - (char *)(bp+1)) >= size) {
#        if 0
             (char *)bp->m_data -= size;
#        endif
         bp->m_data = (void *)((unsigned long) bp->m_data
                             - (unsigned long) size);
         bp->m_len += size;

         memcpy (MTOD (bp, void *), bufp, size);
         return (bp);
      }
   }

   /* Insufficient room - Allocate a new buffer on the head  */
   bpchain = NULL;

   /* Check for partial buffer utilization */
   while (size) {
      len = (size < STDMBLEN) ? size : STDMBLEN;

      nbp = __mb_pushdown (bpchain, len, file, line);

      if (nbp == NULL) {
         __mb_free_p (bpchain, file, line);
         return (NULL);
      }

      memcpy (MTOD (nbp, void *), bufp, len);
      
      /* Append to chain as needed */
      __mb_append (&bpchain, nbp, file, line);

      size -= len;
      bufp  = (char *)bufp + len;
   }

   /* Append the original chain to the end of the new one */
   __mb_append (&bpchain, bp, file, line);

   return (bpchain);
}

