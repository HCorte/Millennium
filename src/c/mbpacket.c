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
/*====[MBPACKET.C]============================================================*/
/*                                                                            */
/* Purpose: Buffer Management - Packet Functions                              */
/*                                                                            */
/* Functions:                                                                 */
/*          __mb_free_p ()                                                    */
/*          __mb_append ()                                                    */
/*          __mb_crunch ()                                                    */
/*          __mb_gather ()                                                    */
/*          __mb_pad_p ()                                                     */
/*          __mb_len_p ()                                                     */
/*          __mb_copy_p ()                                                    */
/*          __mb_dup_p ()                                                     */
/*                                                                            */
/*====[MBPACKET.C]============================================================*/
/*                                                                            */

#include "mbuf_includes.h"

#include <string.h>
#include <ctype.h>

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

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

/* ---[ __mb_free_p ]-----------------------------------------------------

   Summary:

      mb_free_p (register struct mbuf *bp)

   Description:

      Free buffer chain

      Calls mb_free () on each buffer in the chain.

   Input:

      bp             Pointer to buffer chain

   Output:

      None

   Return Value:  

      struct mbuf *
         Pointer to next packet on queue, NULL if none

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_free_p (register struct mbuf *bp, char *file, int line)
{
   register struct mbuf *abp;

   if (bp == NULL)
      return (NULL);

   abp = bp->m_anext;

   while (bp != NULL) {
      if (GDEBUG (DBGBANAL))
         __mb_assert (bp, file, line);

      bp = __mb_free (bp, file, line);
   }

   return (abp);
}


/* ---[ __mb_append ]-----------------------------------------------------

   Summary:

      mb_append (struct mbuf **bpp, struct mbuf *bp)

   Description:

      Append buffer chain to end of a buffer chain

   Input:

      bpp            Pointer to buffer chain

      bp             Pointer to buffer chain to append

   Output:

      bpp            If bpp == NULL (ie. the buffer to append is to be
                     the first buffer in the chain), *bpp will be
                     the new first buffer in the chain

   Return Value:  

      void

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void
__mb_append (struct mbuf **bpp, 
               register struct mbuf *bp, char *file, int line)
{
   register struct mbuf *p;

   if ((bpp == NULL) || (bp == NULL))
      return;

   if (GDEBUG (DBGBANAL))
      __mb_assert_p (bp, file, line);
      
   /* First one on chain  */
   if (*bpp == NULL)
      *bpp = bp;

   else {
      if (GDEBUG (DBGBANAL))
         __mb_assert_p (*bpp, file, line);

      for (p = *bpp; p->m_next != NULL; p = p->m_next)
         MBUDA_CPY (&p->m_uda, &bp->m_uda);

      p->m_next = bp;
   }

   /* Update head pointer of elements in chain  */
   for (bp = *bpp; bp != NULL; bp = bp->m_next)
      bp->m_head = *bpp;

   if (GDEBUG (DBGBANAL) && *bpp)
      __mb_assert_p (*bpp, file, line);
}


/* ---[ __mb_crunch ]-----------------------------------------------------

   Summary:

      mb_crunch (struct mbuf **bpp)

   Description:

      Reclaim unused space in a buffer chain
      
      If the argument is a buffer chain and/or it appears to have 
      wasted space, copy it to a single new buffer and free the old 
      buffer(s).  But refuse to move buffers that merely reference other 
      buffers, or that have other headers referencing them.

      Be extremely careful that there aren't any other pointers to
      (or into) this buffer chain, since we have no way of detecting 
      them here.  This function is meant to be called only when free 
      memory is in short supply.

   Input:

      bpp            Pointer to pointer to buffer chain

   Output:

      bpp            Pointer to new head of buffer chain

   Return Value:  

      void

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void
__mb_crunch (struct mbuf **bpp, char *file, int line)
{
   register struct mbuf *bp = *bpp;
   struct mbuf *nbp;

   /* Can't crunch, there are other refs */
   if ((bp->m_refcnt > 1) || (bp->m_dup != NULL))
      return;

   /* Nothing to be gained by crunching */
   if ((bp->m_next == NULL) && (bp->m_len == bp->m_mxlen))
      return;

   /* Copy failed due to lack of (contiguous) space */
   if ((nbp = 
         __mb_copy_p (bp, __mb_len_p (bp, file, line), file, line)) == NULL)
      return;

   nbp->m_anext = bp->m_anext;

   __mb_free_p (bp, file, line);
   
   *bpp = nbp;
}


/* ---[ __mb_gather ]-----------------------------------------------------

   Summary:

      mb_gather (struct mbuf *bp)

   Description:

      Gather a buffer chain into a single buffers, if it isn't already.

      Note that if the buffer chain is already a single buffer, nothing
      is done; if the gathered chain will not fit in a single buffer,
      the original buffer is freed and you lose everything.

   Input:

      bp             Pointer to buffer chain

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to new buffer chain, NULL upon failure.  If
         original chain is in a single buffer, p == bp.  Original chain 
         is freed.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_gather (struct mbuf *bp, char *file, int line)
{
   struct mbuf *p;

   if (bp == NULL)
      return (NULL);

   /* Already in a single buffer  */
   if (bp->m_next == NULL)
      p = bp;
   
   else {
      p = __mb_copy_p (bp, __mb_len_p (bp, file, line), file, line);
      __mb_free_p (bp, file, line);
   }

   return (p);
}


/* ---[ __mb_pad_p ]------------------------------------------------------

   Summary:

      mb_pad_p (struct mbuf **bpp, unsigned short len, char c)

   Description:

      Pad the end of the buffer chain with the specified character

   Input:

      bpp            Pointer to pointer to buffer chain

      len            Length to pad buffer

      c              Fill character

   Output:

      bpp            Pointer to new head of buffer chain

   Return Value:  

      void

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void
__mb_pad_p (struct mbuf **bpp, unsigned short len, char c, 
            char *file, int line)
{
   struct mbuf *p, *mbp;
   unsigned short room;
   char *ppad;

   if ((bpp == NULL) || (*bpp == NULL))
      return;

   /* Go to last mbuf in the packet  */
   for (p = *bpp; p->m_next != NULL; p = p->m_next)
      ;

   /* Is there enough room at the end of the current buffer */
   room = p->m_mxlen - p->m_len - 
            ((char *)p->m_data - (char *)p - sizeof (struct mbuf));

   /* Enough room at the tail of the current mbuf */
   if (len < room) {
      ppad = (char *)p->m_data + p->m_len;
      p->m_len += len;
   }

   else {
      mbp = __mb_pushdown (NULL, len, file, line);

      __mb_append (bpp, mbp, file, line);
      
      ppad = (char *)mbp->m_data;
   }

   memset (ppad, c, len);
}


/* ---[ __mb_len_p ]------------------------------------------------------

   Summary:

      mb_len_p (register struct mbuf *bp)

   Description:

      Count up the total number of bytes in a buffer chain

   Input:

      bp             Pointer to buffer chain

   Output:

      None

   Return Value:  

      unsigned short
         Returns length of buffer chain

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
unsigned short
__mb_len_p (register struct mbuf *bp, char *file, int line)
{
   register unsigned short cnt = 0;

   while (bp != NULL) {
/*    __mb_assert_p (bp, file, line);  */

      cnt += bp->m_len;
      bp = bp->m_next;
   }

   return (cnt);
}


/* ---[ __mb_copy_p ]-----------------------------------------------------

   Summary:

      mb_copy_p (register struct mbuf *bp, register unsigned short cnt)

   Description:

      Copy first 'cnt' bytes of buffer chain into a new, single buffer

      Note that if 'cnt' is larger than a single buffer, only one
      buffer is allocated, and the trailing bytes will NOT be copied.

      Note also that the original buffer chain is NOT freed.

   Input:

      bp             Pointer to buffer chain to copy

      cnt            Number of bytes to copy

   Output:

      None

   Return Value:  

      struct mbuf *
         Returns pointer to new buffer, or NULL if copy failed

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_copy_p (register struct mbuf *bp, 
            register unsigned short cnt, char *file, int line)
{
   register struct mbuf *cp;
   register char *wp;
   register unsigned short n;

   /* Allocate new buffer  */

   /* This copy routine is only used by mb_gather and mb_crunch.  The MX   */ 
   /* Server does not make use of mb_crunch, therefore calls to that       */
   /* routine were not considered in this change.  The mb_gather calls     */
   /* made by the MX Server are critical to reading and writing of data    */
   /* over the MX interface.  In particular the writing of data frees      */
   /* mbufs; potentially tens or even hundreds of them.  Therefore, the    */
   /* mbuf allocation is deeded a priority.                                */

   if ((bp == NULL) || (cnt == 0) || 
      ((cp = __mb_alloc_pri (cnt, file, line)) == NULL))
      return (NULL);

   wp = cp->m_data;

   while ((cnt != 0) && (bp != NULL)) {
      n = _MIN (cnt, bp->m_len);
   
      memcpy (wp, bp->m_data, n);
      
      wp += n;
      cp->m_len += n;
      cnt -= n;

      bp = bp->m_next;
   }

   return (cp);
}


/* ---[ __mb_dup_p ]----------------------------------------------------

   Summary:

      mb_dup_p (struct mbuf **bpp, register struct mbuf *bp,
                     register unsigned short offset, unsigned short cnt)

   Description:

      Duplicate first 'cnt' bytes of buffer chain starting at 'offset'.

      This is done without copying any data; only the headers are 
      duplicated, but without data segments of their own.  The pointers 
      are set up to share the data segments of the original copy.  

   Input:

      bpp            Pointer to pointer in which to return the new
                     buffer chain

      bp             Pointer to buffer chain to duplicate

      offset         Point at which new buffer chain is to start

      cnt            Number of bytes to duplicate, starting at offset

   Output:

      bpp            Pointer to new duplicated buffer chain, or NULL 
                     if duplication failed

   Return Value:  

      unsigned short
         Returns the number of bytes actually duplicated, or 0 if the
         duplication failed

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
unsigned short
__mb_dup_p (struct mbuf **bpp, register struct mbuf *bp,
          register unsigned short offset, 
          unsigned short cnt, char *file, int line)
{
   register struct mbuf *cp;
   unsigned short tot, ooff;

   if ((cnt == 0) || (bp == NULL) || (bpp == NULL)) {
      if (bpp != NULL)
         *bpp = NULL;

      if (GDEBUG (DBGBERRS))
         logger (LOGERR, 
                  "mb_dup_p: Entry error - cnt %d, bp 0x%X, bpp 0x%X, caller: module %s, line %d",
                  cnt, bp, bpp, file, line);

      return (0);
   }

   if ((*bpp = cp = __mb_alloc (0, file, line)) == NULL)
      return (0);

   /* Copy user definable field from first mbuf  */
   MBUDA_CPY (&cp->m_uda, &bp->m_uda);

   ooff = offset;

   /* Skip over leading mbufs that are smaller than the offset */
   while ((bp != NULL) && (bp->m_len <= offset)) {
      offset -= bp->m_len;
      bp = bp->m_next;
   }

   if (bp == NULL) {
      __mb_free (cp, file, line);

      *bpp = NULL;

      if (GDEBUG (DBGBERRS))
         logger (LOGERR, 
                  "mb_dup_p: Offset too big - cnt %d, offset %d, caller: module %s, line %d",
                  cnt, ooff, file, line);

      /* Offset was too big */
      return (0); 
   }

   tot = 0;
   
   for (;;) {
      /* Make sure we get the original, "real" buffer (i.e. handle the
       * case of duplicating a duplicated buffer)
       */
      cp->m_dup = (bp->m_dup != NULL)? bp->m_dup : bp;

      /* Increment the duplicated buffer's reference count */
      cp->m_dup->m_refcnt++;

      cp->m_data = (char *)bp->m_data + offset;
      cp->m_len  = _MIN (cnt, bp->m_len - offset);
      
      cp->m_head = *bpp;

      offset = 0;

      cnt -= cp->m_len;
      tot += cp->m_len;
      
      bp = bp->m_next;
      
      if ((cnt == 0) || (bp == NULL) || 
         ((cp->m_next = __mb_alloc (0, file, line)) == NULL))
         break;

      cp = cp->m_next;
   }

   return (tot);
}
