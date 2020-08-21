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
/*====[MBUF.C]================================================================*/
/*                                                                            */
/* Purpose: Buffer Management - General Functions                             */
/*                                                                            */
/* Functions:                                                                 */
/*          __mb_trim ()                                                      */
/*          __mb_pullchar ()                                                  */
/*          __mb_pull16 ()                                                    */
/*          __mb_pull32 ()                                                    */
/*          __mb_get16 ()                                                     */
/*          __mb_get32 ()                                                     */
/*          __mb_put16 ()                                                     */
/*          __mb_put32 ()                                                     */
/*          __mb_dump ()                                                      */
/*          __mb_hexdump ()                                                   */
/*          __mb_getchar ()                                                   */
/*          __mb_putchar ()                                                   */
/*                                                                            */
/*====[MBUF.C]================================================================*/
/*                                                                            */

#include "mbuf_includes.h"

#include <string.h>
#include <ctype.h>

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#    include "gdata.h"
#    include "bufpool.h"

#    include "logger.h"

#elif defined(XOS_VMS)

#    include "gdata.h"
#    include "bufpool.h"

#    include "logger.h"

#else

#    error - OS-specific logic not handled.

#endif

static void __mb_dumpbuf (unsigned char sev, struct mbuf *bp);

/* ---[ __mb_trim ]-------------------------------------------------------

   Summary:

      mb_trim (struct mbuf **bpp, unsigned short length)

   Description:

      Trim buffer to specified length by lopping off end

   Input:

      bpp            Pointer to pointer to buffer chain

      length         Desired resulting length of buffer chain

   Output:

      bpp            Pointer to new first buffer in chain (ie. if the 
                     new length is 0, the entire buffer chain is 
                     freed, and *bpp == NULL)

   Return Value:  

      void

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void
__mb_trim (struct mbuf **bpp, unsigned short length, char *file, int line)
{
   register unsigned short tot;
   register struct mbuf *bp;

   /* Check if anything to trim  */
   if ((bpp == NULL) || (*bpp == NULL))
      return;

   if (GDEBUG (DBGBANAL))
      __mb_assert_p (*bpp, file, line);

   /* Toss the whole thing if resulting length is zero  */
   if (length == 0) {
      __mb_free_p (*bpp, file, line);

      *bpp = NULL;
      
      return;
   }

   /* 
    * Find the point at which to trim. If length is greater than
    * the chain, we'll just fall through without doing anything
    */
   for (tot = 0, bp = *bpp; bp != NULL; bp = bp->m_next) {
      if (tot + bp->m_len < length)
         tot += bp->m_len;
      
      else {
         /* Cut here  */
         bp->m_len = length - tot;

         __mb_free_p (bp->m_next, file, line);

         bp->m_next = NULL;
         break;
      }
   }
}


/* ---[ __mb_pullchar ]---------------------------------------------------

   Summary:

      mb_pullchar (struct mbuf **bpp)

   Description:

      Pull single character from buffer

   Input:

      bpp            Pointer to pointer to buffer chain

   Output:

      bpp            Pointer to new head of buffer chain

   Return Value:  

      int
         Returns pulled value, or -1 upon failure.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
int
__mb_pullchar (struct mbuf **bpp, char *file, int line)
{
   char c;

   /* Check if anything left  */
   if (__mb_pullup (bpp, &c, 1, file, line) != 1)
      return (-1);

   return ((int) ((unsigned char) c));
}


/* ---[ __mb_pull16 ]-----------------------------------------------------

   Summary:

      mb_pull16 (struct mbuf **bpp)

   Description:

      Pull a 16-bit integer in host order from buffer, which is in 
      network byte order.  

   Input:

      bpp            Pointer to pointer to buffer chain

   Output:

      bpp            Pointer to new head of buffer chain

   Return Value:  

      long
         Returns pulled value, or -1 upon failure.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
long
__mb_pull16 (struct mbuf **bpp, char *file, int line)
{
   char buf[2];

   /* Check if anything left  */
   if (__mb_pullup (bpp, buf, 2, file, line) != 2)
      return (-1);

   return (GET16 (buf));
}


/* ---[ __mb_pull32 ]-----------------------------------------------------

   Summary:

      mb_pull32 (struct mbuf **bpp)

   Description:

      Pull a 32-bit integer in host order from buffer, which is in 
      network byte order.  

   Input:

      bpp            Pointer to pointer to buffer chain

   Output:

      bpp            Pointer to new head of buffer chain

   Return Value:  

      long
         Returns pulled value, or zero upon failure.  Note that the 
         failure return is indistinguishable from a normal return.

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
long
__mb_pull32 (struct mbuf **bpp, char *file, int line)
{
   char buf[4];

   /* Return zero if insufficient buffer  */
   if (__mb_pullup (bpp, buf, 4, file, line) != 4)
      return (0);

   return (GET32 (buf));
}


/* ---[ __mb_get16 ]------------------------------------------------------

   Summary:

      mb_get16 (register char *cp)

   Description:

      Get a 16-bit unsigned integer out of a character array in network 
      order, and return in host order.

   Input:

      cp             Pointer to character array

   Output:

      None

   Return Value:  

      unsigned short
         Returned integer

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
unsigned short
__mb_get16 (register char *cp)
{
   register unsigned short x;

   x  = (unsigned char) (*cp++); x <<= 8;
   x |= (unsigned char) (*cp);

   return (x);
}


/* ---[ __mb_get32 ]------------------------------------------------------

   Summary:

      mb_get32 (register char *cp)

   Description:

      Get a 32-bit signed integer out of a character array in network 
      order, and return in host order.

      Machine-independent, alignment insensitive network-to-host long 
      conversion

   Input:

      cp             Pointer to character array

   Output:

      None

   Return Value:  

      long
         Returned integer

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
long
__mb_get32 (register char *cp)
{
   long rval;

   rval  = (unsigned char) (*cp++); rval <<= 8;
   rval |= (unsigned char) (*cp++); rval <<= 8;
   rval |= (unsigned char) (*cp++); rval <<= 8;
   rval |= (unsigned char) (*cp);

   return (rval);
}


/* ---[ __mb_put16 ]------------------------------------------------------

   Summary:

      mb_put16 (register char *cp, unsigned short x)

   Description:

      Put a 16-bit unsigned integer in host order into a character 
      array in network order.

   Input:

      cp             Pointer to character array

      x              Integer to insert in character array

   Output:

      None

   Return Value:  

      char *
         Bumps cp to point next character, and returns this pointer

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
char *
__mb_put16 (register char *cp, unsigned short x)
{
   *cp++ = x >> 8;
   *cp++ = x;

   return (cp);
}


/* ---[ __mb_put32 ]------------------------------------------------------

   Summary:

      mb_put32 (register char *cp, long x)

   Description:

      Put a 32-bit signed integer in host order into a character array 
      in network order.

   Input:

      cp             Pointer to character array

      x              Integer to insert in character array

   Output:

      None

   Return Value:  

      char *
         Bumps cp to point next character, and returns this pointer

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
char *
__mb_put32 (register char *cp, long x)
{
   *cp++ = x >> 24;
   *cp++ = x >> 16;
   *cp++ = x >> 8;
   *cp++ = x;

   return (cp);
}


/* ---[ __mb_dump ]-------------------------------------------------------

   Summary:

      mb_dump (unsigned char sev, char *str, struct mbuf *bp)

   Description:

      Dump given buffer chain (header structure, and data in hex) to 
      standard output.

   Input:

      str            Leading identifying string to be printed out

      bp             Pointer to buffer chain

   Output:

      None

   Return Value:  

      void

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void 
__mb_dump (unsigned char sev, char *str, struct mbuf *bp)
{
   struct mbuf *tbp;

   logger (sev, "%s", (str)? str : "<NULL>");

   log_start ();

   for (tbp = bp; tbp != NULL; tbp = tbp->m_next)
      __mb_dumpbuf (sev, tbp);

   log_stop ();
}


/* ---[ __mb_dumpbuf ]----------------------------------------------------

   Summary:

      mb_dumpbuf (unsigned char sev, struct mbuf *bp)

   Description:

      Dump given single buffer (header structure, and data in hex) to 
      standard output.

   Input:

      bp             Pointer to buffer chain

   Output:

      None

   Return Value:  

      void

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
static void 
__mb_dumpbuf (unsigned char sev, struct mbuf *bp)
{
   if (bp->m_foot != BUFFOOT) {
      logger (sev, "Attempted dump of an invalid buffer <0x%08.8X>", bp);
      return;
   }

/* Code modification for host port */

   logger (sev, "Pool %d, task %s, buffer 0x%p, next 0x%p, head 0x%p", 
            bp->m_pool,
	    bp, bp->m_next, bp->m_head);

/* Original GTX code, removed when ported to the host

   logger (sev, "Pool %d, task %s, buffer 0x%p, next 0x%p, head 0x%p", 
            bp->m_pool, 
            (cur_tdesc != NULL)? ((TDESCTYP *)cur_tdesc)->tkey : "????",
            bp, bp->m_next, bp->m_head);
*/

   logger (sev, "Anext 0x%08.8X, Dup 0x%08.8X, Ref %d",
            bp->m_anext, bp->m_dup, bp->m_refcnt);
            
   logger (sev, "UDA 0x%08.8X, 0x%08.8X, 0x%08.8X, 0x%08.8X, off %d",
            bp->m_uda.stk[0], bp->m_uda.stk[1], 
            bp->m_uda.stk[2], bp->m_uda.stk[3], bp->m_uda.off);
            
   logger (sev, "Len %d, Mxlen %d, Data 0x%08.8X",
            bp->m_len, bp->m_mxlen, bp->m_data);

/* Code modification for host port */

   logger (sev, "Last free:  module %s, line %d, task %s",
            bp->mdbg_ffile, bp->mdbg_fline);

   logger (sev, "Last alloc: module %s, line %d, task %s",
            bp->mdbg_afile, bp->mdbg_aline);

/* Original GTX code, removed when ported to the host

   logger (sev, "Last free:  module %s, line %d, task %s",
            (bp->mdbg_ftask)? bp->mdbg_ftask->tkey : "????");

   logger (sev, "Last alloc: module %s, line %d, task %s",
            bp->mdbg_afile, bp->mdbg_aline,
            (bp->mdbg_atask)? bp->mdbg_atask->tkey : "????");

*/

   hexdump (sev, NULL, bp->m_data, bp->m_len, 1);
}


/* ---[ __mb_hexdump ]----------------------------------------------------

   Summary:

      mb_hexdump (unsigned char sev, char *str, struct mbuf *bp)

   Description:

      Dump given buffer chain (data in hex) to standard output.

   Input:

      str            Leading identifying string to be printed out

      bp             Pointer to buffer chain

   Output:

      None

   Return Value:  

      void

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void 
__mb_hexdump (unsigned char sev, 
               char *str, struct mbuf *bp, char *file, int line)
{
   int len;
   char buf[16];
   unsigned short n;
   struct mbuf *tbp;

   if (bp == NULL)
      logger (sev, "%s - Attempted NULL hex buffer dump", 
               (str)? str : "<NULL>");
   
   else if ((len = __mb_len_p (bp, file, line)) == 0)
      logger (sev, "%s - Empty packet", (str)? str : "<NULL>");

   else if (__mb_dup_p (&tbp, bp, 0, len, file, line)) {
      if (str) {
         logger (sev, str);
         log_start ();
      }

      while (tbp && ((n = __mb_pullup (&tbp, buf, 16, file, line)) != 0))
         hexdump (sev, NULL, buf, n, (str)? 1 : 0);

      log_stop ();
   }
}


/* ---[ __mb_getchar ]----------------------------------------------------
                                                                          
   Summary:                                                               
                                                                          
      mb_getchar (struct mbuf *bp, unsigned int index)                             
                                                                          
   Description:                                                           
                                                                          
      Extract the character from the mbuf chain at the specified offset.  
                                                                          
   Input:                                                                 
                                                                          
      bp             Pointer to buffer chain
                                                                          
      index          Location to extract char from                        
                                                                          
   Output:                                                                
                                                                          
      None                                                                
                                                                          
   Return Value:                                                          
                                                                          
      char           mbuf data character
                                                                          
   Possible Errors:                                                       
                                                                          
      None                                                                
                                                                          
   ----------------------------------------------------------------------- */
char                                                                         
__mb_getchar (struct mbuf *bp, unsigned int index)
{                                                                            
   register int i;
   struct mbuf *mbp;

   if (bp == NULL)
      return (0);

   mbp = bp;
   i = index;

   while (i >= mbp->m_len) {
      i -= mbp->m_len;

      if (mbp->m_next != NULL)
         mbp = mbp->m_next;

      /* Fell off the end  */
      else
         i = (mbp->m_len > 0)? mbp->m_len - 1 : 0;
   }

   return (((char *)(mbp->m_data))[i]);
}


/* ---[ __mb_putchar ]----------------------------------------------------
                                                                          
   Summary:                                                               
                                                                          
      mb_putchar (struct mbuf *bp, char ch, unsigned int index)                    
                                                                          
   Description:                                                           
                                                                          
      Write the character into the mbuf chain at the specified offset.    
                                                                          
   Input:                                                                 
                                                                          
      bp             Pointer to buffer chain
                                                                          
      ch             char to write
                                                                          
      index          Location to write char
                                                                          
   Output:                                                                
                                                                          
      None                                                                
                                                                          
   Return Value:                                                          
                                                                          
      None                                                                
                                                                          
   Possible Errors:                                                       
                                                                          
      None                                                                
                                                                          
   ----------------------------------------------------------------------- */
void                                                                         
__mb_putchar (struct mbuf *bp, char ch, unsigned int index)
{                                                                            
   register int i;
   struct mbuf *mbp;

   if (bp == NULL)
      return;

   mbp = bp;
   i = index;

   while (i >= mbp->m_len) {
      i -= mbp->m_len;

      if (mbp->m_next != NULL)
         mbp = mbp->m_next;

      /* Fell off the end  */
      else
         i = (mbp->m_len > 0)? mbp->m_len - 1 : 0;
   }

   ((char *)(mbp->m_data))[i] = ch;
}
