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
/*====[MBQUEUE.C]=============================================================*/
/*                                                                            */
/* Purpose: Buffer Management - Queue Functions                               */
/*                                                                            */
/* Functions:                                                                 */
/*          __mb_free_q ()                                                    */
/*          __mb_len_q ()                                                     */
/*          __mb_enqueue ()                                                   */
/*          __mb_dequeue ()                                                   */
/*          __mb_qdata ()                                                     */
/*          __mb_dqdata ()                                                    */
/*                                                                            */
/*====[MBQUEUE.C]=============================================================*/
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

/* ---[ __mb_free_q ]-----------------------------------------------------

   Summary:

      mb_free_q (struct mbuf **q)

   Description:

      Free an entire queue if mbuf chains

   Input:

      q              Pointer to pointer to head of mbuf queue

   Output:

      None

   Return Value:  

      None

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void
__mb_free_q (struct mbuf **q, char *file, int line)
{
   register struct mbuf *bp;

   while ((bp = __mb_dequeue (q)) != NULL)
      __mb_free_p (bp, file, line);
}


/* ---[ __mb_len_q ]------------------------------------------------------

   Summary:

      mb_len_q (register struct mbuf *q)

   Description:

      Count the number of packets in a queue

   Input:

      q              Pointer to head of mbuf queue

   Output:

      None

   Return Value:  

      unsigned short
         Number of packets in the queue

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
unsigned short
__mb_len_q (register struct mbuf *q)
{
   register unsigned short cnt;
   register struct mbuf *bp = q;

   for (cnt = 0; bp != NULL; cnt++, bp = bp->m_anext)
      ;

   return (cnt);
}


/* ---[ __mb_enqueue ]----------------------------------------------------

   Summary:

      mb_enqueue (struct mbuf **q, struct mbuf *bp)

   Description:

      Append a packet to the end of the queue

   Input:

      q              Pointer to pointer to head of mbuf queue

      bp             Pointer to packet (mbuf chain) to append

   Output:

      None

   Return Value:  

      None

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
void
__mb_enqueue (struct mbuf **q, struct mbuf *bp)
{
   register struct mbuf *p;
   int flags;

   if ((q == NULL) || (bp == NULL))
      return;

   flags = disable_int ();

   /* List is empty, stick at front  */
   if (*q == NULL)
      *q = bp;      

   else {
      for (p = *q ; p->m_anext != NULL ; p = p->m_anext)
         ;

      p->m_anext = bp;
   }

   restore_int ();
}


/* ---[ __mb_dequeue ]----------------------------------------------------

   Summary:

      mb_dequeue (struct mbuf **q)

   Description:

      Unlink a packet from the head of the queue

   Input:

      q              Pointer to pointer to head of mbuf queue

   Output:

      None

   Return Value:  

      struct mbuf *
         Pointer to the packet dequeued, NULL if queuue is empty

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_dequeue (register struct mbuf **q)
{
   register struct mbuf *bp;
   int flags;

   if (q == NULL)
      return (NULL);

   flags = disable_int ();

   if ((bp = *q) != NULL) {
      *q = bp->m_anext;
      bp->m_anext = NULL;
   }

   restore_int ();

   return (bp);
}  


/* ---[ __mb_qdata ]------------------------------------------------------

   Summary:

      mb_qdata (char *data, unsigned short cnt)

   Description:

      copy user data into an mbuf chain

   Input:

      data           Pointer to data to be copied

      cnt            Number of bytes to copy

   Output:

      None

   Return Value:  

      struct mbuf *
         mbuf chain data was copied into

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
struct mbuf *
__mb_qdata (char *data, unsigned short cnt, char *file, int line)
{
   return (__mb_pushcpy (NULL, cnt, data, file, line));
}


/* ---[ __mb_dqdata ]-----------------------------------------------------

   Summary:

      mb_dqdata (struct mbuf *bp, char *buf, unsigned short cnt)

   Description:

      Copy data in mbuf to user buffer deallocating mbuf on success

   Input:

      bp             mbuf to copy data from

      buf            Pointer to buffer to copy data to

      cnt            Number of bytes to copy

   Output:

      None

   Return Value:  

      unsigned short
         Number of bytes copied

   Possible Errors:

      None

   ----------------------------------------------------------------------- */
unsigned short
__mb_dqdata (struct mbuf *bp, char *buf, 
               unsigned short cnt, char *file, int line)
{
   unsigned short tot;

   if (buf == NULL)
      return (0);

   __mb_trim (&bp, cnt, file, line);

   tot = __mb_pullup (&bp, buf, cnt, file, line);

   __mb_free_p (bp, file, line);

   return (tot);
}

