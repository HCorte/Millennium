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
/*====[BUFPOOL.C]=============================================================*/
/*                                                                            */
/* Purpose: Buffer Pool Management                                            */
/*                                                                            */
/* Functions:                                                                 */
/*          bufp_mem ()                                                       */
/*          bufp_config ()                                                    */
/*          bufp_init ()                                                      */
/*          bufp_defmask ()                                                   */
/*          buf_alloc ()                                                      */
/*          buf_allocmax ()                                                   */
/*          buf_free ()                                                       */
/*                                                                            */
/*====[BUFPOOL.C]=============================================================*/
/*                                                                            */

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include "mbuf_includes.h"

#elif defined(XOS_VMS)

#   include "mbuf_includes.h"

#else

#   error - OS-specific logic not handled.

#endif

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#define  ALLOCATE

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include "bufpool.h"
#   include "gdata.h"

#elif defined(XOS_VMS)

#   include "bufpool.h"
#   include "gdata.h"

#else

#   error - OS-specific logic not handled.

#endif

#undef   ALLOCATE

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include "logger.h"

#elif defined(XOS_VMS)

#   include "logger.h"

#else

#   error - OS-specific logic not handled.

#endif

#define BUFPOOL_VERIFY     /* Perform buffer pool integrity check          */
#undef  BUFPOOL_VERBOSE    /* Trace each buffer as it is verified          */

#define VBUFTAIL(bp)       (*(unsigned long *)((char *)(bp) + \
                              sizeof (struct mbuf) + (bp)->m_mxlen))

static struct mbuf *bufp_allocbuf (int pool, int priority, 
                                   char *file, int line);
static void bufp_freebuf (int pool, struct mbuf *bp, char *file, int line);


void
bufp_mem (void) {
    int pool;

    /* Ensure sufficient memory allocation available for buffer pools  */
    for( pool = 0; pool < MAX_BUFPOOLS; pool++ ) {
        if( bufpool[pool].nbuf ) {

/* Original GTX code, removed when ported to the host

            psos_ct.kc_nmsgbuf += bufpool[pool].nbuf;
            memreserve (1, bufpool[pool].nbuf * bufpool[pool].bsize);

*/

        }
    }
}

int
bufp_config (int pool, struct BUFPOOL *bfp) {
    int rem;

    /* Check for valid pool offset  */
    if( pool > MAX_BUFPOOLS ) {
        printf ("Unable to configure buffer pool %s (%d) - Invalid pool number\n",
                bfp->name, pool);

        return(-1);
    }

    /* Copy pool descriptor  */
    bufpool[pool] = *bfp;

    /* Round buffer size to long-word multiple  */
    if( bufpool[pool].dsize )
        bufpool[pool].dsize += bufpool[pool].dsize % sizeof (unsigned long);

    /* Calculate allocation size from buffer data size  */
    bufpool[pool].bsize  = bufpool[pool].dsize + 
                           sizeof (struct mbuf) +
                           sizeof (unsigned long);  /* Buffer tailprint  */

    /* Do not adjust length of zero-length buffers  */
    if( bufpool[pool].dsize ) {
        /* Round up allocation to 16-byte multiple, for ease in pROBEing  */
        rem = 16 - (bufpool[pool].bsize % 16);

        bufpool[pool].dsize += rem;
        bufpool[pool].bsize += rem;
    }

    bufpool[pool].pool = pool;

    return(0);
}

static int
bufp_verify (int pool) {
    int nbuf;
    struct mbuf *bp;
    struct BUFPOOL **poolpp;
    char *padr, *qadr, *pend, *qend;
    unsigned long psize, qsize;

    if( GDEBUG (DBGSYS) ) {
        printf ("Pool verify - pool %d, nbuf %ld\n",
                pool, 
                bufpool[pool].nbuf);

        printf ("              dsize %ld, bsize %ld, addr %p, %ld bytes\n",
                bufpool[pool].dsize, 
                bufpool[pool].bsize,
                bufpool[pool].block,
                bufpool[pool].nbuf * bufpool[pool].bsize);
    }

    psize = bufpool[pool].nbuf * bufpool[pool].bsize;

    /* Make sure this buffer pool doesn't overlap any other  */
    for( poolpp = &bufplist[0]; *poolpp; poolpp++ ) {
        if( (*poolpp)->pool != pool ) {
            qsize = (*poolpp)->nbuf * (*poolpp)->bsize;

            padr = (char *)bufpool[pool].block;
            pend = padr + psize;

            qadr = (char *)((*poolpp)->block);
            qend = qadr + qsize;

            if( ((qadr >= padr) && (qadr < pend)) ||
                ((padr >= qadr) && (padr < qend)) ) {

                printf ("Verify fail - Pool overlap, pool %d, addr %p, size %ld, end %p\n", 
                        pool, 
                        bufpool[pool].block, 
                        psize,
                        (char *)(bufpool[pool].block) + psize);
                printf ("              overlaps pool %ld, addr %p, size %ld, end %p\n", 
                        (*poolpp)->pool, 
                        (*poolpp)->block, 
                        qsize,
                        (char *)((*poolpp)->block) + qsize);

                return(-1);
            }
        }
    }

    /* Run through pool  */
    for( bp = bufpool[pool].free, nbuf = 0; bp != NULL; nbuf++ ) {
        /* Doesn't smell like a buffer  */
        if( bp->m_foot != BUFFOOT ) {
            printf ("Verify fail - Invalid buffer footprint, pool %d, buf %p, num %d\n", 
                    pool, bp, nbuf);
            printf ("              foot %4.4lX, next %p\n", 
                    bp->m_foot, bp->m_next);

            return(-1);
        }

        /* Verify buffer tailprint, to see if we've overrun the buffer  */
        else if( VBUFTAIL (bp) != BUFTAIL ) {
            printf ("Verify fail - Invalid buffer tailprint, pool %d, buf %p, num %d\n", 
                    pool, bp, nbuf);
            printf ("              tail %4.4lX, next %p\n", 
                    VBUFTAIL (bp), bp->m_next);

            return(-1);
        }

        /* Pool message apparently wasn't properly deallocated  */
        else if( bp->m_alloc ) {
            printf ("Verify fail - Non-free buffer, pool %d, buf %p, num %d\n", 
                    pool, bp, nbuf);

            return(-1);
        }

        else {
            /* Clear buffer, as a rudimentary memory check  */
            if( bp->m_mxlen > 0 )
                memset ((char *)bp + sizeof (struct mbuf), BUFSCRUB, bp->m_mxlen);

#           ifdef BUFPOOL_VERBOSE

                printf ("Buff verify - pool %d, buf 0x%X, num %d, head 0x%X, len %d\n", 
                        pool, bp, nbuf, 
                        (char *)bp + sizeof (struct mbuf), 
                        bp->m_mxlen);

#           endif

            bp = bp->m_next;
        }
    }      

    /* Check free count is accurate  */
    if( nbuf != bufpool[pool].nfree ) {
        printf ("Verify fail - Bad free count, pool %d, nfree %ld, nbuf %d\n", 
                pool, bufpool[pool].nfree, nbuf);

        return(-1);
    }

    return(0);
}

static int
bufp_create (int pool) {
    int i;
    struct mbuf *bp;
    unsigned long poolsize;

    poolsize = bufpool[pool].nbuf * bufpool[pool].bsize;

    /* Allocate buffer memory  */
    if( (bufpool[pool].block = (struct mbuf *) malloc (poolsize)) == NULL ) {
        printf ("Unable to create buffer pool %s - Cannot allocate memory (%ld bytes)",
                bufpool[pool].name, poolsize);

        return(-1);
    }

    bp = bufpool[pool].block;

    if( GDEBUG (DBGSYS) )
        printf ("Buffer pool: %d, nbuf %ld, dsize %ld, bsize %ld, addr %p, %ld bytes\n",
                pool, 
                bufpool[pool].nbuf, 
                bufpool[pool].dsize, 
                bufpool[pool].bsize, 
                bufpool[pool].block,
                poolsize);

    /* Clear memory, as a rudimentary memory check  */
    memset (bp, BUFSCRUB, poolsize);

    /* Initialise free list  */
    bufpool[pool].free = NULL;
    bufpool[pool].nfree = 0;

    /* Create buffers, and hang off free list  */
    for( i = 0; i < bufpool[pool].nbuf; i++ ) {
        bp->m_foot = BUFFOOT;
        bp->m_pool = pool;

        bp->m_alloc = 0;

        bp->m_mxlen = bufpool[pool].dsize;

/* Original GTX code, removed when ported to the host

        bp->mdbg_atask = (TDESCTYP *)cur_tdesc;

*/

        bp->mdbg_afile = __FILE__;
        bp->mdbg_aline = __LINE__;

        VBUFTAIL (bp) = BUFTAIL;

        /* Enqueue buffer on free list  */
        bufp_freebuf (pool, bp, __FILE__, __LINE__);

        bp = (struct mbuf *) ((unsigned char *)bp + bufpool[pool].bsize);
    }

    /* Clear statistics  */
    bufpool[pool].nalloc = 0;
    bufpool[pool].maxalloc = 0;
    bufpool[pool].nallocfail = 0;

    bufpool[pool].thexc = 0;

    bufpool[pool].dmin = -1;
    bufpool[pool].dmax = -1;

    return(0);
}

void
bufp_init (void) {
    int i, j;
    struct BUFPOOL *jp, *ip, **poolpp;

    /* Initialise the pool pointer list in pool number order  */
    for( i = 0; i < MAX_BUFPOOLS; i++ )
        bufplist[i] = &bufpool[i];

    /* Sort the pools by ascending buffer size  */
    for( i = 0; i < (MAX_BUFPOOLS - 1); i++ ) {
        for( j = i + 1; j < MAX_BUFPOOLS; j++ ) {
            ip = bufplist[i];
            jp = bufplist[j];

            /* Swap pools  */
            if( jp->nbuf && ((ip->nbuf == 0) || (jp->dsize < ip->dsize)) ) {
                bufplist[i] = jp;
                bufplist[j] = ip;
            }
        }
    }

    /* Null out pointers to empty pools  */
    for( i = 0; i < MAX_BUFPOOLS; i++ ) {
        if( bufplist[i]->nbuf == 0 )
            bufplist[i] = NULL;
    }

    /* Create all configured buffer pools  */
    for( poolpp = &bufplist[0]; *poolpp; poolpp++ ) {
        if( bufp_create ((*poolpp)->pool) < 0 )
            FATAL (0);
    }

#   ifdef BUFPOOL_VERIFY

        /* Verify buffer pool integrity  */
        for( poolpp = &bufplist[0]; *poolpp; poolpp++ ) {
            if( bufp_verify ((*poolpp)->pool) < 0 )
                FATAL (0);
        }

#   endif

}

/* Original GTX code, removed when ported to the host

void
bufp_defmask (TDESCTYP *tdesc)
{
    struct DFBUFPCFG *dfcfgp;

-- Null task descriptor means current task  --
    if (tdesc == NULL)
        tdesc = cur_tdesc;

-- Find default buffer pool configuration for this task  --
    for (dfcfgp = &dfbufpcfg[0]; dfcfgp->tname != NULL; dfcfgp++) {
        if (strcmp (tdesc->name, dfcfgp->tname) == 0) {
            tdesc->bpmask = dfcfgp->bpmask;
            break;
        }
    }
};

    INLINE
*/

static struct mbuf * 
bufp_allocbuf (int pool, int priority, char *file, int line) {
    struct mbuf *bp;

    /* Check for empty pool  */
    if( bufpool[pool].nfree == 0 ) {
        bufpool[pool].nallocfail++;

        if( GDEBUG (DBGBERRS) )
            logger (LOGERR, 
                    "bufp_allocbuf: Empty pool - pool %d, nfree %d, nalloc %d, maxalloc %d, nallocfail %d, caller: module %s, line %d",
                    pool, 
                    bufpool[pool].nfree, 
                    bufpool[pool].nalloc, 
                    bufpool[pool].maxalloc, 
                    bufpool[pool].nallocfail, file, line);

        return(NULL);
    }

    /* Check if reserve threshold exceeded  */
    else if( bufpool[pool].nfree <= bufpool[pool].thold ) {
        bufpool[pool].thexc++;
	
/* Code modification for host port */

        if( !priority )

/* Original GTX code, removed when ported to the host

        if( !priority || 
           (priority && (cur_tdesc != NULL) && 
            !(((TDESCTYP *)cur_tdesc)->bpmask && (1 << pool))) )
*/

        {
            bufpool[pool].nallocfail++;

            if( GDEBUG (DBGBERRS) )
                logger (LOGERR, 
                        "bufp_allocbuf: Threshold exceeded - pool %d, nfree %d, thold %d, nalloc %d, maxalloc %d, nallocfail %d, caller: module %s, line %d",
                        pool, 
                        bufpool[pool].nfree, 
                        bufpool[pool].thold, 
                        bufpool[pool].nalloc, 
                        bufpool[pool].maxalloc, 
                        bufpool[pool].nallocfail, file, line);

            return(NULL);
        }
    }

    /* Pull a buffer off of free queue  */
    if( (bp = bufpool[pool].free) == NULL ) {
        logger (LOGFATAL, 
                "bufp_allocbuf: Free count doesn't match - pool %d, file %s, line %d",
                pool, file, line);

        UFATAL (0);
    }

    /* Doesn't smell like a buffer  */
    else if( bp->m_foot != BUFFOOT ) {
        logger (LOGFATAL, 
                "bufp_allocbuf: Invalid buffer footprint - pool %d, msg %X",
                pool, bp);

        logger (LOGFATAL, "Caller: module %s, line %d", file, line);

        UFATAL (0);
    }

    /* Verify buffer tailprint, to see if we've overrun the buffer  */
    else if( VBUFTAIL (bp) != BUFTAIL ) {
        logger (LOGFATAL, 
                "bufp_allocbuf: Invalid buffer tailprint - pool %d, msg %X",
                pool, bp);

        logger (LOGFATAL, "Caller:     module %s, line %d", file, line);

/* Code modification for host port */

        logger (LOGFATAL, "Last alloc: module %s, line %d, task %s", 
                bp->mdbg_afile, bp->mdbg_aline);

        logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
	        bp->mdbg_ffile, bp->mdbg_fline);

/* Original GTX code, removed when ported to the host

        logger (LOGFATAL, "Last alloc: module %s, line %d, task %s",
                bp->mdbg_afile, bp->mdbg_aline,
                (bp->mdbg_atask)? bp->mdbg_atask->tkey : "????");

        logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
                bp->mdbg_ffile, bp->mdbg_fline,
                (bp->mdbg_ftask)? bp->mdbg_ftask->tkey : "????");
*/

        UFATAL (0);
    }

    /* Pool message apparently wasn't properly deallocated  */
    else if( bp->m_alloc ) {
        logger (LOGFATAL, "bufp_allocbuf: Non-deallocated buffer - pool %d, msg %X",
                pool, bp);

        logger (LOGFATAL, "Caller:     module %s, line %d", file, line);

/* Code modification for host port */

        logger (LOGFATAL, "Last alloc: module %s, line %d, task %s", 
                bp->mdbg_afile, bp->mdbg_aline);

        logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
	        bp->mdbg_ffile, bp->mdbg_fline);

/* Original GTX code, removed when ported to the host

        logger (LOGFATAL, "Last alloc: module %s, line %d, task %s",
                bp->mdbg_afile, bp->mdbg_aline,
                (bp->mdbg_atask)? bp->mdbg_atask->tkey : "????");

        logger (LOGFATAL, "Last free:  module %s, line %d, task %s", 
                bp->mdbg_ffile, bp->mdbg_fline,
                (bp->mdbg_ftask)? bp->mdbg_ftask->tkey : "????");

*/

        UFATAL (0);
    }

    else {
        /* Remove from free list  */
        bufpool[pool].free = bp->m_next;

        /* Flag the buffer as allocated */
        bp->m_alloc = 1;

        bp->m_head = NULL;
        bp->m_next = NULL;

        /* Update pool statistics  */
        bufpool[pool].nfree--;

        bufpool[pool].nalloc++;
        bufpool[pool].maxalloc = 
        _MAX (bufpool[pool].nalloc, bufpool[pool].maxalloc);

/* Original GTX code, removed when ported to the host

        bp->mdbg_atask = (TDESCTYP *)cur_tdesc;

*/

        bp->mdbg_afile = file;
        bp->mdbg_aline = line;

        /* Update task statistics  */

/* Original GTX code, removed when ported to the host

        if( cur_tdesc != NULL ) {
            ((TDESCTYP *)cur_tdesc)->memstat.buf[pool].nalloc++;
            ((TDESCTYP *)cur_tdesc)->memstat.buf[pool].maxalloc =
            _MAX (((TDESCTYP *)cur_tdesc)->memstat.buf[pool].nalloc, 
                  ((TDESCTYP *)cur_tdesc)->memstat.buf[pool].maxalloc);
        }
*/

        return(bp);
    }
    return NULL;
}

/* Original GTX code, removed when ported to the host

    INLINE

*/

static void
bufp_freebuf (int pool, struct mbuf *bp, char *file, int line) {
    /* Link buffer into free list  */
    bp->m_next = bufpool[pool].free;
    bufpool[pool].free = bp;

    bufpool[pool].nfree++;

/* Original GTX code, removed when ported to the host

    bp->mdbg_ftask = (TDESCTYP *)cur_tdesc;

*/

    bp->mdbg_ffile = file;
    bp->mdbg_fline = line;

    /* Scrub buffer, if we're running in anal mode  */
    if( GDEBUG (DBGBANAL) )
        memset ((char *)bp + sizeof (struct mbuf), BUFSCRUB, bp->m_mxlen);
}

struct mbuf *
__buf_alloc (int len, int priority, char *file, int line) {
    int flags;
    struct mbuf *bp;
    struct BUFPOOL **poolpp;

    /* Disable interrupts  */
    flags = disable_int ();

    bp = NULL;

    /* If a zero-length buffer is requested, must come from first pool  */
    if( len == 0 ) {
        poolpp = &bufplist[0];

        if( (*poolpp != NULL) && ((*poolpp)->dsize == 0) && 
            ((bp = bufp_allocbuf ((*poolpp)->pool, 
                                  priority, file, line)) != NULL) ) {
            (*poolpp)->dmin = 
            ((*poolpp)->dmin < 0)? len : _MIN (len, (*poolpp)->dmin);
            (*poolpp)->dmax = _MAX (len, (*poolpp)->dmax);
        }
    }

    else {
        /* Allocate buffer from the first pool that fits  */
        for( poolpp = &bufplist[0]; *poolpp; poolpp++ ) {
            if( ((*poolpp)->dsize >= len) && 
                ((bp = bufp_allocbuf ((*poolpp)->pool, 
                                      priority, file, line)) != NULL) ) {
                (*poolpp)->dmin = 
                ((*poolpp)->dmin < 0)? len : _MIN (len, (*poolpp)->dmin);
                (*poolpp)->dmax = _MAX (len, (*poolpp)->dmax);

                break;
            }
        }
    }

/* Original GTX code, removed when ported to the host

    if( bp == NULL ) {
        if( cur_tdesc != NULL )
            ((TDESCTYP *)cur_tdesc)->memstat.nallocfail++;
    }
*/

    /* Restore interrupt state  */
    restore_int ();

    return(bp);
}

struct mbuf * 
__buf_allocmax (char *file, int line) {
    int pool, flags, maxlen;
    struct mbuf *bp;

    /* Disable interrupts  */
    flags = disable_int ();

    maxlen = 0;

    /* Find largest available pool  */
    for( pool = 0; (pool < MAX_BUFPOOLS) && (bufpool[pool].nbuf); pool++ ) {
        if( bufpool[pool].free != NULL )
            maxlen = _MAX (maxlen, bufpool[pool].dsize);
    }

    /* Allocate maximum size buffer  */
    bp = (maxlen > 0)? 
         __buf_alloc (maxlen, 0, file, line) : NULL;

    /* Restore interrupt state  */
    restore_int ();

    return(bp);
}

void 
__buf_free (struct mbuf *bp, char *file, int line) {
    int pool, flags;

    /* Disable interrupts  */
    flags = disable_int ();

    pool = bp->m_pool;

    /* Return buffer to pool queue  */
    bp->m_alloc = 0;

    bufpool[pool].nalloc--;

/* Original GTX code, removed when ported to the host

    if( bp->mdbg_atask )
        bp->mdbg_atask->memstat.buf[pool].nalloc--;

*/

    /* Enqueue on buffer free list  */
    bufp_freebuf (pool, bp, file, line);

    /* Restore interrupt state  */
    restore_int ();
}

void
bufp_stats (int pool)
{
    /* Check for valid pool offset  */
    if( pool > MAX_BUFPOOLS ) {
        printf ("Unable to display buffer pool (%d) - Invalid pool number\n", pool);

        return;
    }

    printf("\nPool index: %d, ID: %ld, pool name: %s\n", pool, bufpool[pool].pool, bufpool[pool].name);
    printf("Pool nbuf: %lu, dsize: %lu, thold: %lu\n", bufpool[pool].nbuf, bufpool[pool].dsize, bufpool[pool].thold);
    printf("Pool bsize: %lu, thexc: %lu, nfree: %lu\n", bufpool[pool].bsize, bufpool[pool].thexc, bufpool[pool].nfree);
    printf("Pool nalloc: %lu, maxalloc: %lu, nallocfail: %lu\n", bufpool[pool].nalloc, bufpool[pool].maxalloc, bufpool[pool].nallocfail);
    printf("Pool dmin: %d, dmax: %d\n", bufpool[pool].dmin, bufpool[pool].dmax);

    return;
}
