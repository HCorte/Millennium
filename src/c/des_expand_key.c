/* set_key.c */
/* Copyright (C) 1992 Eric Young - see COPYING for more details */
/* set_key.c v 1.4 eay 24/9/91
 * 1.4 Speed up by 400% :-)
 * 1.3 added register declarations.
 * 1.2 unrolled make_key_sched a bit more
 * 1.1 added norm_expand_bits
 * 1.0 First working version
 */
#include "des_local.h"
#include "podd.h"
#include "sk.h"

static int check_parity();

int des_check_key=0;

void des_set_odd_parity(key)
des_cblock *key;
        {
        int i;

        for (i=0; i<DES_KEY_SZ; i++)
                (*key)[i]=odd_parity[(*key)[i]];
        }

static int check_parity(key)
des_cblock *key;
        {
        int i;

        for (i=0; i<DES_KEY_SZ; i++)
                {
                if ((*key)[i] != odd_parity[(*key)[i]])
                        return(0);
                }
        return(1);
        }

/* Weak and semi week keys as take from
 * %A D.W. Davies
 * %A W.L. Price
 * %T Security for Computer Networks
 * %I John Wiley & Sons
 * %D 1984
 * Many thanks to smb@ulysses.att.com (Steven Bellovin) for the reference
 * (and actual cblock values).
 */
#define NUM_WEAK_KEY    16
static des_cblock weak_keys[NUM_WEAK_KEY]={
        /* weak keys */
        0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,
        0xFE,0xFE,0xFE,0xFE,0xFE,0xFE,0xFE,0xFE,
        0x1F,0x1F,0x1F,0x1F,0x1F,0x1F,0x1F,0x1F,
        0xE0,0xE0,0xE0,0xE0,0xE0,0xE0,0xE0,0xE0,
        /* semi-weak keys */
        0x01,0xFE,0x01,0xFE,0x01,0xFE,0x01,0xFE,
        0xFE,0x01,0xFE,0x01,0xFE,0x01,0xFE,0x01,
        0x1F,0xE0,0x1F,0xE0,0x0E,0xF1,0x0E,0xF1,
        0xE0,0x1F,0xE0,0x1F,0xF1,0x0E,0xF1,0x0E,
        0x01,0xE0,0x01,0xE0,0x01,0xF1,0x01,0xF1,
        0xE0,0x01,0xE0,0x01,0xF1,0x01,0xF1,0x01,
        0x1F,0xFE,0x1F,0xFE,0x0E,0xFE,0x0E,0xFE,
        0xFE,0x1F,0xFE,0x1F,0xFE,0x0E,0xFE,0x0E,
        0x01,0x1F,0x01,0x1F,0x01,0x0E,0x01,0x0E,
        0x1F,0x01,0x1F,0x01,0x0E,0x01,0x0E,0x01,
        0xE0,0xFE,0xE0,0xFE,0xF1,0xFE,0xF1,0xFE,
        0xFE,0xE0,0xFE,0xE0,0xFE,0xF1,0xFE,0xF1};

int des_is_weak_key(key)
des_cblock *key;
        {
        uint *lp;
        register uint l,r;
        int i;

        c2l(key,l);
        c2l(key,r);
        /* the weak_keys bytes should be aligned */
        lp=(uint *)weak_keys;
        for (i=0; i<NUM_WEAK_KEY; i++)
                {
                if ((l == lp[0]) && (r == lp[1]))
                        return(1);
                lp+=2;
                }
        return(0);
        }

/* See ecb_encrypt.c for a pseudo description of these macros. */
#define PERM_OP(a,b,t,n,m) ((t)=((((a)>>(n))^(b))&(m)),\
        (b)^=(t),\
        (a)^=((t)<<(n)))

#define HPERM_OP(a,t,n,m) ((t)=((((a)<<(16-(n)))^(a))&(m)),\
        (a)=(a)^(t)^(t>>(16-(n))))\

static char shifts2[16]={0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,0};

/* return 0 if key parity is odd (correct),
 * return -1 if key parity error,
 * return -2 if illegal weak key.
 */
int des_set_key(key,schedule)
des_cblock *key;
des_key_schedule schedule;
        {
        register uint c,d,t,s;
        register uchar *in;
        register uint *k;
        register int i;

        if (des_check_key)
                {
                if (!check_parity(key))
                        return(-1);

                if (des_is_weak_key(key))
                        return(-2);
                }

        k=(uint *)schedule;
        in=(uchar *)key;

        c2l(in,c);
        c2l(in,d);

        /* do PC1 in 60 simple operations */ 
        PERM_OP(d,c,t,4,0x0f0f0f0f);
        HPERM_OP(c,t,-2, 0xcccc0000);
        HPERM_OP(c,t,-1, 0xaaaa0000);
        HPERM_OP(c,t, 8, 0x00ff0000);
        HPERM_OP(c,t,-1, 0xaaaa0000);
        HPERM_OP(d,t,-8, 0xff000000);
        HPERM_OP(d,t, 8, 0x00ff0000);
        HPERM_OP(d,t, 2, 0x33330000);
        d=((d&0x00aa00aa)<<7)|((d&0x55005500)>>7)|(d&0xaa55aa55);
        d=(d>>8)|((c&0xf0000000)>>4);
        c&=0x0fffffff;

        for (i=0; i<ITERATIONS; i++)
                {
                if (shifts2[i])
                        { c=((c>>2)|(c<<26)); d=((d>>2)|(d<<26)); }
                else
                        { c=((c>>1)|(c<<27)); d=((d>>1)|(d<<27)); }
                c&=0x0fffffff;
                d&=0x0fffffff;
                /* could be a few less shifts but I am to lazy at this
                 * point in time to investigate */
                s=      des_skb[0][ (c    )&0x3f                ]|
                        des_skb[1][((c>> 6)&0x03)|((c>> 7)&0x3c)]|
                        des_skb[2][((c>>13)&0x0f)|((c>>14)&0x30)]|
                        des_skb[3][((c>>20)&0x01)|((c>>21)&0x06) |
                                              ((c>>22)&0x38)];
                t=      des_skb[4][ (d    )&0x3f                ]|
                        des_skb[5][((d>> 7)&0x03)|((d>> 8)&0x3c)]|
                        des_skb[6][ (d>>15)&0x3f                ]|
                        des_skb[7][((d>>21)&0x0f)|((d>>22)&0x30)];

                /* table contained 0213 4657 */
                *(k++)=((t<<16)|(s&0x0000ffff));
                s=     ((s>>16)|(t&0xffff0000));
                
                s=(s<<4)|(s>>28);
                *(k++)=s;
                }
        return(0);
        }

int des_expand_key(des_cblock *key, des_key_schedule schedule)
        {

        /* Do NOT check for parity or weak key */
        des_check_key = 0;

        return(des_set_key(key,schedule));
        }
