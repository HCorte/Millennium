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
/* Copyright 2007 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[MBUF_TEMP_PROTO.H]=====================================================*/
/*                                                                            */
/* Purpose: This header file contains the mbuf library prototypes             */ 
/*                                                                            */
/*====[MBUF_TEMP_PROTO.H]=====================================================*/
/*                                                                            */

#ifndef MBUF_TEMP_PROTO_H
#define MBUF_TEMP_PROTO_H
/* bufpool.c */
void bufp_mem(void);
int bufp_config(int pool, struct BUFPOOL *bfp);
void bufp_init(void);
struct mbuf *__buf_alloc(int len, int priority, char *file, int line);
struct mbuf *__buf_allocmax(char *file, int line);
void __buf_free(struct mbuf *bp, char *file, int line);

/* mballoc.c */
int __mb_sanitychk(struct mbuf *bp);
void __mb_assert(struct mbuf *bp, char *file, int line);
int __mb_sanitychk_p(struct mbuf *bp);
void __mb_assert_p(struct mbuf *bp, char *file, int line);
struct mbuf *__mb_alloc(register unsigned short size, char *file, int line);
struct mbuf *__mb_alloc_wait(register unsigned short size, char *file, int line);
struct mbuf *__mb_alloc_pri(register unsigned short size, char *file, int line);
struct mbuf *__mb_alloc_max(char *file, int line);
struct mbuf *__mb_free(register struct mbuf *bp, char *file, int line);
unsigned short __mb_pullup(struct mbuf **bpp, void *bufp, unsigned short cnt, char *file, int line);
struct mbuf *__mb_pushdown(register struct mbuf *bp, unsigned short size, char *file, int line);
struct mbuf *__mb_pushcpy(register struct mbuf *bp, unsigned short size, void *bufp, char *file, int line);

/* mbpacket.c */
struct mbuf *__mb_free_p(register struct mbuf *bp, char *file, int line);
void __mb_append(struct mbuf **bpp, register struct mbuf *bp, char *file, int line);
void __mb_crunch(struct mbuf **bpp, char *file, int line);
struct mbuf *__mb_gather(struct mbuf *bp, char *file, int line);
void __mb_pad_p(struct mbuf **bpp, unsigned short len, char c, char *file, int line);
unsigned short __mb_len_p(register struct mbuf *bp, char *file, int line);
struct mbuf *__mb_copy_p(register struct mbuf *bp, register unsigned short cnt, char *file, int line);
unsigned short __mb_dup_p(struct mbuf **bpp, register struct mbuf *bp, register unsigned short offset, unsigned short cnt, char *file, int line);

/* mbqueue.c */
void __mb_free_q(struct mbuf **q, char *file, int line);
unsigned short __mb_len_q(register struct mbuf *q);
void __mb_enqueue(struct mbuf **q, struct mbuf *bp);
struct mbuf *__mb_dequeue(register struct mbuf **q);
struct mbuf *__mb_qdata(char *data, unsigned short cnt, char *file, int line);
unsigned short __mb_dqdata(struct mbuf *bp, char *buf, unsigned short cnt, char *file, int line);

/* mbuf.c */
void __mb_trim(struct mbuf **bpp, unsigned short length, char *file, int line);
int __mb_pullchar(struct mbuf **bpp, char *file, int line);
long __mb_pull16(struct mbuf **bpp, char *file, int line);
long __mb_pull32(struct mbuf **bpp, char *file, int line);
unsigned short __mb_get16(register char *cp);
long __mb_get32(register char *cp);
char *__mb_put16(register char *cp, unsigned short x);
char *__mb_put32(register char *cp, long x);
void __mb_dump(unsigned char sev, char *str, struct mbuf *bp);
void __mb_hexdump(unsigned char sev, char *str, struct mbuf *bp, char *file, int line);
char __mb_getchar(struct mbuf *bp, unsigned int index);
void __mb_putchar(struct mbuf *bp, char ch, unsigned int index);

/* mbuf_stubs.c */
int disable_int(void);
void hexdump(unsigned char sev, char *str, void *p, int nbytes, int cont);
void tm_wkafter(int value);
void FATAL(int value);
void UFATAL(int value);
void log_start(void);
void log_stop(void);
void logger(int logtype, unsigned char sev, const char *format, void *argp);
int restore_int(void);

#endif
