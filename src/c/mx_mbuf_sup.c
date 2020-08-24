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
/* Copyright 2003 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[MX_MBUF_SUP.C]=========================================================*/
/*                                                                            */
/* Purpose: These functions support the MBUF implementation.                  */
/*                                                                            */
/* Functions:                                                                 */
/*          Duplicate_Chain (struct mbuf *inputchain, unsigned short conn)    */
/*          Init_Write_Chain ()                                               */
/*                                                                            */
/*====[MX_MBUF_SUP.C]=========================================================*/
/*                                                                            */

#include <stdlib.h>
#include <stdio.h>

#include "includes_mbuf.h"

/*============================================================================*/
/*                                                                            */
/* Duplicate_Chain (struct mbuf *inputchain, unsigned short conn)             */
/*                                                                            */
/* Purpose: This function duplicates each PDU in the input chain as a separate*/
/*          PDU in the output chain.  It is assumed that each mbuf in the     */
/*          input chain will contain only whole PDU(s), i.e. no PDU is split  */
/*          across mbuf boundaries.  However,it is permissible for a single   */
/*          mbuf in the input chain to contain multiple, whole PDUs. If any   */
/*          errors are encountered while processing a PDU, then processing    */
/*          terminates and those PDUs which have been processed prior to the  */
/*          error are returned in the duplicate mbuf chain.                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          inputchain:      pointer to input chain to be duplicated          */
/*                                                                            */
/*          conn             Connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          dupchain         pointer to duplicate mbuf chain (may be NULL)    */
/*                                                                            */
/* Assumptions: Each mbuf in the input chain will contain only whole PDU(s),  */
/*              i.e. no PDU is split across mbuf boundaries.                  */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

struct mbuf *Duplicate_Chain (struct mbuf *inputchain, unsigned short conn) {

    int offset;                            /* Offset into data area pointed to*/
                                           /* by mbuf_data_ptr. Used to keep  */
                                           /* track of PDU boundaries in the  */
                                           /* mbuf data area.                 */
    int i;
    int rc = P_SUCCESS;                    /* generic return code             */

    unsigned short duplen;                 /* number of bytes duplicated,     */
                                           /* returned by mb_dup_p            */
    unsigned short mbuf_data_size;         /* total number of bytes in the    */
                                           /* current mbuf; only used for     */
                                           /* checking remainderlen           */
    unsigned short pdulen;                 /* number of bytes in PDU, also    */
                                           /* tells mb_dup_p how many bytes to*/
                                           /* duplicate                       */
    unsigned short remainderlen;           /* Count of the number of bytes    */
                                           /* remaining to be processed in a  */
                                           /* single mbuf.  Used to determine */
                                           /* when we are done processing the */
                                           /* current mbuf                    */
    unsigned short totlen;                 /* total number of data bytes in   */
                                           /* the input mbuf chain            */

    unsigned char *mbuf_data_ptr;          /* pointer currentmbuf data area   */

    struct mbuf *currentmbuf;              /* pointer to input mbuf we're     */
                                           /* currently processing.           */
    struct mbuf *dupchain;                 /* Returned chain of mbufs for     */
                                           /* holding each PDU to be processed*/
                                           /* We assume that each mbuf        */
                                           /* contains 1 entire PDU           */
    struct mbuf *dupmbuf;                  /* intermediate pointer for        */
                                           /* duplicating mbufs               */

/* Get total number of data bytes in the input mbuf chain                     */

    totlen = mb_len_p (inputchain);

    if( totlen == 0 ) {
        printf ("Attempting dup operation on 0 length input chain \n");
        return(NULL);
    }

/* currentmbuf will always point to the mbuf we're currently processing in    */
/* the chain.  Start with the head of the input mbuf chain                    */

    currentmbuf = inputchain;

/* Set data pointer to the data area and data area length of the first mbuf   */
/* at the head of the input mbuf chain                                        */

    mbuf_data_ptr = currentmbuf->m_data;
    mbuf_data_size = currentmbuf->m_len;
    remainderlen = currentmbuf->m_len;

/* Initialize running index into current mbuf's data area                     */

    offset = 0;

    dupchain = NULLBUF;                    /* initialize returned chain head  */
    for( i = 0; i < totlen; i+=pdulen ) {  /* i will ALWAYS be data offset    */
                                           /* within the entire input chain   */
        if( remainderlen == 0 ) {

/*          There is no more data in the current input mbuf.  Go to next mbuf */
/*          in chain                                                          */

            currentmbuf = currentmbuf->m_next;
            mbuf_data_ptr = currentmbuf->m_data;
            mbuf_data_size = currentmbuf->m_len;
            remainderlen = currentmbuf->m_len;
            offset=0;
        }

        else if( (remainderlen > 0) && (remainderlen <= mbuf_data_size) ) {
/*          Continue processing current mbuf                                  */
        }

        else {

/*          Remainderlen should never be less than 0 or greater than the size */
/*          of the current mbuf's data area                                   */

            printf ("Invalid remainder %d during dup operation",remainderlen);
            return(dupchain);
        }

/*      Verify protocol and version id.  If these are OK, then set PDU length */
/*      for duplication.                                                      */

        if( *(mbuf_data_ptr+offset+proto_off) == TNI_PROTOCOL &&
            *(mbuf_data_ptr+offset+version_off) == 
            tnicon_p->connection[conn].tni_proto_ver) {

            pdulen = ntohs(*(unsigned short int *)
                           (mbuf_data_ptr+offset+pdu_len_off));

/*          Update offset and remainder for next iteration                    */

            offset += pdulen;
            remainderlen -= pdulen;
        } else {
            rc = send_invalid_pdu_error (conn);

            return(dupchain);
        }

/*      Now that we have the PDU length, duplicate the data                   */

        dupmbuf = NULLBUF;
        duplen = mb_dup_p (&dupmbuf,inputchain,i,pdulen);

        if( duplen == 0 ) {
            printf ("Bad mbuf dup operation \n");
            return(dupchain);
        } else {
            mb_append (&dupchain, dupmbuf);
        }
    }                                                           /* end of for */

    return(dupchain);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Init_Write_Chain ()                                                        */
/*                                                                            */
/* Purpose: This function initializes the mbuf write chain                    */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          (struct mbuf *)  Pointer to a NULL mbuf structure                 */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

struct mbuf *Init_Write_Chain ()
{
   struct mbuf *mymbuf;

   mymbuf = NULLBUF;
   return (NULL);

}
