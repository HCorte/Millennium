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
/*====[TNI_READ_FROM_CONNECTION.C]============================================*/
/*                                                                            */
/* Read_From_Connection (unsigned short conn)                                 */
/*                                                                            */
/* Purpose: This function reads an integral number of whole TNI PDUs from the */
/*          input connection and returns a pointer to an mbuf chain           */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          (struct mbuf *)  Pointer to an mbuf chain                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_READ_FROM_CONNECTION.C]============================================*/
/*                                                                            */

#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "includes_mbuf.h"

int Protocol_Valid (unsigned char *pdu_header,
                    unsigned short conn)
{
    int                 act_conn;          /* Actual connection PDU was sent  */
    int                 rc = P_SUCCESS;
    int                 ver_idx = 0;
    int                 found_version = 0;

    unsigned char       proto_id;
    unsigned char       proto_version;

    err_string = null_err_string;

    proto_id = *(pdu_header + proto_off);
    proto_version = *(pdu_header + version_off);

    if (proto_id != TNI_PROTOCOL)
    {
        rc = P_FAILURE;

        sprintf (err_string.par1, "%d", conn);
        sprintf (err_string.par2, "%x", TNI_PROTOCOL);
        sprintf (err_string.par3, "%2.2x", proto_id);

        output_err ("Protocol_Valid",
                    MI_TNI_INV_PROTO_ID,
                    MX_ERR_LVL_WARNING,
                    err_string);
    }
    else
    {

/* If the TNI protocol version has not been determined for this connection    */
/* yet, then verify the received TNI protocol version is supported.  If the   */
/* TNI protocol version is determined to be supported, then assigned it to the*/
/* connection.  Once a connection has been assigned to a TNI protocol version */
/* verify all received PDUs on that connection have the assgined TNI protocol */
/* version.                                                                   */

        if (tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_UNKNOWN)
        {
            while ((ver_idx < NUMBER_OF_TNI_VERSIONS) && 
                   (!found_version))
            {
                if (proto_version == tni_proto_versions[ver_idx])
                {
                    tnicon_p->connection[conn].tni_proto_ver = proto_version;
                    found_version = 1;

/* Assign managed by field if the connection is a temporary one               */

                    if (conn > MAX_CONFIGURABLE_CONN)
                    {
                        switch (proto_version)
                        {
                        case TNI_VERSION_23:
                        case TNI_VERSION_22:
                        case TNI_VERSION_21:

                            tnicon_p->connection[conn].managed_by = CONNS_APP_NAME;
                            break;
                        
                        default:

                            tnicon_p->connection[conn].managed_by = CONNS_CLIENT_ID;
                            break;
                        }
                    }
                }
                ver_idx++;
            }

            if (!found_version)
            {
                sprintf (err_string.par2,
                         "%d.%d",
                         (DEFAULT_TNI_VERSION & 0xf0) >> 4,
                         (DEFAULT_TNI_VERSION & 0x0f));
            }
        }
        else
        {
            if (proto_version == tnicon_p->connection[conn].tni_proto_ver)
            {
                found_version = 1;
            }
            else
            {
                sprintf (err_string.par2,
                         "%d.%d",
                         (tnicon_p->connection[conn].tni_proto_ver & 0xf0) >> 4,
                         (tnicon_p->connection[conn].tni_proto_ver & 0x0f));
            }
        }

        if (!found_version)
        {
            rc = P_FAILURE;

            sprintf (err_string.par1, "%d", conn);

            sprintf (err_string.par3,
                     "%d.%d",
                     (proto_version & 0xf0) >> 4,
                     (proto_version & 0x0f));

            output_err ("Protocol_Valid",
                        MI_TNI_INV_PROTO_VER,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }

    return (rc);
}

void Inv_Pdu_Hdr_Err (struct mbuf *mbuf_chain,
                      unsigned short conn) {

    int rc = P_SUCCESS;                  /* generic return code               */

    DumpMbufChain(mbuf_chain, conn);

    rc = send_invalid_pdu_error (conn);

    if (rc == P_SUCCESS)
    {
       Close_Connection (conn); 
    }

    return;
}

void Inv_Pdu_Len_Err (struct mbuf *mbuf_chain,
                      unsigned short conn) {

    int rc = P_SUCCESS;                    /* generic return code             */

    err_string = null_err_string;

    sprintf(err_string.par1, "%d", conn);

    output_err("Inv_Pdu_Len_Err",
               MI_TNI_INV_PDU_LEN,
               MX_ERR_LVL_ERROR,
               err_string);

    DumpMbufChain(mbuf_chain, conn);

    rc = send_invalid_pdu_error (conn);

    return;
}

struct mbuf *Read_From_Connection (unsigned short conn) {
    int read_len;                        /* number of bytes read in first     */
                                         /* transfer                          */
    int hdr_readlen;                     /* amount we have to read to         */
                                         /* complete a PDU fixed header       */
    int var_readlen;                     /* amount we have to read to         */
                                         /* complete a PDU, excluding         */
                                         /* the size of the fixed header      */
    int act_readlen;                     /* amount we actually read when      */
                                         /* trying to complete a split PDU    */
    int i;                               /* byte count index                  */

    unsigned short pdulen;               /* size of the PDU based upon the    */
                                         /* length in the PDU fixed header    */
    unsigned short remainderlen;         /* number of bytes "to go" when      */
                                         /* determining PDU boundaries in     */
                                         /* initial transfer                  */

    struct mbuf *mymbuf;                 /* local mbuf pointer                */
    struct mbuf *tcpchain;               /* chain of mbufs for holding TCP    */
                                         /* transfer data.  The mbufs in      */
                                         /* this chain are duplicated in      */
                                         /* the PDU chain to minimize the     */
                                         /* number of copy operations needed  */
    struct mbuf *remainderchain;         /* chain of mbufs for holding each   */
                                         /* any partial PDU.  If the          */
                                         /* remainder chain is used, it is    */
                                         /* appended to the tcp chain prior   */
                                         /* to duplicating the mbufs for PDU  */
                                         /* processing                        */
    unsigned char *mbuf_data_ptr;        /* pointer to tcpchain data area     */
    unsigned char *mbuf_rem_data_ptr;    /* pointer to remainder chain data   */
                                         /* area                              */


    unsigned char *mbuf_dbg_data_ptr;        /* debug pointer to tcpchain data area */

    TNICON *ptr;                             /* pointer to global section info      */

    err_string = null_err_string;

    ptr = tnicon_p;

/* Do an initial read from the connection                                    */

    tcpchain = NULLBUF;
    read_len = Read_From_Socket (conn, &mymbuf, max_pdu_size_val);

    if( read_len > 0 ) {

        mb_append (&tcpchain, mymbuf);

/*     Get pointer to the data just moved into the tcp mbuf chain          */

        mbuf_data_ptr = mymbuf->m_data;
        mbuf_dbg_data_ptr = mbuf_data_ptr;

/*     Initialize variables for handling a split PDU at the end of the     */
/*     data area                                                           */

        hdr_readlen = 0;
        var_readlen = 0;

        remainderlen = read_len;
        remainderchain = NULLBUF;            /* init partial PDU chain head */

        for( i = 0; i < read_len; i+=pdulen ) {
            if( (remainderlen > 0) &&
                (remainderlen < sizeof(struct TNI_FIXED_HDR)) ) {

/*             To avoid having a PDU split across mbufs, move the          */
/*             remaining bytes in this mbuf's data area to a new, separate */
/*             mbuf.  Use maximum transfer size to avoid possible copy in  */
/*             subsequent mb_gather call.                                  */

                if( (mymbuf = mb_alloc (max_pdu_size_val)) == NULL ) {

                    sprintf(err_string.par1,"data mbuf");
                    sprintf(err_string.par2,"%d",max_pdu_size_val);

                    output_err("Read_From_Connection",
                               MI_TNI_MB_ALLOC,
                               MX_ERR_LVL_ERROR,
                               err_string);

                    mb_free_p(tcpchain);
                    return(NULL);
                }

                memcpy (mymbuf->m_data, mbuf_data_ptr+i, remainderlen);
                mymbuf->m_len = remainderlen;
                mb_append (&remainderchain, mymbuf);
                tcpchain->m_len -= remainderlen;

/*             We have an incomplete fixed header.  Read enough data to    */
/*             complete the TNI fixed header                               */

                do {
                    hdr_readlen = (unsigned short)sizeof(struct TNI_FIXED_HDR) -
                                  remainderlen;

                    act_readlen = Read_From_Socket (conn,
                                                    &mymbuf,
                                                    hdr_readlen);
                    if( act_readlen <= 0 ) {
/*                 read error      */
                        DumpMbufChain(tcpchain, conn);

                        mb_free_p(tcpchain);
                        mb_free_p(remainderchain);
                        return(NULL);
                    }

                    mb_append (&remainderchain, mymbuf);
                    remainderlen+=act_readlen;

                } while( act_readlen != hdr_readlen );

/*             Gather the entire fixed header into a single mbuf to make   */
/*             PDU length determination easier                             */
                if( (mymbuf = mb_gather (remainderchain)) == NULL ) {

                    sprintf(err_string.par1,
                            "fixed header  remainderchain");
                    sprintf(err_string.par2,
                            "Read_From_Connection");

                    output_err("Read_From_Connection",
                               MI_TNI_MB_GATHER,
                               MX_ERR_LVL_ERROR,
                               err_string);

                    mb_free_p(tcpchain);
                    return(NULL);
                }

                remainderchain = mymbuf;
                mbuf_rem_data_ptr = remainderchain->m_data;

                if (Protocol_Valid (mbuf_rem_data_ptr, conn)) {

                    pdulen = ntohs(*(unsigned short int *)
                                   (mbuf_rem_data_ptr+pdu_len_off));

                    if (pdulen == 0) {

                        Inv_Pdu_Len_Err(remainderchain,conn);

                        mb_free_p(remainderchain);
                        mb_free_p(tcpchain);
                        return(NULL);
                    }
                } else {

                    Inv_Pdu_Hdr_Err(remainderchain,conn);

                    mb_free_p(remainderchain);
                    mb_free_p(tcpchain);
                    return(NULL);
                }
            }                                   /* end of if remainderlen... */

            else if( remainderlen == sizeof(struct TNI_FIXED_HDR) ) {

/*              we have only the fixed header                                 */

                if (Protocol_Valid (mbuf_data_ptr, conn)) {

                    pdulen = ntohs(*(unsigned short int *)
                                   (mbuf_data_ptr+i+pdu_len_off));

                    if (pdulen == 0) {

                        Inv_Pdu_Len_Err(tcpchain,conn);

                        mb_free_p(tcpchain);
                        return(NULL);
                    }
                } else {

                    Inv_Pdu_Hdr_Err(tcpchain,conn);

                    mb_free_p(tcpchain);
                    return(NULL);
                }
            }

            else {

/*              we have at least the fixed header                             */

                if (Protocol_Valid (mbuf_data_ptr, conn)) {

                    pdulen = ntohs(*(unsigned short int *)
                                   (mbuf_data_ptr+i+pdu_len_off));

                    if (pdulen == 0) {

                        Inv_Pdu_Len_Err(tcpchain,conn);

                        mb_free_p(tcpchain);
                        return(NULL);
                    }
                } else {

                    Inv_Pdu_Hdr_Err(tcpchain,conn);

                    mb_free_p(tcpchain);
                    return(NULL);
                }
            }

/*          If we don't have the entire PDU, read enough to complete PDU      */

            if( pdulen > (remainderlen + hdr_readlen) ) {

/*            Pull the remaining data from the tcp buffer if we haven't       */
/*            already done so (i.e. hdr_readlen = 0)                          */

                if( hdr_readlen == 0 ) {

/*               To avoid having a PDU split across mbufs, move the          */
/*               remaining bytes in this mbuf's data area to a new, separate */
/*               mbuf.  Use maximum transfer size to avoid possible copy in  */
/*               subsequent mb_gather call.                                  */

                    if( (mymbuf = mb_alloc (max_pdu_size_val)) == NULL ) {

                        sprintf(err_string.par1,"data mbuf");
                        sprintf(err_string.par2,"%d",max_pdu_size_val);

                        output_err("Read_From_Connection",
                                   MI_TNI_MB_ALLOC,
                                   MX_ERR_LVL_ERROR,
                                   err_string);

                        mb_free_p(tcpchain);
                        return(NULL);
                    }

                    memcpy (mymbuf->m_data, mbuf_data_ptr+i, remainderlen);
                    mymbuf->m_len = remainderlen;
                    mb_append (&remainderchain, mymbuf);
                    tcpchain->m_len -= remainderlen;

                }

                do {
                    var_readlen = pdulen - remainderlen;

                    act_readlen = Read_From_Socket (conn, 
                                                    &mymbuf, 
                                                    var_readlen);

                    if( act_readlen <= 0 ) {

                        DumpMbufChain(tcpchain, conn);
                        mb_free_p(tcpchain);
                        mb_free_p(remainderchain);
                        return(NULL);
                    }

                    mb_append (&remainderchain, mymbuf);
                    remainderlen+=act_readlen;                    

                } while( act_readlen != var_readlen );

                if( (mymbuf = mb_gather (remainderchain)) == NULL ) {

                    sprintf(err_string.par1,
                            "split PDU remainderchain");
                    sprintf(err_string.par2,
                            "Read_From_Connection");

                    output_err("Read_From_Connection",
                               MI_TNI_MB_GATHER,
                               MX_ERR_LVL_ERROR,
                               err_string);

                    mb_free_p(tcpchain);
                    return(NULL);
                }
                remainderchain = mymbuf;
            }

/*        At this point, remainder chain will either point to the gathered */
/*        mbuf from the remainder chain or be NULL.  In either case,       */
/*        append the remainder chain to the tcp chain so the tcp chain     */
/*        will contain only whole PDUs.                                    */

            mb_append (&tcpchain, remainderchain);
            remainderlen -= pdulen;

        }                                            /* end for loop          */

    }                                                /* end if read_len > 0   */


    return(tcpchain);
}
