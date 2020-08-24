
static const char  *fileid = "";

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
/*====[TNI_BUILD_SERVER_CHALLENGE_RQST_PDU.C]=================================*/
/*                                                                            */
/* Server_Challenge_Request( int conn )                                       */
/*                                                                            */
/* Purpose: This function builds a Server Challenge Request                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_BUILD_SERVER_CHALLENGE_RQST_PDU.C]=================================*/
/*                                                                            */

#include <stdio.h>
#include "includes_mbuf.h"

#if defined(PROSYS_ENV_PLATFORM)
#include "g_pswd.h"
#else

#endif

void
Server_Challenge_Request(int conn)
{
    int                 app_num;
    char                username[256];
    char                password[256];
    unsigned char       salt[MAX_AUTH_SALT_LENGTH];
    unsigned char       msg[MAX_AUTH_SALT_LENGTH + 256];
    unsigned char      *msg_p;
    int                 msglen;
    unsigned char       md_value[MAX_AUTH_HASH_LENGTH];
    unsigned int        md_len;
    int                 rstat;
    struct mbuf        *mymbuf;
    struct mbuf        *salt_data_mbuf;
    unsigned char      *data_buf;
    int                 mlen;
    int                 param_count;
    int                 act_conn;
    struct TNI_PARAM_PAIR params[3];

    err_string = null_err_string;
    app_num = tnicon_p->connection[conn].app_idx;
    rstat = P_FAILURE;
    memset(username,0x00,sizeof(username));
    memset(password,0x00,sizeof(password));


    if (tnicon_p->app[app_num].auth_required == 1)
    {
        if(tnicon_p->app[app_num].auth_lockout_state == APP_AUTH_NOTLOCKED )
        {
#if defined(PROSYS_ENV_PLATFORM)
            G_ERROR      error;

            /* Retrieve password */
            rstat = g_pswd_retrieve(tnicon_p->app[app_num].name, username, password, &error);
#endif
            if (rstat != P_SUCCESS)
            {
                sprintf(err_string.par1, "%s", tnicon_p->app[app_num].name);
                sprintf(err_string.par2, "%d", conn);
                output_err("Server_Challenge_Request", MI_TNI_ERR_PSWD_RTRV, MX_ERR_LVL_ERROR, err_string);

                Close_Connection(conn);

                return;
            }

            /* Generate Salt */
            Generate_Salt_Data(salt, tnicon_p->app[app_num].auth_salt_length);

            /* Generate Hash */
            msg_p = msg;
            memcpy(msg_p, salt, tnicon_p->app[app_num].auth_salt_length);
            msg_p += tnicon_p->app[app_num].auth_salt_length;
            memcpy(msg_p, password, strlen(password));
            msg_p += strlen(password);
            msglen = msg_p - msg;

            memset(md_value, 0x00, MAX_AUTH_HASH_LENGTH);
            if (Generate_Msg_Digest(tnicon_p->app[app_num].auth_hash_algorithm, msg, msglen, md_value, &md_len) != 1)
            {
                sprintf(err_string.par1, "%s", tnicon_p->app[app_num].name);
                sprintf(err_string.par2, "%d", conn);

                output_err("Server_Challenge_Request", MI_TNI_ERR_HASH_GEN, MX_ERR_LVL_ERROR, err_string);

                Close_Connection(conn);

                return;

            }

            /* Build Salt data mbuf */
            mlen = tnicon_p->app[app_num].auth_salt_length;
            if ((salt_data_mbuf = mb_alloc(mlen)) == NULL)
            {
                sprintf(err_string.par1, "Salt Data");
                sprintf(err_string.par2, "%d", mlen);
                sprintf(err_string.par3, "Server_Challenge_Request");

                output_err("Server_Challenge_Request", MI_TNI_MB_ALLOC, MX_ERR_LVL_ERROR, err_string);

                return;
            }

            data_buf = (unsigned char *) salt_data_mbuf->m_data;
            memcpy(data_buf, salt, tnicon_p->app[app_num].auth_salt_length);

            salt_data_mbuf->m_len = tnicon_p->app[app_num].auth_salt_length;

            /* Build Request */

            param_count = 0;

            params[param_count].param_code = HASH_ALGORITHM;
            params[param_count].param_value = tnicon_p->app[app_num].auth_hash_algorithm;
            param_count++;

            params[param_count].param_code = SALT_LENGTH;
            params[param_count].param_value = tnicon_p->app[app_num].auth_salt_length;
            param_count++;

            params[param_count].param_code = 0;             /* NULL terminated list */
            params[param_count].param_value = 0;

            mymbuf = Build_Pdu(conn, SRV_CHALLENGE_RQST_PDU, salt_data_mbuf, &params[0]);

        /* Send response on the same connection on which CLIENT REQUEST PDU   */
        /* was received                                                       */

            act_conn = Send_To_Conn(mymbuf, conn, NO_ALT_CONN);

            if (act_conn == 0)
            {

                sprintf(err_string.par1, "Server Challenge Request");
                sprintf(err_string.par2, "%d", conn);

                output_err("Server_Challenge_Request", MI_TNI_ERR_SEND, MX_ERR_LVL_ERROR, err_string);
            }
            else
            {

                tnicon_p->connection[conn].auth_state = AUTH_ATTEMPTED;
                tnicon_p->connection[conn].last_auth_atmpt_time = current_time;
                tnicon_p->srv_challenge_req_cnt_tot[act_conn]++;
                /* update hash in TNICON */
                tnicon_p->connection[conn].auth_hash_len = md_len;
                memcpy(tnicon_p->connection[conn].auth_hash_value, md_value, md_len);
            }
        }
        else if(tnicon_p->app[app_num].auth_lockout_state == APP_AUTH_LOCKED)
        {
                tnicon_p->connection[conn].auth_state = AUTH_NOTATTEMPTED_LOCKED;
        }
    }
}

/* ===========================================================================*/
/* Build_Serv_Challenge_Rqst_Pdu(unsigned short conn,                         */
/*                               struct mbuf *inputchain,                     */
/*                               struct TNI_PARAM_PAIR *params)               */
/*                                                                            */
/* Purpose: This function constructs an mbuf chain containing a single        */
/*          TNI server challenge PDU. Two separate mbufs are constructed and  */
/*          chained together to form a single chain.  The first mbuf created  */
/*          contains the variable part of the PDU based upon the input        */
/*          parameters.  The second mbuf created contains the fixed header    */
/*          of the PDU.  The mbuf containing the variable part                */
/*          of the PDU is then appended to the mbuf containing the fixed part */
/*          of the PDU to form an mbuf chain.  Finally the input chain is     */
/*          appended to the mbuf chain to form the returned mbuf chain.       */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection the PDU is to be sent on              */
/*                                                                            */
/*          inputchain       Pointer to an mbuf chain consisting of           */
/*                           salt data                                        */
/*                                                                            */
/*                           ---------------------------------                */
/*          inputchain -->   | Salt data                     |                */
/*                           | inmbuf->m_next=NULL           |                */
/*                           ---------------------------------                */
/*                                                                            */
/*          params           Pointer to a structure containing parameter      */
/*                           code/value pairs.  The list is terminated with a */
/*                           parameter code of zero.                          */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          (struct mbuf *)  Pointer to the mbuf chain containing the single  */
/*                           server challenge PDU                             */
/*                                                                            */
/*                           ---------------------------------                */
/*       (struct mbuf *) --> | Fixed part of PDU             |                */
/*                           | fixed_hdr_mbuf->m_next        | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------                */
/*                      |--> | Variable part of PDU          |                */
/*                           | var_hdr_mbuf->m_next          | --|            */
/*                           ---------------------------------   |            */
/*                      ------------------------------------------            */
/*                      |    ---------------------------------------          */
/*     (inputchain) --> |--> | Salt data                            |         */
/*                           | var_hdr_mbuf->m_next->m_next = NULL  |         */
/*                           ---------------------------------------          */
/* Assumptions: inputchain is not NULL                                        */
/* ===========================================================================*/

struct mbuf        *
Build_Serv_Challenge_Rqst_Pdu(unsigned short conn, struct mbuf *inmbuf, struct TNI_PARAM_PAIR *params)
{
    int                 mlen;                       /* length variable                   */
    struct mbuf        *fixed_hdr_mbuf;             /* pointer to mbuf containing fixed  */
                                                    /* header part of Error PDU          */
    struct mbuf        *var_hdr_mbuf;               /* pointer to mbuf containing        */
                                                    /* variable part of Error PDU        */
    struct TNI_FIXED_HDR *fixed_buf;                /* pointer to data area of           */
                                                    /* fixed_hdr_mbuf                    */
    char               *var_buf;                    /* pointer to data area of           */
                                                    /* var_hdr_mbuf                      */

    err_string = null_err_string;
/* Determine the maximum size of the fixed and variable TNI REQUEST PDU       */
/* header so we can allocate an mbuf of the correct size.  The returned value */
/* is larger than we need for the variable part only, but it's close enough.  */


    mlen = Calc_Pdu_Hdr_Size(SRV_CHALLENGE_RQST_PDU);

    if ((var_hdr_mbuf = mb_alloc(mlen)) == NULL)
    {

        sprintf(err_string.par1, "variable data");
        sprintf(err_string.par2, "%d", mlen);
        sprintf(err_string.par3, "Build_Serv_Challenge_Rqst_Pdu");

        output_err("Build_Serv_Challenge_Rqst_Pdu", MI_TNI_MB_ALLOC, MX_ERR_LVL_ERROR, err_string);

        return (NULL);
    }

/* Build Server Request PDU variable header first to make the total PDU       */
/* length easier to calcuate for the Server Request PDU fixed header          */

    var_buf = (char *) var_hdr_mbuf->m_data;

    var_hdr_mbuf->m_len = 0;

    while (params->param_code)
    {                                               /* parameter list is 0 terminated    */

        mlen = Build_Parameter(params->param_code, params->param_value, params->param_value_char, (struct TNI_DATA *) var_buf);
        var_hdr_mbuf->m_len += mlen;
        var_buf += mlen;
        params++;
    }

/* Determine the maximum size of the fixed and variable Server Request PDU    */
/* header so we can allocate an mbuf of the correct size.  The returned value */
/* is larger than we need for the fixed part only, but it's close enough.     */

    mlen = Calc_Pdu_Hdr_Size(SRV_CHALLENGE_RQST_PDU);

    if ((fixed_hdr_mbuf = mb_alloc(mlen)) == NULL)
    {

        sprintf(err_string.par1, "fixed header");
        sprintf(err_string.par2, "%d", mlen);
        sprintf(err_string.par3, "Build_Serv_Challenge_Rqst_Pdu");

        output_err("Build_Serv_Challenge_Rqst_Pdu", MI_TNI_MB_ALLOC, MX_ERR_LVL_ERROR, err_string);

        mb_free_p(var_hdr_mbuf);                    /* free previously allocated mbuf    */
        return (NULL);
    }

/* Build Server Request PDU fixed header                                      */

    fixed_buf = (struct TNI_FIXED_HDR *) fixed_hdr_mbuf->m_data;
    fixed_buf->proto_id = TNI_PROTOCOL;

/* If the TNI protocol version is unknown then set the protocol version to    */
/* the default verson                                                         */

    if (tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_UNKNOWN)
    {
        fixed_buf->version_id = DEFAULT_TNI_VERSION;
    }
    else
    {
        fixed_buf->version_id = tnicon_p->connection[conn].tni_proto_ver;
    }

    fixed_buf->pdu_type = SRV_CHALLENGE_RQST_PDU;
    fixed_hdr_mbuf->m_len = sizeof(struct TNI_FIXED_HDR);

    fixed_buf->pdu_length = fixed_hdr_mbuf->m_len + var_hdr_mbuf->m_len + inmbuf->m_len;
    fixed_buf->pdu_length = htons(fixed_buf->pdu_length);

/* Now append the 2 mbuf chains - fixed header and variable header            */

    mb_append(&fixed_hdr_mbuf, var_hdr_mbuf);
    mb_append(&fixed_hdr_mbuf, inmbuf);

    if ((tnicon_p->dbg_state == DBG_ACTIVE) && (tnicon_p->print_flag & PDU_LEVEL_DBG))
    {
        fprintf(tnicon_p->dbg_p, "\nBuilt server challenge request PDU");
    }

    return (fixed_hdr_mbuf);
}
