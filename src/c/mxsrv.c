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
/*====[MXSRV.C]===============================================================*/
/*                                                                            */
/* PTNIPRO()                                                                  */
/*                                                                            */
/* Purpose: GSWITCH Main processing function, task of PX2X product            */
/*                                                                            */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*                                                                            */
/*====[MXSRV.C]===============================================================*/
/*                                                                            */

#include <stddef.h>
#include <stdlib.h>

#include "includes.h"

#if defined(XOS_VMS)

#   include <iodef.h>

#endif

struct ERR_STRING err_string;
struct ERR_STRING null_err_string={"","","","",""};

#if defined(PROSYS_ENV_ALL)

/*
*** --------------- dispose of no-action reply ---------------
*/

    void null_reply(void *msgp) {
        return;
    }

/*
** ----------------- trminp2 reply processor ------------------
*/

#   if defined(PROSYS_ENV_PLATFORM)

        void
        trminp2_reply (GM_ADDRESS *srcadrp,
                       GM_MESDSC  *mesdscp,
                       GM_MESDSC  *cntxtdscp)
        {
            int                 conn_num = 0;
            int                 term_stats_idx = -1;
            int                 rc = P_SUCCESS;    /* generic return code             */

            long int            term_server_id;

            TERM_STATS         *term_stats_p;      /* Pointer to the statistics       */
                                                   /* area for each network terminal  */

/*
 *  If the device location index returned in the terminal response
 *  message is negative, then the inputted device location identifier
 *  is unknown by GTRM.  Send then error PDU to the sender indicating
 *  the terminal is unknown.  Also, remove the terminals entry in
 *  the terminal statistics hash table.
 */

            term_server_id = srcadrp->adrval.trmnum;

            if (term_server_id > 0)
            {
                if (mesdscp->meslen != 0)
                {
                    Send_Oltp_Resp_Msg (term_server_id,
                                        mesdscp->meslen,
                                        mesdscp->mesbufp);
                }
                else
                {
/*
 *          Get the terminal's index to the terminal statistics global section
 *          and verify it is within the proper range.
 */

                    term_stats_idx = retrieve_term_stats_idx (term_server_id);

                    if ((term_stats_idx > 0) &&
                        (term_stats_idx <= max_terminal))
                    {
                        Calc_Game_Delay (term_stats_idx);

                        term_stats_p = &tstats_p->term_stats;
                        term_stats_p = &term_stats_p[term_stats_idx];

                        conn_num = term_stats_p->requesting_conn_num;

                        rc = send_term_unknown_error (conn_num,
                                                      term_server_id,
                                                      0,
                                                      NULL,
                                                      term_stats_p->correlation_tag);

                        memset(term_stats_p, 0x00, sizeof(TERM_STATS));
                        remove_from_term_stats_hash (term_server_id);

                        sprintf(err_string.par1, "OLTP request");

                        output_err("trminp2_reply",
                                   MI_TNI_NOPROC_MSG,
                                   MX_ERR_LVL_ERROR,
                                   err_string);

                        sprintf(err_string.par1, "Terminal Server Id");
                        sprintf(err_string.par2,"%ld",term_server_id);

                        output_err("trminp2_reply",
                                   MI_TNI_UNKNOWN,
                                   MX_ERR_LVL_ERROR,
                                   err_string);
                    }
                    else
                    {
                        sprintf (err_string.par1, "%s", "Response");
                        sprintf (err_string.par2, "%ld", term_server_id);
                        sprintf (err_string.par3, "%s", "?");
                        sprintf (err_string.par4, "%s", "bad stats idx");

                        output_err ("trminp2_reply",
                                    MI_TNI_NO_DELIVER,
                                    MX_ERR_LVL_ERROR,
                                    err_string);
                    }
                }
            }
            else
            {
                sprintf(err_string.par1, "OLTP response");

                output_err("trminp2_reply",
                           MI_TNI_NOPROC_MSG,
                           MX_ERR_LVL_ERROR,
                           err_string);

                sprintf(err_string.par1, "Terminal Server Id");
                sprintf(err_string.par2, "%ld", term_server_id);

                output_err("trminp2_reply",
                           MI_TNI_UNKNOWN,
                           MX_ERR_LVL_ERROR,
                           err_string);
            }
        }

#   endif

/*
*** ----------------- reply router ------------------
*/

    void reply(GM_ADDRESS *srcadrp, GM_MESDSC *mesdscp,
               GM_MESDSC *cntxtdscp) 
    {
        G_ERROR             error;
        struct GEN_CONTEXT *cntxt_p;
        GM_GENERIC         *msg_p;
        GM_ERROR           *gerr_p;

        cntxt_p = (struct GEN_CONTEXT *)cntxtdscp->mesbufp;

        switch (srcadrp->adrtyp) {

        case GAT_GTMS:

            msg_p = (GM_GENERIC *) mesdscp->mesbufp;

            switch (msg_p->fun)
            {
            case GMF_UNDELIVERABLE:
                g_setgtmserr(GE_STRING, &error);
                g_adderrstr("GTMS is unable to deliver message", &error);
                g_senderror(&error);

                break;

            case GMF_ERROR:
                gerr_p = (GM_ERROR *) mesdscp->mesbufp;

#               if defined(PROSYS_ENV_V7)

                    ((G_ERROR *)gerr_p->err)->errlvl = GEL_FATAL;
                    g_senderror((G_ERROR *)gerr_p->err);

#               else

                    gerr_p->error.errlvl = GEL_FATAL;
                    g_senderror(&gerr_p->error);

#               endif

                volcom_p->hdr.gamsts = OFF_LINE;
                g_endtask();
                break;

            default:
                p_reply (mesdscp, cntxtdscp);

                break;
            }
            break;

        case GAT_PRODUCT:
        case GAT_TASK:

            p_reply (mesdscp, cntxtdscp);
            break;

#       if !defined(PROSYS_ENV_PLATFORM)

            case GAT_TERMINAL:

                /*
                 *  Only terminal response messages queued to the game will
                 *  carry a context buffer.  Request messages queued to the
                 *  the game will have a NULL context buffer; their processing 
                 *  has not been changed.
                 */

                if (cntxtdscp->meslen == 0)
                {
                    if ((srcadrp->adrval.trmnum > 0) && 
                        (srcadrp->adrval.trmnum <= volcom_p->sys.mxtrm))
                    {
                        Send_Oltp_Resp_Msg (srcadrp->adrval.trmnum,
                                            mesdscp->meslen,
                                            mesdscp->mesbufp);
                    }
                    else
                    {
                        sprintf(err_string.par1, "OLTP response");

                        output_err("reply",
                                   MI_TNI_NOPROC_MSG,
                                   MX_ERR_LVL_ERROR,
                                   err_string);

                        sprintf(err_string.par1,"%d",index);
                        sprintf(err_string.par2,"1");
                        sprintf(err_string.par3,"%d",volcom_p->sys.mxtrm);

                        output_err("reply",
                                   ME_BAD_TER_RNG,
                                   MX_ERR_LVL_ERROR,
                                   err_string);
                    }
                }
                else
                {
                    p_reply (mesdscp, cntxtdscp);
                }
                break;

#       else

            case GAT_TERMINAL2:

                /*
                 *  Only terminal response messages queued to the game will
                 *  carry a context buffer.  Request messages queued to the
                 *  the game will have a NULL context buffer; their processing
                 *  has not been changed.
                 */

                if (cntxtdscp->meslen == 0)
                {
                    trminp2_reply (srcadrp, mesdscp, cntxtdscp);
                }
                else
                {
                    p_reply (mesdscp, cntxtdscp);
                }
                break;

#       endif

        default:  /* ignore all other replies */

            null_reply(mesdscp);
            break;
        }
        return;
    }

#    define TASK_NAME "MXSRV"
#    define VERSION MXSRV_RELEASE

#endif

int main(void) {
    int result, version, filnum, temp;
    int receive;
    int st;                              /* System service return status */
    int x2x_inp_st, cdc0;
    int product;

#   if defined(PROSYS_ENV_ALL)

#       if defined(PROSYS_ENV_V7)

            CHKPNTLOCATE chkloc;

#       else

            CHKPNT_ADDRESS      chkloc;          /* serial number information */

#       endif

        G_ERROR      error,glb_error;
        GM_ADDRESS   srcadr;
        GM_MESDSC    mesdsc, cntxtdsc;  

#   endif

    Mxsrv_Global();

#   if defined(GOLS_ENV_ALL)

        if (!create_core_sections()) {

            output_err("Mxsrv",
                       ME_TNI_NO_START,
                       MX_ERR_LVL_FATAL,
                       err_string);

            return(0);
        }

#   endif

#   if defined(PROSYS_ENV_ALL)

        /* initialize task with GTMS */

        g_initask(PROD_NBR, TASK_NAME, VERSION, C_YEAR, &error);
        if( error.errflg ) {
            g_senderror(&error);
            g_endtask();
        }

        /* register task with GTMS */

        gtms_regtask(MX_MAINPRO_ID, GW_NEVERWAKE, &error);
        if( error.errflg ) {
            g_senderror(&error);
            g_endtask();
        }

        /* Attach to global memory */

        if( atchglb_all_write(PROD_NBR) != P_SUCCESS ) {
            g_senderror(&glb_error);
            printf("Not able to attach to GLOBAL SECTIONS.\n");

            gtms_deregtask(&error);                      
            if( error.errflg ) g_senderror(&error);
            g_endtask();                        
        }

        /* set checkpoint information */

        if (inichk (&chkloc) == P_FAILURE)
        {
            g_endtask();
        }

        volcom_p->hdr.syssts = NOSTAT;

        gtms_prodreproc(GRT_NEW, chkloc.cdc, chkloc.logadr, NULL, &glb_error);

        if( glb_error.errflg ) {
            g_senderror(&glb_error);
            g_endtask();
        }

        volcom_p->prod.state = REPROCESSING;

    /* Initial the TNI task. This includes reading the configuration file */
    /* as well as specific TNI requirements                               */

#   endif

    if( ! Tni_Init() ) {

        output_err("mxpro",
                   ME_TNI_ERR_CFG_FIL,
                   MX_ERR_LVL_FATAL,
                   err_string);


#   if defined(PROSYS_ENV_ALL)

        /* task is coming down */

        gtms_deregtask(&error);                      
        if( error.errflg ) g_senderror(&error);
        g_endtask();                        

#   endif

    }
    else {

        Server_Main_Loop();

#   if defined(PROSYS_ENV_ALL)

        gtms_deregtask(&error);                      
        g_endtask();                        

        if( error.errflg ) {
           g_senderror(&error);
        }

#   endif

    }

    return(0); 
} /* end main */
