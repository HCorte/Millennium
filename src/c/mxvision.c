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
/*====[MXVISION.C]============================================================*/
/*                                                                            */
/* TNIVISION                                                                  */
/*                                                                            */
/* Purpose: This is the main function for the TNIVISION program, which allows */
/*          the user to monitor the TNI server performance.                   */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*          This program attaches to the PX2CONCOM and TNICONCOM global       */
/*          sections.                                                         */
/*                                                                            */
/*====[MXVISION.C]============================================================*/
/*                                                                            */

#define DECLARE_MXVISION_GLOBAL_DATA

#include "includes_vision.h"

struct ERR_STRING err_string;
struct ERR_STRING null_err_string={"","","","",""};

WINDOW *win;

int main(void)
{
    int flags = 0;

#   if defined(PROSYS_ENV_ALL)

        G_ERROR error;

        g_initask(PROD_NBR, TASK_NAME, VERSION, C_YEAR, &error);
        if( error.errflg )
        {
            g_senderror(&error);
            g_endtask();
        }

        if( atchglb_all_read(PROD_NBR) != P_SUCCESS )
        {
            g_endtask();
        }

        if (atchglb_read_events(PROD_NBR) != P_SUCCESS )
        {
            g_endtask();
        }

#   endif

#   if defined(GOLS_ENV_ALL)

        if( !attach_core_sections('r', 'r', 'r') )
        {
            perror("can not attatch to shared memory");
            exit(0);
        }

#   endif    

    Mxsrv_Global();

    win = initscr();

#   if defined(PROSYS_ENV_ALL)

        max_terminal = volcom_p->sys.mxtrm;

#   endif

#   if defined(GOLS_ENV_ALL)

        GET_MAX_TERMS (&max_terminal);

#   endif

    if( win != NULL ) {

        outstanding_cmd = 0;
        current_menu = MAIN_MENU;

        wclear(win);
        Menu();

        Set_Next_Screen_Update();
        client();

    } else {
        output_err("mxvision",
                   ME_TNI_NO_WINDOW,
                   MX_ERR_LVL_FATAL,
                   err_string);
    }

    endwin();
    exit(0);

    /* make compiler happy */
    return 0;
}
