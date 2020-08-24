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
/*====[TNIVIS_MAIN_LOOP.C]====================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Server ()                                                         */
/*          Set_Next_Screen_Update ()                                         */
/*          Check_Gtms_Messages()                                             */
/*          Reply(GM_ADDRESS *srcadrp,                                        */
/*                GM_MESDSC *mesdscp,                                         */
/*                GM_MESDSC *cntxtdscp)                                       */
/*                                                                            */
/*====[TNIVIS_MAIN_LOOP.C]====================================================*/
/*                                                                            */

#include <string.h>
#include <errno.h>

#include "includes_vision.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <signal.h>
#   include <sys/timeb.h>

#elif defined(XOS_VMS)

#   include <ssdef.h>
#   include <timeb.h>

#else

#   error - OS-specific logic not handled.

#endif

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Server (sig)                                                               */
/*                                                                            */
/* Purpose: This function calls the propriate snapshot based on the value of  */
/*          current menu.  The entire screen is cleared only when the user    */
/*          moves from one snapshot to another.  Once the snapshot is         */
/*          displayed the next screen updated is calculated.                  */
/*                                                                            */
/* Input Arguments:                                                           */
/*          sig              signal (UNIX only)                               */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Server(int sig) {

    static int old_menu = 0;

#   if defined(PROSYS_ENV_ALL)

        if (outstanding_cmd) {

            Check_Gtms_Messages();
        }

#   endif

    if (current_menu != old_menu) {

        old_menu = current_menu;
        wclear(win);
    }

    switch( current_menu ) {
    case (MAIN_MENU):
        Menu();
        break;

    case (APPLICATION):
        Application();
        break;

    case (CLIENTID):
        Client_Id();
        break;

    case (CONNECTION):
        Conn();
        break;

    case (DEBUG):
        Debug();
        break;

    case (DEBUG_TERMINAL):
        Debug_Terminal();
        break;

    case (EVENT_QUEUES):
        Event_Queues();
        break;

    case (LOG):
        Logging();
        break;

    case (CHNG_LOG_FILE):
        ChngLogFile();
        break;

    case (CHNG_LOG_THRESHOLD):
        ChngLogThreshold();
        break;
       
    case (RPC):
        Rpc_Messaging();
        break;

    case (SERVER_SUMMARY):
        Serv_Summary();
        break;

    case (TERM):
        Term_Serv();
        break;

    case (MXVIS_STOP):
        break;

    }

    wmove(win,0,0);

    Set_Next_Screen_Update();

    return;
} 

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Set_Next_Screen_Update()                                                   */
/*                                                                            */
/* Purpose: This function calculates the next call time to the server         */
/*          function.  The server function updates the snapshot displayed.    */
/*          In VMS an AST is used.  In UNIX the signal SIGALRM is used.       */
/*          The display update rate is 1 second (hard-coded).                 */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Set_Next_Screen_Update(void) {

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

    int ret;
    static int init_invl = 1;

    struct sigaction sa;                   /* sigaction structure             */

    if (init_invl)
    {
        sa.sa_handler = Server;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART;

        if (sigaction(SIGALRM, &sa, NULL) == -1)
        {
            sprintf(err_string.par1, "SIGALRM");

            output_err("Set_Next_Screen_Update",
                       MI_TNI_INIT_SIG,
                       MX_ERR_LVL_FATAL,
                       err_string);
        }
        init_invl = 0;
    }

    if ( (ret= alarm(1)) < 0 )
    {
        output_err("Set_Next_Screen_Update",
                   MI_TNI_SIG_ALRM,
                   MX_ERR_LVL_WARNING,
                   err_string);
    }

#elif defined(XOS_VMS)

#   define SCREEN_UPD_EFN       7          /* screen upat event flag          */

    int status;                            /* system service return status    */

    static int init_invl = 1;

#   pragma member_alignment save
#   pragma nomember_alignment

        static struct TIMER_VALUES update_invl;

#   pragma member_alignment restore

    if (init_invl)
    {
        update_invl = Set_Timervalue_Vms(100);
        init_invl = 0;
    }

    status = SYS$SETIMR(SCREEN_UPD_EFN,
                        &update_invl,
                        Server,
                        NULL,
                        0);

    if (status != SS$_NORMAL)
    {
        output_err("Set_Next_Screen_Update",
                   MI_TNI_SCREEN_TIMER,
                   MX_ERR_LVL_FATAL,
                   err_string);
    }
    
#else

#   error - OS-specific logic not handled.

#endif

    return;
}

#if defined(PROSYS_ENV_ALL)

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Check_Gtms_Messages()                                                      */
/*                                                                            */
/* Purpose: This function is only executed in a PRO:SYS system.  It polls     */
/*          GTMS for message replies when a task-to-task command message is   */
/*          outstanding                                                       */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

    void Check_Gtms_Messages() 
    {
        int result;

        static int init_buffs = 1;

        G_ERROR  error;
        GM_ADDRESS srcadr;
        GM_MESDSC mesdsc;
        GM_MESDSC cntxtdsc;

        static ubyte_1 *inp_msg_p;
        static ubyte_1 *cntxt_p;

        /* allocate memory for input, output message buffer and context buffer*/

        if (init_buffs)
        {
            inp_msg_p   = (ubyte_1 *)malloc( volcom_p->sys.prod_meslen );
            cntxt_p     = (ubyte_1 *)malloc( volcom_p->sys.prod_meslen );

            init_buffs = 0;
        }

        /* set up receive buffers */

        mesdsc.mesbufsiz= volcom_p->sys.prod_meslen;
        mesdsc.mesbufp  = inp_msg_p;

        cntxtdsc.mesbufsiz  = volcom_p->sys.prod_meslen;
        cntxtdsc.mesbufp    = cntxt_p;

        result = gtms_receive(&srcadr, &mesdsc, &cntxtdsc, &error);

        if (!error.errflg)
        {

            /* Service replys from GTMS */

            switch (result)
            {

            /* All replies will be resolved  */
            /* by executing context function */

            case GR_REPLY:

                p_reply(&mesdsc, &cntxtdsc);

                free(inp_msg_p);
                free(cntxt_p);

                init_buffs = 1;
                break;

            case GR_NOMESSAGE:
                break;

            default:
                g_setgtmserr(GE_INVALID, &error);
                g_adderrstr("result", &error);
                g_adderrint(result, &error);
                error.errlvl = GEL_INFO;
                g_senderror(&error);
                error.errflg=0;
                break;
            }
        }
        else
        {
            g_senderror(&error);
        }

        return;
    }
#endif
