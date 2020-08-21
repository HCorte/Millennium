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
/*====[TNIVIS_DEBUG.C]========================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Debug()                                                           */
/*          Debug_Input(char *command, int par)                               */
/*                                                                            */
/*====[TNIVIS_DEBUG.C]========================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Debug()                                                                    */
/*                                                                            */
/* Purpose: This function displays the debug snapshot.                        */
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

void Debug() {

    wmove(win,0,0);
    wprintw(win,"DEBUG SNAPSHOT");

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    switch( tnicon_p->print_flag ) {
    case -1:

        wmove(win,2,5);
        wprintw(win,"Debug level: debug file error       ");
        break;

    case 0:

        wmove(win,2,5);
        wprintw(win,"Debug level: off                    ");
        break;

    case SOCKET_LEVEL_DBG:

        wmove(win,2,5);
        wprintw(win,"Debug level: socket                 ");
        break;

    case PDU_LEVEL_DBG:

        wmove(win,2,5);
        wprintw(win,"Debug level: PDU                    ");
        break;

    case SOCKET_LEVEL_DBG + PDU_LEVEL_DBG:

        wmove(win,2,5);
        wprintw(win,"Debug level: socket/PDU             ");
        break;

    case STATS_LEVEL_DBG:

        wmove(win,2,5);
        wprintw(win,"Debug level: statistics             ");
        break;

    case SOCKET_LEVEL_DBG + STATS_LEVEL_DBG:

        wmove(win,2,5);
        wprintw(win,"Debug level: socket/statistics      ");
        break;

    case PDU_LEVEL_DBG + STATS_LEVEL_DBG:

        wmove(win,2,5);
        wprintw(win,"Debug level: PDU/statistics         ");
        break;

    case SOCKET_LEVEL_DBG + PDU_LEVEL_DBG + STATS_LEVEL_DBG:

        wmove(win,2,5);
        wprintw(win,"Debug level: socket/PDU/statistics  ");
        break;

    case TERMINAL_LEVEL_DBG:

        switch(tnicon_p->dbg_term_num) {

        case ALL_TERMINALS:
        
            wmove(win,2,5);
            wprintw(win,"Debug level: terminal (all)         ");
            break;

        case 0:
        
            current_menu = DEBUG_TERMINAL;
            break;

        default:

            wmove(win,2,5);
            wprintw(win,"Debug level: terminal (%d)          ",
                    tnicon_p->dbg_term_num);
            break;
        }
        break;
    }

    wmove(win,21,0);
    wprintw(win,"    EXIT, MENU");

    wmove(win,23,0);
    wprintw(win,"Selection: ");

    refresh();
    wrefresh(win);

    wmove(win,23,11);

    input_y = 23;
    input_x = 11;
} 

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Debug_Input(char *command, int par)                                        */
/*                                                                            */
/* Purpose: This function processes all the commands specific to the          */
/*          debug snapshot.                                                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          command          string portion of the command entered            */
/*          par              integer command or integer parameter accompaning */
/*                           command (optional)                               */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Debug_Input(char *command, int par) {

    int status = 0;

    if (par != -1)
    {
        status = Generate_Vision_Cmd(CHANGE_DEBUG_LEVEL,
                                     par,
                                     0,
                                     NULL,
                                     NULL);
        Display_Cmd_Result(status);
    }
}
