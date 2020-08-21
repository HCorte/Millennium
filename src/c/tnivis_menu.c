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
/*====[TNIVIS_MENU.C]=========================================================*/
/*                                                                            */
/* FUNCTION NAME AND CALLING PARAMETERS GO HERE                               */
/*   Menu()                                                                   */
/* Purpose:                                                                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/* Output Arguments:                                                          */
/*                                                                            */
/* Return Value:                                                              */
/*                                                                            */
/* Assumptions:                                                               */
/*                                                                            */
/*====[TNIVIS_MENU.C]=========================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

void Menu() {
    int line;

    wmove(win,0,0);
/*  wprintw(win,"MXVISION         %s",VERSION);  */

/*  use the VIP_ defines directly since VERSION does not work with /STAND=VAXC option from CC (which
    is used in LOTCC from Lotgen); note that VERSION would only work with /STAND=RELAXED_ANSI89 option  */
    wprintw(win,"MXVISION         %d.%d.%d %s", VIP_MAJOR, VIP_MINOR, VIP_MICRO, VIP_PATCH);

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    wmove(win,2,0);
    wprintw(win,"                         **** MENU of SELECTIONS **** ");

    line=5;
    wmove(win,line,20);
    wprintw(win,"APPlication  <index> ");

    line++; 
    wmove(win,line,20);
    wprintw(win,"CLIENT id  <clientId>");

    line++;
    wmove(win,line,20);
    wprintw(win,"CONnection  <index> ");

    line++; 
    wmove(win,line,20);
    wprintw(win,"DEBUG");

    line++;
    wmove(win,line,20);
    wprintw(win,"EVENT queues");

    line++; 
    wmove(win,line,20);
    wprintw(win,"LOG");

    line++; 
    wmove(win,line,20);
    wprintw(win,"RPC messaging");

    line++; 
    wmove(win,line,20);
    wprintw(win,"SERVER summary");

#   if defined(PROSYS_ENV_ALL)

        line++;
        wmove(win,line,20);

#       if defined(PROSYS_ENV_PLATFORM)

            wprintw(win,"TERMinal server id  <dev_loc_id>");

#       else

            wprintw(win,"TERMinal server id  <term>");

#       endif

#   endif

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
