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
/*====[create_core_sections.c]================================================*/
/*                                                                            */
/* create_core_sections ()                                                    */
/*                                                                            */
/* Purpose: Create core global sections.                                      */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[create_core_sections.c]================================================*/
/*                                                                            */

#include "includes.h"

#if defined(GOLS_ENV_ALL)

#define BLOCK_BYTES     512     /* disk block size in bytes         */
#define MAX_FNAME_SIZE   50

/* ===[GH_PAGESIZE.C]===================================================

   Description: Get the memory page size
   ===================================================================== */

#include <syidef.h>
#include <ssdef.h>

unsigned int SYS$GETSYIW();

#ifndef SYI$_PAGE_SIZE
#define SYI$_PAGE_SIZE 4452
#endif

struct itm
{
    short int buflen;
    short int item_code;
    int      bufadr;
    int      retlenadr;
};

int gh_pagesize(int *errp)
{
int cpu_pagesize, cpu_pagesize_len;     /* alpha */
int status;

struct itm itmlst[2];                    /* alpha */

    err_string = null_err_string;
    *errp = 0;

    itmlst[0].buflen = sizeof(cpu_pagesize);
    itmlst[0].item_code = SYI$_PAGE_SIZE;
    itmlst[0].bufadr = (int)&cpu_pagesize;
    itmlst[0].retlenadr = (int)&cpu_pagesize_len;

    itmlst[1].buflen = 0;
    itmlst[1].item_code = 0;

    status = SYS$GETSYIW(0, 0, 0, itmlst, 0, 0, 0);
    if (status != SS$_NORMAL)
    {
        output_err("gh_pagesize",
                   MI_PAGESIZE,
                   MX_ERR_LVL_FATAL,
                   err_string);

        *errp  = status;
        return(0);
    }
    return cpu_pagesize;
}

/*  ===[ GH_GLBSECFILNAM.C ]=================================================
   ==========  THIS IS A VMS PLATFORM SPECIFIC MODULE  ==========
    =========================================================================*/

#include <stdio.h>

void gh_glbsecfilnam( char *glbsec_name, char *file_name) {

    sprintf( file_name, "GXSHR:%s.GLB", glbsec_name);
}

/*  ===[ G_DELVIRSPC.C ]=======================================================

    Description:

      Delete a range of addresses from a process's virtual address space.
      It only deletes a number of PAGES in the process's virtual address
      space. Thus, the starting address of the range must be page aligned,
      and the size of space to be deleted must be a multiple of pages

    Argument list:
      startp         Starting address of the range to be deleted,
                     must be a multiple of pages
      size           size of the range to be deleted  (in bytes)
                     must be a multiple of pages
      errorp         Pointer to error structure   (output)

    Return:          void

    Possible error:
      MI_INVALID     Invalid size of virtual space to be deleted,
                     or starting address is not page aligned
      MI_DELETE      Error deleting virtual space

    =========================================================================*/

#include <ssdef.h>
#include <secdef.h>
#include <descrip.h>

void g_delvirspc(void *startp,            /* start address              */
                 int  size,               /* size in byte               */
                 int *errorp)             /* pointer to error structure */
{
      unsigned int   deletests;
      unsigned int   inadr[2];
      unsigned int   retadr[2];
      int page_size;

      err_string=null_err_string;
      *errorp = 0;

      page_size = gh_pagesize(errorp);
      if (*errorp) {
         return;
      }

/*
**  make sure size is multiple of pages and starting address is
**  page aligned
*/
      if (size <= 0)
      {
         sprintf(err_string.par1,"virtual address size");
         sprintf(err_string.par2,"%d",size);

         output_err("g_delvirspc",
                    MI_INVALID,
                    MX_ERR_LVL_FATAL,
                    err_string);

         *errorp = 1;
         return;
      }

      if ((int)startp % page_size)
      {
         sprintf(err_string.par1,"starting address, page not aligned");
         sprintf(err_string.par2,"%d",(int)startp);

         output_err("g_delvirspc",
                    MI_INVALID,
                    MX_ERR_LVL_FATAL,
                    err_string);

         *errorp = 1;
         return;
      }

      inadr[0] = (unsigned int)startp;
      inadr[1] = inadr[0] + size - 1;
      retadr[0] = 0;
      retadr[1] = 0;

      deletests = SYS$DELTVA(inadr, retadr, 0);

      if (deletests != SS$_NORMAL)
      {
         sprintf(err_string.par1,"VIRTUAL SPACE");

         output_err("g_delvirspc",
                    MI_DELETE,
                    MX_ERR_LVL_FATAL,
                    err_string);

         *errorp = deletests;
         return;
      }
      return;
}

/*  ===[ G_DELETEGLB.C ]=======================================================

    Description:

      Delete system or group global section

    Argument list:
      name           Name of global section to be deleted  (input)
                     e.g.  "GXPROCOM"
      glbtype        Type of global section,
                     'G' or 'g' for group global section,
                     'S' or 's' for system global section
      errorp         Pointer to error structure   (output)

    Return:          void

    Possible error:
      MI_DELETE      Error deleting global section
                     (global section does not exists, etc.)

    =========================================================================*/

#include <ssdef.h>
#include <secdef.h>
#include <string.h>
#include <descrip.h>

void g_deleteglb(char *name,              /* global section name        */
                 char glbtype,            /* type of global section     */
                 int *errorp)             /* pointer to error structure */
{
   unsigned int deletests;
   unsigned int flag;

   struct dsc$descriptor_s logicalname;

   err_string=null_err_string;
   *errorp = 0;

   flag = 0;

   if (glbtype == 'S' || glbtype == 's')
      flag = SEC$M_SYSGBL;

   logicalname.dsc$w_length = strlen(name);
   logicalname.dsc$b_dtype = DSC$K_DTYPE_T;
   logicalname.dsc$b_class = DSC$K_CLASS_S;
   logicalname.dsc$a_pointer = name;

   deletests = SYS$DGBLSC(flag, &logicalname, 0);

   if ((deletests != SS$_NORMAL) &&
       (deletests != SS$_NOSUCHSEC))
   {
      sprintf(err_string.par1,"GLBSEC");
      sprintf(err_string.par2,"%s",name);

      output_err("g_deleteglb",
                 MI_DELETE,
                 MX_ERR_LVL_FATAL,
                 err_string);

      *errorp = deletests;
      return;
   }

   *errorp = 0;

   return;
}
/*  ===[ G_ATTACHNEWGLB.C ]====================================================

    Description:

      Create and attach to a global section
      Attempt to reuse existing one if size matches

    Argument list:
      glbfilname     Name of physical file associated
                     with global section (input)
                     e.g. "GXPROCOM.FIL"
      glbsecname     Global section name (input)
                     e.g. "GXPROCOM"
      glbtype        Type of global section
                     'G' or 'g' for group global section,
                     'S' or 's' for system global section
      size           Global section Size (input)
      errorp         Pointer to error structure   (output)

    Return:          Pointer to the beginning address from which
                     global section was actually created
                     0 is returned if an error occurs

    Possible errors:
      MI_CREATE      Error creating RMS file or global section
      GE_CLOSE       Error closing RMS file
      MI_INVALID     Invalid  global section size

    =========================================================================*/

#include <stdio.h>
#include <string.h>
#include <ssdef.h>
#include <secdef.h>
#include <rms.h>
#include <descrip.h>

char *g_attachnewglb(char *glbfilname,        /* physical file name         */
                     char *glbsecname,        /* global section name        */
                     char glbtype,            /* type of global section     */
                     int  size,               /* global section size        */
                     int  *errorp)            /* pointer to error structure */
{

  struct FAB fblock;                /* File Access Block */

  unsigned int rmssts;              /* Record Management Services status */
  unsigned int inadr[2];
  unsigned int retadr[2];
  unsigned int creatests;
  unsigned int flags;
  unsigned int chan;                /* channel number to access file */
  unsigned int attachsts;
  int existing_size;
  int adjusted_size;
  int page_size;
  char phys_glb_name[256];
  struct dsc$descriptor_s logicalname;

    err_string=null_err_string;
    *errorp = 0;

    if (size <= 0) {
        sprintf(err_string.par1,"SIZE");
        sprintf(err_string.par2,"%d",size);

        output_err("g_attachnewglb",
                   MI_INVALID,
                   MX_ERR_LVL_FATAL,
                   err_string);

        *errorp = 1;
        return(0);
    }

    page_size = gh_pagesize(errorp);

    if (*errorp) {
        return 0;
    }

    adjusted_size = ( (size + page_size - 1) / page_size) * page_size;

    gh_glbsecfilnam( glbsecname, phys_glb_name);
                                    /* starting and ending addresses to map */
    inadr[0] = 0;

    flags = SEC$M_WRT | SEC$M_EXPREG ;
    logicalname.dsc$w_length = strlen(glbsecname);
    logicalname.dsc$b_dtype = DSC$K_DTYPE_T;
    logicalname.dsc$b_class = DSC$K_CLASS_S;
    logicalname.dsc$a_pointer = glbsecname;

    attachsts = SYS$MGBLSC(inadr, retadr, 0, flags, &logicalname, 0, 0);
    if (attachsts == SS$_NORMAL)
    {
        existing_size = retadr[1] - retadr[0] + 1;
        if( existing_size == adjusted_size) {
             memset( (void *)(retadr[0]), 0, size);
             return((char *)retadr[0]);
        }

        g_delvirspc( (char *)retadr[0], existing_size, errorp);
        if (*errorp) {
             return 0;
        }
     }

    g_deleteglb(glbsecname, 'G', errorp);

                                    /* initialize file access block */
    fblock = cc$rms_fab;
    fblock.fab$l_alq = (adjusted_size + BLOCK_BYTES - 1)/BLOCK_BYTES;
    fblock.fab$w_ifi = 0;
    fblock.fab$l_fop = FAB$M_UFO | FAB$M_CBT;
    fblock.fab$l_fna = phys_glb_name;
    fblock.fab$b_fns = strlen(phys_glb_name);
    fblock.fab$b_fac = FAB$M_DEL | FAB$M_GET | FAB$M_PUT | FAB$M_UPD;
    fblock.fab$b_shr = FAB$M_NIL;

                                    /* create RMS file */

    rmssts = SYS$CREATE(&fblock);

    if (rmssts != RMS$_NORMAL && rmssts != RMS$_CREATED)
    {
        sprintf(err_string.par1,"FILE");
        sprintf(err_string.par2,"%s",phys_glb_name);

        output_err("g_attachnewglb",
                   MI_CREATE,
                   MX_ERR_LVL_FATAL,
                   err_string);

        *errorp = rmssts;
        return(0);
    }

    chan = fblock.fab$l_stv;

                                    /* find first slto in p0 space */
    inadr[0] = 0;
                                    /* read/write and permanent global section*/

    flags = SEC$M_GBL | SEC$M_WRT | SEC$M_PERM | SEC$M_EXPREG | SEC$M_DZRO;

    logicalname.dsc$w_length = strlen(glbsecname);
    logicalname.dsc$b_dtype = DSC$K_DTYPE_T;
    logicalname.dsc$b_class = DSC$K_CLASS_S;
    logicalname.dsc$a_pointer = glbsecname;

                                    /* create global section */
    creatests = SYS$CRMPSC(inadr, retadr, 0, flags, &logicalname, 0, 0,
                                  chan, 0, 0, 0, 0);

    if (creatests != SS$_NORMAL && creatests != SS$_CREATED)
    {
        sprintf(err_string.par1,"GLBSEC");
        sprintf(err_string.par2,"%s",glbsecname);

        output_err("g_attachnewglb",
                   MI_CREATE,
                   MX_ERR_LVL_FATAL,
                   err_string);

        *errorp = creatests;
        return(0);
    }
                                    /* demand zero is not enough if pages not
                                     referenced by THIS task */
    memset( (void *)(retadr[0]), 0, size);

    *errorp = 0;

    return((char *)retadr[0]);
}

/*  ===[ G_ATTACHGLB.C ]=======================================================

    Description:

      Attach to an existing global section (create memory)

    Argument list:
      name           Global section name  (input)
                     e.g. "GXPROCOM"
      glbtype        Type of global section
                     'G' or 'g' for group global section,
                     'S' or 's' for system global section
      rwflg          Global section access mode (input)
                     'r' or 'R' for read-only,
                     'w' or "W' for write
      errorp         Pointer to error structure   (output)

    Return:          pointer to the beginning address from which
                     global section was actually mapped
                    0 is returned if an error occurs

    Possible errors:
      MI_ACCESS      Error accessing global section
      MI_ATTACH      Error attaching global section
                                                                                
    =========================================================================*/

#include <string.h>
#include <ssdef.h>
#include <secdef.h>
#include <descrip.h>

char *g_attachglb(char *name,             /* global section name        */
                  char glbtype,           /* type of global section     */
                  char rwflag,            /* global section access mode */
                  int *errorp)            /* pointer to error structure */
{
   unsigned int  attachsts;
   unsigned int  flags;
   unsigned int  inadr[2];
   unsigned int  retadr[2];

   struct dsc$descriptor_s logicalname;

   
   err_string=null_err_string;
   *errorp = 0;

   /* starting and ending addresses to be mapped into */

   inadr[0] = 0;

   if (rwflag != 'r' && rwflag != 'R' && rwflag != 'w' && rwflag != 'W')
   {
      /* bad parameter for mapping global section access mode */

      sprintf(err_string.par1,"GLBSEC");
      sprintf(err_string.par2,"%s",name);

      output_err("g_attachglb",
                 MI_ACCESS,
                 MX_ERR_LVL_FATAL,
                 err_string);

      return(0);
   }

   if (rwflag == 'r' || rwflag == 'R')
   {
      /* map (system) global section with read-only access */

      flags = SEC$M_EXPREG;
   }
   else
   {
      /* map (system) global section with read/write access */

      flags = SEC$M_WRT | SEC$M_EXPREG ;
   }

   if (glbtype == 'S' || glbtype == 's')
      flags = flags | SEC$M_SYSGBL;

   logicalname.dsc$w_length = strlen(name);
   logicalname.dsc$b_dtype = DSC$K_DTYPE_T;
   logicalname.dsc$b_class = DSC$K_CLASS_S;
   logicalname.dsc$a_pointer = name;

   attachsts = SYS$MGBLSC(inadr, retadr, 0, flags, &logicalname, 0, 0);

   if (attachsts != SS$_NORMAL)
   {

       sprintf(err_string.par1,"GLBSEC");
       sprintf(err_string.par2,"%s",name);

       output_err("g_attachglb",
                  MI_ATTACH,
                  MX_ERR_LVL_FATAL,
                  err_string);

       *errorp = attachsts;
       return(0);
   }


   *errorp = 0;

   return((char *)retadr[0]);
}

/*  ===[ G_GETSYSMES.C ]=======================================================

   ==========  THIS IS A VMS PLATFORM SPECIFIC MODULE  ==========

    Description:
    
    translates system error code into text using VMS service
    =========================================================================*/

#include <stdio.h>
#include <string.h>
#include <descrip.h>
#include <ssdef.h>

void g_getsysmes(int errcod,            /* system error code            */
                 char *message,         /* text buffer >= 132 bytes (output) */
                 int *meslen)           /* received text length (output)    */

{
    struct dsc$descriptor_s text;       /* VAX string descriptor */
    register st;
    short ml;

    text.dsc$w_length = 100;            /* set up the string descriptor */
    text.dsc$a_pointer = message;

    if ((st=SYS$GETMSG(errcod,&ml,&text,15,0)) != SS$_NORMAL)
    {
        printf("\n>>> Error from GETMSG %d \n",st);     /* trouble  */   
        meslen=0;
        message[0]=0;
    }
    else
    {
        *meslen = ml; 
        message[*meslen]=0;             /* insert terminator    */
/*      printf("\n %.*s \n", *meslen, message);      */
    }
}

#include <string.h>

int create_core_sections()
{
    int error;
    int stats_size;
    int num_devices;
    int vmserrlen;

    char vmserrmes[100];
    char filnam[MAX_FNAME_SIZE];
    char glbnam[MAX_FNAME_SIZE];

    glbnam[MAX_FNAME_SIZE-1] = '\0';
    filnam[MAX_FNAME_SIZE-1] = '\0';

    strncpy(glbnam,"MXCON_SECT", MAX_FNAME_SIZE - 1);
    strncpy(filnam,"GXSHR:MXCON_SECT.GBL", MAX_FNAME_SIZE - 1);

    tnicon_p = (TNICON *)g_attachnewglb(filnam, 
                                        glbnam, 
                                        'g',
                                        sizeof(TNICON),
                                        &error);

    if (error)
    {
        g_getsysmes(error,vmserrmes,&vmserrlen);

        sprintf(err_string.par1,
                "%s",
                vmserrmes);

        output_err("Create_Core_Sections",
                 MI_TNI_STRING,
                 MX_ERR_LVL_FATAL,
                 err_string);

        return(0);
    }

    strncpy(glbnam,"MXSTATS_SECT", MAX_FNAME_SIZE - 1);
    strncpy(filnam,"GXSHR:MXSTATS_SECT.GBL", MAX_FNAME_SIZE - 1);

    GET_MAX_TERMS(&num_devices);

    stats_size = (num_devices + 1) * sizeof (TSTATS);
    tstats_p = (TSTATS *)g_attachnewglb(filnam,
                                        glbnam,
                                        'g',
                                        stats_size,
                                        &error);

    if (error)
    {
        g_getsysmes(error,vmserrmes,&vmserrlen);

        sprintf(err_string.par1,
                "%s",
                vmserrmes);

        output_err("Create_Core_Sections",
                 MI_TNI_STRING,
                 MX_ERR_LVL_FATAL,
                 err_string);

        return(0);
    }

    return(1);
}

int create_events_section(int num_events)
{
    int error;
    int var_size;
    int vmserrlen;

    char vmserrmes[100];
    char filnam[MAX_FNAME_SIZE];
    char glbnam[MAX_FNAME_SIZE];

    glbnam[MAX_FNAME_SIZE-1] = '\0';
    filnam[MAX_FNAME_SIZE-1] = '\0';

    strncpy(glbnam,"EVENTS_SECT", MAX_FNAME_SIZE - 1);
    strncpy(filnam,"GXSHR:EVENTS_SECT.GBL", MAX_FNAME_SIZE - 1);

    var_size = sizeof(RQST_EVENT) * num_events;


    event_free_queue = (EVENT_QUE_HDR *) g_attachnewglb(filnam, 
                                                        glbnam, 
                                                        'g',
                                                        var_size,
                                                        &error);

    if (error)
    {
        g_getsysmes(error,vmserrmes,&vmserrlen);

        sprintf(err_string.par1,
                "%s",
                vmserrmes);

        output_err("create_events_section",
                 MI_TNI_STRING,
                 MX_ERR_LVL_FATAL,
                 err_string);

        return(0);
    }
    else
    {
        rpc_events_queue = event_free_queue + 1;
        request_events = (RQST_EVENT *) (rpc_events_queue + 1);
    }

    return(1);
}

int attach_core_sections(char    tnicon_type,       /* tnicon section  */
                         char    tstats_type,       /* tstats section  */
                         char    events_type)       /* events section  */
{
    int status;
    int error;

    char filnam[MAX_FNAME_SIZE]; /* global section file name */
    char glbnam[MAX_FNAME_SIZE]; /* global section name */

    glbnam[MAX_FNAME_SIZE-1] = '\0';
    filnam[MAX_FNAME_SIZE-1] = '\0';

/* attach to global sections */

    status = 1;

/* attach tnicon section */

    if(tnicon_type == 'w' || tnicon_type == 'r')
    {
        strncpy(glbnam,"MXCON_SECT", MAX_FNAME_SIZE - 1);
        strncpy(filnam,"GXSHR:MXCON_SECT.GBL", MAX_FNAME_SIZE - 1);

        tnicon_p = (TNICON *) g_attachglb(glbnam,
                                          'g', 
                                          tnicon_type, 
                                          &error);
        if (error)
        {
           return(0);
        }
    }

/* attach tstats section */

    if(tstats_type == 'w' || tstats_type == 'r')
    {
        strncpy(glbnam,"MXSTATS_SECT", MAX_FNAME_SIZE - 1);
        strncpy(filnam,"GXSHR:MXSTATS_SECT.GBL", MAX_FNAME_SIZE - 1);

        tstats_p = (TSTATS *) g_attachglb(glbnam,
                                          'g', 
                                          tstats_type, 
                                          &error);
        if (error)
        {
           return(0);
        }
    }

/* attach events section */

    if(events_type == 'w' || events_type == 'r')
    {
        strncpy(glbnam,"EVENTS_SECT", MAX_FNAME_SIZE - 1);
        strncpy(filnam,"GXSHR:EVENTS_SECT.GBL", MAX_FNAME_SIZE - 1);

        event_free_queue = (EVENT_QUE_HDR *) g_attachglb(glbnam,
                                                         'g',
                                                         events_type,
                                                         &error);
        if (error)
        {
           return(0);
        }
        else
        {
            rpc_events_queue = event_free_queue + 1;
            request_events = (RQST_EVENT *) (rpc_events_queue + 1);
        }
    }

    return(status);
}

#endif

#if defined(PROSYS_ENV_ALL)

int status;                                /* status of function */
G_ERROR error;                             /* GTMS error structure */

int create_core_sections(int prdnbr, int num_devices)
{
    ubyte_4 var_size;
    ubyte_4 stats_size;                        /* V1.01                */
    ubyte_4 term_hash_size = 0;

    char filnam[MAX_FNAME_SIZE];
    char glbnam[MAX_FNAME_SIZE];

/* create TNICON */

    p_makglbnam(prdnbr, TNICON_SECT, filnam, glbnam);

#   if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        tnicon_p = (TNICON *)g_createshrmem(filnam, 
                                            sizeof(TNICON), 
                                            &error);

#   else

        tnicon_p = (TNICON *)g_attachnewglb(filnam, glbnam, 'g',
                                            sizeof(TNICON),
                                            &error);

#   endif

    if (error.errflg)
    {
        g_senderror(&error);
        return(P_FAILURE);
    }

/* create VOLCOM */

    p_makglbnam(prdnbr, VOLCOM_SECT, filnam, glbnam);
    var_size = size_of_volcom_prodbuf();

#   if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        volcom_p = (VOLCOM *)g_createshrmem(filnam, 
                                            (sizeof(VOLCOM) + var_size), 
                                             &error);

#   else

        volcom_p = (VOLCOM *)g_attachnewglb(filnam, glbnam, 'g',
                                            (sizeof(VOLCOM) + var_size),
                                            &error);

#   endif

    if (error.errflg)
    {
        g_senderror(&error);
        return(P_FAILURE);
    }
    volcom_p->prodbuf_size = var_size;

/* create GBLCOM */

    p_makglbnam(prdnbr, GBLCOM_SECT, filnam, glbnam);

#   if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        gblcom_p = (GBLCOM *)g_createshrmem(filnam,
                                            SIZEOF_GBLCOM,
                                            &error);

#   else

        gblcom_p = (GBLCOM *)g_attachnewglb(filnam, glbnam, 'g',
                                            SIZEOF_GBLCOM,
                                            &error);

#   endif

    if (error.errflg)
    {
        g_senderror(&error);
        return(P_FAILURE);
    }

/* create TSTATS */

    p_makglbnam(prdnbr, TSTATS_SECT, filnam, glbnam);

#   if defined(PROSYS_ENV_PLATFORM)

        term_hash_size = calc_term_stats_hash_size(num_devices -1);

#   endif

    stats_size = (num_devices * sizeof (TSTATS)) +
                 term_hash_size; 


#   if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        tstats_p = (TSTATS *)g_createshrmem(filnam, 
                                            stats_size, 
                                            &error);

#       if defined(PROSYS_ENV_PLATFORM)

            term_hash_p = (void *) (tstats_p + num_devices);

            init_term_stats_hash(term_hash_p, (ubyte_4)num_devices -1);

#       endif

#   else

        tstats_p = (TSTATS *)g_attachnewglb(filnam, glbnam, 'g',
                                            stats_size,
                                            &error);

#   endif

    if (error.errflg)
    {
        g_senderror(&error);
        return(P_FAILURE);
    }

    return(P_SUCCESS);
}

int create_events_section(int prdnbr, int num_events)
{
    ubyte_4 var_size;
    char filnam[MAX_FNAME_SIZE];
    char glbnam[MAX_FNAME_SIZE];

    var_size = (sizeof(EVENT_QUE_HDR) * 2) + (sizeof(RQST_EVENT) * num_events);
    p_makglbnam(prdnbr, EVENTS_SECT, filnam, glbnam);

#   if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        event_free_queue = (EVENT_QUE_HDR *) g_createshrmem(filnam,
                                                            var_size,
                                                            &error);

#   else

        event_free_queue = (EVENT_QUE_HDR *) g_attachnewglb(filnam, glbnam, 'g',
                                                            var_size,
                                                            &error);

#   endif

    if (error.errflg)
    {
        g_senderror(&error);
        return(P_FAILURE);
    }
    else
    {
        rpc_events_queue = event_free_queue + 1;
        request_events = (RQST_EVENT *) (rpc_events_queue + 1);
    }

    return(P_SUCCESS);
}


int atchglb(ubyte_4    prod_nbr,          /* product number  */
               char    tnicon_type,       /* tnicon section  */
               char    volcom_type,       /* volcom section  */
               char    gblcom_type,       /* gblcom section  */
               char    tstats_type,       /* tstats section  */
               char    events_type)       /* events section  */
{
    char filnam[MAX_FNAME_SIZE]; /* global section file name */
    char glbnam[MAX_FNAME_SIZE]; /* global section name */

/* attach to global sections */

    status=P_SUCCESS;

/* attach tnicon section */

    if(tnicon_type == 'w' || tnicon_type == 'r')
    {
        p_makglbnam(prod_nbr, TNICON_SECT, filnam, glbnam);

#       if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

            tnicon_p = (TNICON *) g_attachshrmem(glbnam, 
                                                 tnicon_type, 
                                                 &error);  

#       else

            tnicon_p = (TNICON *) g_attachglb(glbnam,
                                              'g', 
                                              tnicon_type, 
                                              &error);

#       endif

        if (error.errflg)
        {
           g_senderror(&error);
           return(P_FAILURE);
        }
    }

/* attach volcom section */

    if(volcom_type == 'w' || volcom_type == 'r')
    {
        p_makglbnam(prod_nbr, VOLCOM_SECT, filnam, glbnam);

#       if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

            volcom_p = (VOLCOM *) g_attachshrmem(glbnam, 
                                                 volcom_type, 
                                                 &error);

#       else

            volcom_p = (VOLCOM *) g_attachglb(glbnam,
                                              'g', 
                                              volcom_type, 
                                              &error);

#       endif

        if (error.errflg)
        {
           g_senderror(&error);
           return(P_FAILURE);
        }
    }

/* attach gblcom section */

    if(gblcom_type == 'w' || gblcom_type == 'r')
    {
        p_makglbnam(prod_nbr, GBLCOM_SECT, filnam, glbnam);

#       if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

            gblcom_p = (GBLCOM *) g_attachshrmem(glbnam,
                                                 gblcom_type,
                                                 &error);

#       else

            gblcom_p = (GBLCOM *) g_attachglb(glbnam,
                                              'g',
                                              gblcom_type,
                                              &error);

#       endif

        if (error.errflg)
        {
           g_senderror(&error);
           return(P_FAILURE);
        }
    }

/* attach tstats section */

    if(tstats_type == 'w' || tstats_type == 'r')
    {
        p_makglbnam(prod_nbr, TSTATS_SECT, filnam, glbnam);

#       if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

            tstats_p = (TSTATS *) g_attachshrmem(glbnam, 
                                                 tstats_type, 
                                                 &error);

#           if defined(PROSYS_ENV_PLATFORM)

                term_hash_p = (void *) (tstats_p + volcom_p->sys.mxtrm +1);

#           endif

#       else

            tstats_p = (TSTATS *) g_attachglb(glbnam,
                                              'g', 
                                              tstats_type, 
                                              &error);

#       endif

        if (error.errflg)
        {
           g_senderror(&error);
           return(P_FAILURE);
        }
    }

/* attach events section */

    if(events_type == 'w' || events_type == 'r')
    {
        p_makglbnam(prod_nbr, EVENTS_SECT, filnam, glbnam);

#       if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

            event_free_queue = (EVENT_QUE_HDR *) g_attachshrmem(glbnam,
                                                                events_type,
                                                                &error);

#       else

            event_free_queue = (EVENT_QUE_HDR *) g_attachglb(glbnam,
                                                             'g',
                                                             events_type,
                                                             &error);

#       endif

        if (error.errflg)
        {
           g_senderror(&error);
           return(P_FAILURE);
        }
        else
        {
            rpc_events_queue = event_free_queue + 1;
            request_events = (RQST_EVENT *) (rpc_events_queue + 1);
        }
    }

    return(status);
}

/* attach to all for readonly access */

int atchglb_all_read(ubyte_4 prod_nbr)
{
    status = atchglb(prod_nbr,'r','r','r','r','r');
    return(status);
}

/* attach to volcom for write access */

int atchglb_write_volcom(ubyte_4 prod_nbr)
{
    status = atchglb(prod_nbr,'r','w','r','r','r');
    return(status);
}

/* attach to events for readonly access */

int atchglb_read_events(ubyte_4 prod_nbr)
{
    status = atchglb(prod_nbr,'x','x','x','x','r');
    return(status);
}

/* attach to all for write access */

int atchglb_all_write(ubyte_4 prod_nbr)
{
    status = atchglb(prod_nbr,'w','w','w','w','x');
    return(status);
}

#endif
