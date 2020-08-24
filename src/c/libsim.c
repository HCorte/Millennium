/* ===[ LIBEUSIM.C ]========================================================
   Description:
   Functions:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  ---------   ---      -----------------------------------------
   -----------------------------------------------------------------------
     This item is the property of GTECH Corporation, Providence,
     Rhode Island, and contains confidential and trade secret information.
     It may not be transfered from the custody or control of GTECH except
     as authorized in writing by an officer of GTECH.  Neither this item
     nor the information it contains may be used, transfered, reproduced,
     published, or disclosed, in whole or in part, and directly or
     indirectly, except as expressly authorized by an officer of GTECH,
     persuant to written agreement.
   -----------------------------------------------------------------------
   ======================================================================= */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>


#define READ_BUFFER      300
#define SIMFILE          "gxutl:simfile.fil" 
///#define SIMFILE          "./simfile.fil" 

#define CHECKSUM_OFFSET  2

#define TRUE  1
#define FALSE 0

#define ISDIGIT(c)  ( ((c) >= '0') && ((c) <= '9') )
#define ISHDIGIT(c)  ( ((c) >= 'a') && ((c) <= 'f') )

int last_checksum = 0;

#define LEN_DATA_SIM     12500
#define MAX_MESSAGES       200

int            nmsgs;
char           *simtab[MAX_MESSAGES];
int            simlen[MAX_MESSAGES];
char           data_sim[LEN_DATA_SIM];
unsigned short csum_store[MAX_MESSAGES];
int            msg_cnt;

struct _trans {                 /* structure to control message cycling */
   unsigned char thisone;       /* the current message search element   */
   unsigned char total;         /* the total # of msgs of this type     */
   unsigned char firstone;      /* the first occurence for faster search*/
} trans[16][16];

struct SimFile {
   unsigned char  SimType;                   /* type to match */
   unsigned char  SimControl;                /* dummy - actual copied from input */
   unsigned char  Response[256];             /* ...rest of message */
};

#define PBL_URGENT   1
#define PBL_INFO     2
#define PBL_DEBUG    3

#define TEST_PC      FALSE
#define TEST         FALSE

#if TEST_PC
 void getccitt(char *bary, int *begoff, int *numbyt, int *result)
 {
 }
#else
 void getccitt();
#endif

/* ---[ VOID DEBUG ]----------------------------------------------------
   Summary:
        void debug()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */         
void debug(int level, const char *fmt,  ...)
{	
 va_list ap;	

/*
 char n_template[300];
 
 sprintf(n_template,"Lvl:%2d msg:%s", level, fmt);
  
 va_start (ap, n_template);    
 vprintf(n_template, ap);	
 va_end (ap);	
*/ 

#if TEST
 va_start (ap, fmt);    
 vprintf(fmt, ap);	
 va_end (ap);	
#endif 
}

/* ---[ VOID PRINTMSG ]------------------------------------------------
   Summary:
        void printmsg()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */            
void printmsg(unsigned char *buf, int *len)
{
   char s[100],t[4],an[6];
   int i;

   s[0] = 0;      
   sprintf(an,"MSG:");   
	 
   for(i=0; *len>i ;i++)	
   {
     sprintf(t,"%02x ",buf[i]);	
     strcat(s,t);	  
     if(strlen(s) > 70) 
     {
      printf("%s%s\n",an,s);
      sprintf(an,"    "); 
      s[0] = 0;	       
     }   
   }	

   if(strlen(s) != 0) 
   {
    printf("%s%s\n",an,s);
   }
}


/* ---[ INT CHECK_CHECKSUM ]------------------------------------------------
   Summary:
     int check_checksum()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
long int check_checksum(FILE* fid, long int *length)
{
  int       clc_chksum = 0;	
  char      buffer[READ_BUFFER];
  int       i;
  long int  t_readed=0;

  fseek(fid,0,SEEK_SET);
    
  while (fgets (buffer, READ_BUFFER, fid))
  {
//   printf("ReadedX: %d\n",strlen(buffer));   
   for(i=0; strlen(buffer)>i ;i++)
    clc_chksum += (buffer[i] * (++t_readed % 0x0F));
  }

  debug(PBL_INFO,"Checksum:%08X Length:%ld\n",clc_chksum,t_readed);
  *length = t_readed;

  return(clc_chksum);
}

/* ---[ INT DIGITHEX ]----------------------------------------------------
   Summary:
        int digithex()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
int digithex(char digit)
{
  if(ISDIGIT(digit))	
   return(digit-'0');
  
  if(ISHDIGIT(digit | 0x20))	
   return(((digit|0x20)-'a')+10); 
   
  return(0); 
}
         
/* ---[ INT HEXTOSTR ]----------------------------------------------------
   Summary:
        int hextostr()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */         
int hextostr(char *digits)
{	
  return (digithex(*digits)*0x10 + digithex(*(digits+1)));	
}

/* ---[ INT LENDIGIT ]----------------------------------------------------
   Summary:
        int lendigit()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */         
int lendigit(char *digit, int len_max, int hexa)
{
  int i;
  
  for(i=0; len_max>i; i++)
   if(!ISDIGIT(*(digit+i)))
   {       
    if(hexa)    
    {
     if(!ISHDIGIT((*(digit+i)) | 0x20))
      return(i);	
    }  
    else
     return(i);	
   }  
   
   return(len_max);
}

/* ---[ LONG INT INTTOSTR ]----------------------------------------------------
   Summary:
        long int inttostr()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */         
long int inttostr(char *digits,int len)
{
  char buf[12];
  
  memcpy(buf,digits,len);
  buf[len]='\0';

  return(atoi(buf));
}

/* ---[ UBYTE_4 GET_DATA_SIMTAB ]----------------------------------------------------
   Summary:
        ubyte_4 get_data_simtab
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
unsigned int get_data_simtab(int element)
{
  unsigned int   clc = (*simtab[element] * 0x10000);
  unsigned short subindex;

  memcpy(&subindex,&simtab[element][CHECKSUM_OFFSET],2); 

  return (clc + subindex);
}

/* ---[ VOID QUICK_SORT ]----------------------------------------------------
   Summary:
        int quick_sort()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
void quick_sort(int ilo, int ihi)
{
  int            lo,hi,len;
  unsigned short tmp_csum;
  unsigned int   mid;
  char           *tem;

  lo = ilo;
  hi = ihi;

  mid = get_data_simtab((lo + hi)/2);  
  while (hi >= lo)
  {
   while (get_data_simtab(lo) < mid) lo++;
   while (get_data_simtab(hi) > mid) hi--;
   if (lo <= hi)
   {
    tem = simtab[lo];
    simtab[lo] = simtab[hi];
    simtab[hi] = tem;

    len = simlen[lo];
    simlen[lo] = simlen[hi];
    simlen[hi] = len;

    tmp_csum = csum_store[lo];
    csum_store[lo] = csum_store[hi];
    csum_store[hi] = tmp_csum;

    lo++;
    hi--;
   }
  } 

  if (hi > ilo)
    quick_sort(ilo, hi);
  if (lo < ihi)
    quick_sort(lo, ihi); 
}  

/* ---[ VOID SORT_MESSAGES ]----------------------------------------------
   Summary:
        void sort_messages()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */         
void sort_messages()
{
  unsigned short n;

  if(nmsgs==1)
   return;

  for(n=0; nmsgs>n; n++) {                      /* Use checksum space to set the second index */
    memcpy(&csum_store[n],&simtab[n][CHECKSUM_OFFSET+1],2);    /* save data before using */
    memcpy(&simtab[n][CHECKSUM_OFFSET+1],&n,2);
  }

  quick_sort(0,nmsgs-1);

  for(n=0; nmsgs>n; n++) {
    /* restore saved csums, which will be overwritten anyway or in case of unso news which 
     * has no csum, restore the first two bytes of message */
    memcpy(&simtab[n][CHECKSUM_OFFSET+1],&csum_store[n],2);
  }
}

/* ---[ VOID REVBYTE ]--------------------------------------------------
   Summary     :
   Description :
   Return Value:
   Caveats     :
   Example     :
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
void revbyte(char *dest, char *src, unsigned char size)
{
    for (; size; size--)
        *dest++ = src[size - 1];

    return;
}


/* ---[ VOID LOAD_MEMORY ]----------------------------------------------------
   Summary:
        void load_memory()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */            
void load_memory(FILE* fid, unsigned long length_file)
{
#define MARGIN 0
  char             buffer[READ_BUFFER];
  size_t           l_buffer = sizeof(buffer);
  unsigned long    i=0,position=0;
  int      	   mute=FALSE,quotes=FALSE;
  char             *ptr=buffer,*last_ptr;
  char             *ptr_data=data_sim;
  int              len,overflow = FALSE;
  char             t_byte;
  unsigned short   t_word;
  long int         t_long;

  fseek(fid,0,SEEK_SET);
  if(fgets(buffer, READ_BUFFER, fid))
   l_buffer = strlen(buffer);
  else
   l_buffer = 0;

  nmsgs = 0;

  while(length_file>i)
  {    	
    if(i>=((position+l_buffer-MARGIN)))
    {
      position = i - MARGIN;

      if(fgets(buffer, READ_BUFFER, fid))
       l_buffer = strlen(buffer);
      else
       l_buffer = 0;
      
      ptr = buffer + MARGIN;
    }
    last_ptr = ptr;    

    if(*ptr=='#')
     mute=TRUE;

    if(*ptr=='"')
    {
     if((quotes) && (*(ptr+1)=='"'))
     {
      if(nmsgs!=0)	
        *ptr_data++ = *ptr++;
     } 
     else
      quotes=!quotes;
    }
    if((*ptr==0x0A) || (*ptr==0x0D))
     mute=FALSE;

    if((quotes) && (*ptr!='"'))
    {
     if(nmsgs!=0)	
       *ptr_data++ = *ptr++;
    } 
    else
    {
     if((!mute) && (*ptr!=0x0A) && (*ptr!=0x0D) && (*ptr!=' ') && (*ptr!=0x09) && (!quotes))
     {
      switch (*ptr)
      {
	 case 'x':
	 case 'X':
      	   if((lendigit(ptr+1,2,TRUE)==2) && (nmsgs!=0)) 
      	   {
      	     *ptr_data++ = hextostr(ptr+1);
      	     ptr += 2;
      	   }
      	  break;

      	 case 'b':
      	 case 'B':
	  if(((len = lendigit(ptr+1,10,FALSE)) >0) && (nmsgs!=0)) 
	  {
      	   t_byte = (unsigned char)inttostr(ptr+1,len);
      	   *ptr_data++ = t_byte;
      	   ptr += len;
      	  }
      	  break;
      	  
      	 case 'w':
      	 case 'W':
      	  if(((len = lendigit(ptr+1,10,FALSE)) >0) && (nmsgs!=0))  
	  {
      	   t_word = (unsigned short)inttostr(ptr+1,len);
      	   revbyte(ptr_data,(char *)&t_word,2);
      	   ptr_data+=sizeof(t_word);
      	   ptr += len;
      	  } 
      	  break;

      	 case 't':
      	 case 'T':
      	  if(((len = lendigit(ptr+1,10,FALSE)) >0) && (nmsgs!=0))  
	  {	  	
      	   t_long = inttostr(ptr+1,len);
      	   revbyte(ptr_data,(char *)&t_long,3);
      	   ptr_data+=3;
      	   ptr += len;
      	  } 
      	  break;
      	  
      	 case 'l':
      	 case 'L':
      	  if(((len = lendigit(ptr+1,10,FALSE)) >0) && (nmsgs!=0))  
	  {
     	   t_long = inttostr(ptr+1,len);
      	   revbyte(ptr_data,(char *)&t_long,4);
      	   ptr_data+=sizeof(t_long);
      	   ptr += len;
      	  } 
      	  break;

      	 case ':':
      	   if(lendigit(ptr+1,2,TRUE)==2)
      	   {
      	     nmsgs++;      	           	   	
	     simtab[nmsgs-1] = ptr_data;
             if(nmsgs > 2) 
	       simlen[nmsgs-2] = (ptr_data - simtab[nmsgs-2]);
             else if(nmsgs==2)
	       simlen[nmsgs-2] = ptr_data - data_sim;   
      	     *ptr_data++ = hextostr(ptr+1);
       	     ptr += 2;
      	   }
      	   break;
      }
     }
     ptr++;
    }
    i += (ptr - last_ptr);
    
    if((ptr_data - data_sim) >= LEN_DATA_SIM)
    {
      debug(PBL_URGENT,"ERROR! Data overflow, sim file too long\n");
      overflow = TRUE;
      nmsgs--;
      break;
    }
    
    if(nmsgs >= MAX_MESSAGES)
    {
      debug(PBL_URGENT,"ERROR! Data overflow, sim file with too messages\n");
      overflow = TRUE;
      nmsgs--;
      break;
    } 
  }

  if((!overflow) && (nmsgs>0))
  {
    if(nmsgs > 1)
     simlen[nmsgs-1] = ptr_data - (simtab[nmsgs-1]);
    else
     simlen[nmsgs-1] = ptr_data - data_sim;
  }

  sort_messages(); 


#if 0
  /* Which data */
  debug(PBL_DEBUG,"Data: \n");
  for(i=0; (ptr_data-data_sim)>i; i++)
    debug(PBL_DEBUG,"data_sim[%d] %02X\n",i,(unsigned char)data_sim[i]);
#endif  

#if 0
  /* Show Messages*/
  debug(PBL_DEBUG,"num messages: %d\n",nmsgs);  
  for(i=0; nmsgs>i; i++)
  {
   debug(PBL_DEBUG,"len:%02d %3d\n",i+1,simlen[i]);        
   printmsg((unsigned char*)&simtab[i][0], &simlen[i]);   
  } 
#endif  
}

/* ---[ INT INDEX_SIM_TABLE ]----------------------------------------------
   Summary:
       void index_sim_table()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
void index_sim_table()
{
   unsigned int i,j;
   struct SimFile *simp;


   debug( PBL_DEBUG, "index_sim_table --- 2 \n");


   for (i = 0; i < 16; i++){
       for (j = 0; j < 16; j++){
           trans[i][j].total    = 0;
           trans[i][j].thisone  = 0;   
           trans[i][j].firstone = 0xFF;
       }
   }

   /*-----------------------------------------*/
   /* count up the total for each one present */
   /*-----------------------------------------*/
   for (i = 0; i < nmsgs; i++){

       simp = (struct SimFile *)simtab[i];
       
       trans[(simp->SimType >> 4)][(simp->SimType & 0x0f)].total++;

      /* if the type subtype is new then locate it's offset for quick access */
      if (trans[(simp->SimType >> 4)][(simp->SimType & 0x0f)].firstone == 0xFF)
      {
       trans[(simp->SimType >> 4)][(simp->SimType & 0x0f)].firstone = i;
       trans[(simp->SimType >> 4)][(simp->SimType & 0x0f)].thisone = i;
      }    
   }


   debug( PBL_DEBUG, "index_sim_table --- 2 \n");

   return;
}



/* ---[ INT CHECK_SIM_DATA ]------------------------------------------------
   Summary:
        int check_sim_data()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */            
void check_sim_data(int *initialize, int *sts)
{
  FILE*    fid;
  long int clc_chksum;
  long int length_file;
  
  *sts = FALSE;
  
  if(*initialize)
    last_checksum = -1;

  if((fid = fopen(SIMFILE,"r")) == NULL)
   return;  
  
  *sts = TRUE;

  clc_chksum = check_checksum(fid, &length_file); 
  if(clc_chksum == last_checksum)
  {
   fclose(fid); 
   return;	
  } 

  debug(PBL_URGENT,"Checksum doesn't match, let's reload file\n");
  
  load_memory(fid, length_file);

  debug(PBL_URGENT,"File reloaded\n");

  last_checksum = clc_chksum;

  fclose(fid);
 
  index_sim_table(); 
      
  return;	
}


/* ---[ void FILL_CHECKSUM ]--------------------------------------------------
   Summary:
        void fill_checksum()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
void fill_checksum(char *inpbuf, char *outbuf, int *outlen)
{
  int begoff = 1;
  int outln = *outlen - 1;
  int result;
  
  outbuf[0] = inpbuf[0];
  outbuf[2] = inpbuf[2];
  outbuf[3] = inpbuf[3];  

  getccitt(outbuf, &begoff, &outln, &result);
//  printf("chk:%d %04x %02x %02x \n",outln,result,inpbuf[2],inpbuf[3]);
  
  outbuf[2] = (result >> 8) & 0xFF;
  outbuf[3] = result & 0xFF;
}


/* ---[ void FILL_ERROR ]--------------------------------------------------
   Summary:
        void fill_error()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */
void fill_error(char *inpbuf, char *outbuf, int *outlen, int *code)
{
   char* ob = outbuf;

   debug( PBL_DEBUG, "send_error:%d\n", *code);

   *ob++ = inpbuf[0];
   *ob++ = 0x90;  /* force ERROR type response */
   *ob++ = 0x00;  /* 2 bytes checksum */
   *ob++ = 0x00;
   *ob++ = (unsigned char)*code;
   *ob++ = (unsigned char)0;
   *outlen = ob - outbuf; 
   fill_checksum(inpbuf, outbuf, outlen);

   return;
}



/* ---[ VOID GET_RESPONSE ]------------------------------------------------
   Summary:
        void get_response()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */            
void get_response(char *inpbuf, char *outbuf, int *outlen, int *inpmode)
{
  int  ob, error_num = 1;	
  int  mode = FALSE, sts;	
  struct _trans* trn = &trans[(inpbuf[1]>> 4)][(inpbuf[1] & 0x0f)];

  if(*inpmode == 2)
   return;
     
  check_sim_data(&mode, &sts);
     
  if (trn->total == 0)
  {
    if(*inpmode == 0)
    {	
     fill_error(inpbuf, outbuf, outlen, &error_num);	    
     *inpmode = 1;
    } 
    else
     *inpmode = 2;  
    return;
  }
      
  if(*inpmode == 0)
   msg_cnt = 0;   
  else
  {
   if(msg_cnt >= trn->total) 
   {
    *inpmode = 2;
    return;	     	
   }	
  }  

  ob = trn->thisone; 
  if(inpbuf[1] != simtab[ob][0])
  {
    *inpmode = 2;
    return;	
  }

  (*outlen) = (simlen[ob] - 1);
  memcpy(outbuf, &simtab[ob][1], (*outlen)); 
  fill_checksum(inpbuf, outbuf, outlen);

  trn->thisone++;
  if((trn->thisone - trn->firstone) >= trn->total)
   trn->thisone = trn->firstone;  	
  
  msg_cnt++;
  *inpmode = 1;

  return;	
}


/* ---[ int SELECTED_RESPONSE ]------------------------------------------------
   Summary:
        int selected_response()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */  
int selected_response(char *inpbuf, char *outbuf, int *outlen)
{
  /* Pending for reports*/	
  
  if(inpbuf[1] == 0x63)
  {
//    printf("cl sb %d %d %d %d\n",inpbuf[7],inpbuf[8],outbuf[7],outbuf[8]);	
    if((inpbuf[7] == outbuf[7]) && (inpbuf[8] == outbuf[8]))
    {
     return TRUE; 
    }     
  }	
  else
   return TRUE;	
   
   
  return FALSE; 
}




/* ---[ VOID SELECT_RESPONSE ]------------------------------------------------
   Summary:
        void select_response()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */            
void select_response(char *inpbuf, char *outbuf, int *outlen, int *inpmode)
{
 int error_num = 1;
 	
 for(;;)
 { 
  get_response(inpbuf, outbuf, outlen, inpmode);
  if(*inpmode == 2)
  {
   fill_error(inpbuf, outbuf, outlen, &error_num);	
   return;
  }
  
  if(selected_response(inpbuf, outbuf, outlen))
  {
   return;
  }  
 }
}

/* ---[ VOID SETSERIALNUM ]------------------------------------------------
   Summary:
        void setserualnum()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */            
void setserialnum(int *offset, char *buf, int *serialnum)
{
  int ser = (*serialnum >> 8) + 1;
  int chk = (*serialnum & 0xFF) + 1;
  
  if(chk > 100) chk = 1;
      
  *serialnum = (ser << 8) + chk;
    
  revbyte(&buf[(*offset)-1], (char*)serialnum, (unsigned char)4);	
}

/* ---[ VOID SETTIME ]------------------------------------------------
   Summary:
        void settime()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */            
void settime(int *offset, unsigned char *buf, int *actime)
{
  int hrs  = (*actime) / 3600;
  int min  = ((*actime) - (hrs * 3600)) / 60;
  int sec  = (*actime) - ((hrs * 3600) + (min * 60));
  int off  = (*offset)-1;
  
//  printf("%d:%d:%d   %d\n",hrs,min,sec,*actime);
  
  
  buf[off++] = hrs;
  buf[off++] = min;
  buf[off++] = sec;
    
}




#if TEST   // Test Routine 
#if TEST_PC
 main()
#else
 void test_sim()
#endif
{
 int  a=TRUE, sts;	
 char inpbuf[30];
 unsigned char outbuf[100];
 int  outlen, inpmode;

// check_sim_data(&a, &sts);	 
// debug(PBL_URGENT,"sts: %d\n",sts);
 
 a = 0;
 inpbuf[a++]=0x20;
 inpbuf[a++]=0x10;

 inpbuf[a++]=0x00;
 inpbuf[a++]=0x00;
 
 inpbuf[a++]=0x00;
 inpbuf[a++]=0x00;
 inpbuf[a++]=0x00; 

 inpbuf[a++]=0x08;
 inpbuf[a++]=0x00;
 
 
 printf("Start Looking\n");	
 inpmode = 0;
 for(;;)
 { 
  select_response(inpbuf, (char*)outbuf, &outlen, &inpmode);
//  get_response(inpbuf, (char*)outbuf, &outlen, &inpmode);


  printf("len:%d\n",outlen);
  printmsg(outbuf, &outlen);
  
  if(inpmode == 2)
  {
    printf("Any other one\n");	
    break;	
  }  
 }
 
}
#endif
