/* ===[ LIBSORT.C ]========================================================
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

   Usage:
   
    ----------------------------------------------------------------------------------------- 
     void sort_create(sort_lun, length)
     
     Create unit to work with Memory stream records

       - sort_lun: Provide a number as Logical Unit (to be provided on all subsequent calls)
       - length:   Provide record Length
     Returns
       - If sort_lun = 0 it not was possible to create, 
                    <> 0 it was possible


    ----------------------------------------------------------------------------------------- 
     void sort_create_advanced(sort_lun, length, data_size, mem_keep_cnt)
     
     Create unit to work with Memory stream records

       - sort_lun:       Provide a number as Logical Unit (to be provided on all subsequent calls)
       - length:         Provide record Length
       - data_size:      Provide datasize block to store records (DEFAULT_DATA_SIZE = 65000 bytes)
       - mem_keep_cnt:   Provide how much blocks of data_size to keep on clear o delete 
                         (DEFAULT_MEM_KEEP = 200 blocks) 
     Returns
       - If sort_lun = 0 it not was possible to create, 
                    <> 0 it was possible


    -----------------------------------------------------------------------------------------                
     void sort_add(sort_lun, pointer_data, sts)

     Add a record to the memory stream

       - sort_lun:      Provide Logical Unit (used in sort_create)
       - pointer_data:  Where the data to add is
     Returns
       - sts:           Status, check all possible result status in SORT definitions
           
                 

    -----------------------------------------------------------------------------------------                
     int sort_get_len(sort_lun, length)
     
     gets the number of records in the memory stream
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
     Returns
       - length:        Returns number of records added
       
       
       
    -----------------------------------------------------------------------------------------                
     void sort_go_first(sort_lun, sts)
     
     go to the first record of the memory stream
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
     Returns
       - sts:           Status, check all possible result status in SORT definitions
       
       
       
    -----------------------------------------------------------------------------------------                
     void sort_get_next(sort_lun, pointer_data, sts)
     
     gets the next record of the memory stream
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
     Returns
       - pointer_data:  Where to put the data
       - sts:           Status, check all possible result status in SORT definitions
        

       
    -----------------------------------------------------------------------------------------                
     void sort_update_rec(sort_lun, pointer_data, sts)
     
     Update one record of the memory stream
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
     Returns
       - pointer_data:  New Data to update
       - sts:           Status, check all possible result status in SORT definitions



    -----------------------------------------------------------------------------------------                
     void sort_delete_record(sort_lun, index, sts)
     
     Deletes a record from the memory stream
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
       - index          Record Number
     Returns
       - sts:           Status, check all possible result status in SORT definitions
       

       
    -----------------------------------------------------------------------------------------                
     void sort_get_index(sort_lun, index, pointer_data, sts)

     gets a record based on the record number from the memory stream 
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
       - index:         Provide Index start by 0
     Returns
       - pointer_data:  Where to put the data
       - sts:           Status, check all possible result status in SORT definitions


             
    -----------------------------------------------------------------------------------------                
     void sort_clc_index(sort_lun, index, sts)

     calculates the record number from the last record readed 
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
     Returns
       - index:         Provide Index start by 0
       - sts:           Status, check all possible result status in SORT definitions
                    

       
    -----------------------------------------------------------------------------------------                
     void sort_index(sort_lun, keystart, keylen, ascending, keymode, sts)

     Index the the memory stream based on provided parameters 
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
       - keystart:      Offset where the key is
       - keylen:        Length of the key
       - ascending:     Ascending order (1=Ascending, 0=Descending) 
       - keymode:       
            NUMBER_MODE             Key is numeric, keylen should be 1,2,4,8 
            BYTE_ARRAY_MODE         Array of bytes 
            BYTE_ARRAY_INV_MODE     Array of bytes who's MSB is on the Right
            WORD_ARRAY_MODE         Array of Word  
            WORD_ARRAY_INV_MODE     Array of Word who's MSB is on the Right
            INT_ARRAY_MODE          Array of Integer 4  
            INT_ARRAY_INV_MODE      Array of Integer 4 who's MSB is on the Right
            INT8_ARRAY_MODE         Array of Integer 8
            INT8_ARRAY_INV_MODE     Array of Integer 8 who's MSB is on the Right
            BYTE_ARRAY_ALPHA_MODE   Alphabetic based
              
     Returns
       - pointer_data:  Where to put the data
       - sts:           Status, check all possible result status in SORT definitions
       
       
       
    -----------------------------------------------------------------------------------------                
     void sort_set_keylen(sort_lun, keylen, sts)

     Changes Length of key, that is used in the way that we want to sort using 8 bytes,
     but we need to locate the records using the first 4 bytes. 
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
       - keylen:        Length of the key
     Returns
       - sts:           Status, check all possible result status in SORT definitions
 


    -----------------------------------------------------------------------------------------                
     void sort_get_key(sort_lun, key_pointer, pointer_data, sts)

     gets a record based on the index key from the memory stream 
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
       - key_pointer:   Provide key to search by.
     Returns
       - pointer_data:  Where to put the data
       - sts:           Status, check all possible result status in SORT definitions



    -----------------------------------------------------------------------------------------                
     void sort_clear(sort_lun)
     
     Clear a memory stream data, but not deallocates, so it could be reused, to avoid malloc mess on the OS. 
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
     Returns
       - None                    
     

                    
    -----------------------------------------------------------------------------------------                
     void sort_close(sort_lun)
     
     closes a memory stream file, used in the case you want to close a single memory stream
     
       - sort_lun:      Provide Logical Unit (used in sort_create)
     Returns
       - None                    
                    

                    
    -----------------------------------------------------------------------------------------                
     void sort_close_all()
     
     closes all memory streams, normally do that when you finish yuor program
     
     Returns
       - None                 
       
       

   ======================================================================= */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>


#define MAX_HANDLER            40
#define DEFAULT_DATA_SIZE   65000
#define DEFAULT_MEM_KEEP      200

#define SORT_OK                 0
#define SORT_NOT_OPEN          -1
#define SORT_NO_MEMORY_AVAIL   -2
#define SORT_NOT_INDEXED       -3
#define SORT_EMPTY             -4
#define SORT_INV_KEYLEN        -5
#define SORT_NO_PREV_READ      -6
#define SORT_NO_MATCH          98
#define SORT_EOF               99

#define NUMBER_MODE             0
#define BYTE_ARRAY_MODE         1
#define BYTE_ARRAY_INV_MODE     2
#define WORD_ARRAY_MODE         3  
#define WORD_ARRAY_INV_MODE     4  
#define INT_ARRAY_MODE          5  
#define INT_ARRAY_INV_MODE      6  
#define INT8_ARRAY_MODE         8
#define INT8_ARRAY_INV_MODE     9  
#define BYTE_ARRAY_ALPHA_MODE  10


struct MEM_DATA
{
  int   cnt;	
  void* mem_ptr[1];
};    

struct SORT_DATA
{
 struct SORT_DATA* next;	
 struct SORT_DATA* prev;	 
 int elements;
 int len;
 int offset;
 unsigned char data[2];	
};

struct SORT_HANDLER
{
 struct SORT_DATA* first;
 struct SORT_DATA* last;    
 struct SORT_DATA* curradd;     
 struct SORT_DATA* current;
 struct SORT_DATA* prev_next;
 struct MEM_DATA*  mem_data;
 int    mem_cnt;
 int    data_size;
 int    prev_offset;        	 
 int    data_len;
 int    index_len;
 int    elements;
 int    capacity;
 int    curroffset;
 void*  tmpsort;
 void*  compare;
 int    keystart; 
 int    keylen; 
 int    keyslen; 
 int    keywlen; 
 int    keymode; 
 int    keyrepeats;
 int    ascending;
 char   error;
 char   opened; 
 char   sort; 
};


int initialized = 0;
struct SORT_HANDLER sort_handler[MAX_HANDLER];


/* ---[ VOID SORT_INITIALIZE ]---------------------------------------------
   Summary:
        sort_initialize()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_initialize()
{
  if(initialized) 
   return;	
   
  memset(sort_handler, 0, sizeof(sort_handler)); 
  initialized = 1; 
}

/* ---[ VOID SORT_FREE ]---------------------------------------
   Summary:
        sort_free()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_free(int* sort_h, void* ptr)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];
  
  if(s->mem_data->cnt >= s->mem_cnt)
   free(ptr);
  else
   s->mem_data->mem_ptr[s->mem_data->cnt++] = ptr;
}


/* ---[ VOID* SORT_MALLOC ]---------------------------------------
   Summary:
        sort_malloc()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void* sort_malloc(int* sort_h, size_t size)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];
    
  if(s->mem_data->cnt == 0)
    return(malloc(size));
  else
    return(s->mem_data->mem_ptr[--s->mem_data->cnt]);
}


/* ---[ VOID SORT_CREATE_ADVANCED ]---------------------------------------
   Summary:
        sort_create_advanced()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_create_advanced(int* sort_h, int *len, int *data_size, int *mem_keep_cnt)
{
  int i;
  struct SORT_HANDLER *s;
  	
  sort_initialize();

  if((*len > 20000) || (*len < 1))
  {
    *sort_h = -1;
    return;	 
  }
  
  if((*data_size > 2000000) || (*data_size < 10))
  {
    *sort_h = -2;
    return;	 
  }

  if((*mem_keep_cnt > 4000) || (*mem_keep_cnt < 10))
  {
    *sort_h = -3;
    return;	 
  }

  for(i=1; MAX_HANDLER>i; i++)
  {
    if(!sort_handler[i].opened) 
    {
      *sort_h = i;
      s = &sort_handler[*sort_h];
      s->opened        = 1;
      s->first         = NULL;
      s->last          = NULL;
      s->current       = NULL;  
      s->curradd       = NULL;
      s->tmpsort       = NULL; 
      s->mem_data      = NULL;      
      s->mem_cnt       = *mem_keep_cnt;      
      s->prev_next     = NULL;
      s->prev_offset   = 0;        	       
      s->data_len      = *len; 
      s->index_len     = 0; 	
      s->elements      = 0;
      s->data_size     = *data_size;
      s->capacity      = s->data_size / s->data_len;  	
      s->error         = 0;
      s->curroffset    = 0;
      s->sort          = 1;
      return;
    }		
  }
  
  *sort_h = 0;	
}


/* ---[ VOID SORT_CREATE ]---------------------------------------------
   Summary:
        sort_create()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_create(int* sort_h, int *len)
{
 int default_data_size = DEFAULT_DATA_SIZE;
 int mem_keep_cnt      = DEFAULT_MEM_KEEP;	
 
 sort_create_advanced(sort_h, len, &default_data_size, &mem_keep_cnt);
}

/* ---[ VOID SORT_ADD ]---------------------------------------------
   Summary:
        sort_add()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_add(int* sort_h, void *ptr, int* sts)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];
  
  if(s->error || !s->opened)
  {
    *sts = SORT_NOT_OPEN;	
    return;	
  }
  
  if(s->curradd == NULL)
  {
    s->mem_data      = malloc(sizeof(struct MEM_DATA) + (s->mem_cnt * sizeof(void*))); 
    if(s->mem_data == NULL) 
    {
     s->error = 1;	
     *sts = SORT_NO_MEMORY_AVAIL;
     return;	
    }    
    s->mem_data->cnt = 0;

    s->tmpsort = malloc(s->data_len);
    if(s->tmpsort == NULL) 
    {
     s->error = 1;	
     *sts = SORT_NO_MEMORY_AVAIL;
     return;	
    }    
    
    s->curradd = sort_malloc(sort_h, sizeof(struct SORT_DATA) + s->data_size); 
    if(s->curradd == NULL) 
    {
     s->error = 1;	
     *sts = SORT_NO_MEMORY_AVAIL;
     return;	
    }
        
    s->first = s->curradd;
    s->first->next = NULL;
    s->first->prev = NULL;    
    s->first->elements = 0;
    s->first->len      = 0;    
    s->first->offset   = 0;
    s->last = s->first;
  } 

  if(s->curradd->elements >= s->capacity)
  {
    s->curradd->next = sort_malloc(sort_h, sizeof(struct SORT_DATA) + s->data_size); 
    if(s->curradd->next == NULL)
    {
      s->error = 1;	
      *sts = SORT_NO_MEMORY_AVAIL;
      return;	
    }
    s->curradd->next->prev = s->curradd;
    s->curradd->next->next = NULL;
    s->curradd->next->elements = 0;
    s->curradd->next->len = 0;
    s->curradd->next->offset = s->elements; 
    s->curradd = s->curradd->next;  
    s->last = s->curradd;
  }
  
  memcpy(&s->curradd->data[s->curradd->len], ptr, s->data_len); 
  s->curradd->len += s->data_len;  	
  s->curradd->elements++;
  s->elements++;
  s->sort = 0;
  *sts = SORT_OK;
}

/* ---[ INT SORT_GET_LEN ]---------------------------------------------
   Summary:
        sort_get_len()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
int sort_get_len(int* sort_h, int* len)
{
 *len = sort_handler[*sort_h].elements;	

 return(*len);
}

/* ---[ VOID SORT_CLC_OFFSET ]---------------------------------------------
   Summary:
        sort_clc_offset()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_clc_offset(int* sort_h, int* index)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];	
 struct SORT_DATA* j;
 	
 j = s->current;
 if(j==NULL) 
   j = s->first; 
 
 if(j->offset > *index)
 {
  while (j->offset > *index) 
  {
   j = j->prev; 
   if(j==s->first) 
    break;
  }    	
 }
 else
 {
  while (*index >= (j->offset + j->elements) )
   j = j->next;
 } 
 
 s->current = j;  
 s->curroffset = ((*index) - j->offset) * s->data_len;  
}



/* ---[ VOID SORT_DELETE_RECORD ]-----------------------------------------
   Summary:
        sort_delete_record()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_delete_record(int* sort_h, int* index, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
 struct SORT_DATA* j;
 int    elements;
 	
 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 } 
  
 sort_clc_offset(sort_h, index);
 j = s->current; 	
 
 if((j->elements-1) > ((*index) - j->offset))
 {
  elements = j->elements - ((*index) - j->offset) - 1;
  memmove(&s->current->data[s->curroffset], &s->current->data[s->curroffset+s->data_len], s->data_len * elements);
 } 
 
 j->len -= s->data_len;  	
 j->elements--; 
 j = j->next;
 while(j != NULL)
 {
  j->offset--;
  j = j->next;
 } 


 j = s->current;
 if((j->elements==0) && (s->first->next != NULL)) 
 {
  if(s->first == j)
  {
   s->first = s->first->next;
   s->first->prev = NULL;
  } 
  else
  {
   j->prev->next = j->next;
   if(j->next == NULL)
    s->last = j->prev;
   else
    j->next->prev = j->prev;    	
  }
  sort_free(sort_h, j);
 }

 s->elements--;
 s->sort = 0; 
 
 s->prev_next = NULL;
 s->current   = NULL; 
 
 *sts = SORT_OK;
}


/* ---[ VOID SORT_COMPARE ]---------------------------------------------
   Summary:
        sort_compare()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
int sort_compare(int* sort_h, void* a, void* b)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];
  int   i;
  
  if(s->keymode == NUMBER_MODE)  
  {
   if(s->keywlen == 4)     /* Lets set 4 bytes the firt one, the most common option */
   {
    if((*(unsigned int*)a) > (*(unsigned int*)b)) return(1);
    if((*(unsigned int*)a) < (*(unsigned int*)b)) return(2);     
   }
   else if(s->keywlen == 1)
   {
    if((*(unsigned char*)a) > (*(unsigned char*)b)) return(1);
    if((*(unsigned char*)a) < (*(unsigned char*)b)) return(2);     
   }
   else if(s->keywlen == 2)
   {
    if((*(unsigned short int*)a) > (*(unsigned short int*)b)) return(1);
    if((*(unsigned short int*)a) < (*(unsigned short int*)b)) return(2);     
   }
   else if(s->keywlen == 8)
   {
    if((*(unsigned long int*)a) > (*(unsigned long int*)b)) return(1);
    if((*(unsigned long int*)a) < (*(unsigned long int*)b)) return(2);     
   }
  }
  else if(s->keymode == BYTE_ARRAY_MODE)  
  {
   for(i=0; s->keywlen>i; i++)
   {
    if(((unsigned char*) a)[i] > ((unsigned char*) b)[i]) return(1);
    if(((unsigned char*) a)[i] < ((unsigned char*) b)[i]) return(2);
   }
  }	
  else if(s->keymode == BYTE_ARRAY_INV_MODE)  
  {
   for(i=s->keywlen-1; i>=0; i--)
   {
    if(((unsigned char*) a)[i] > ((unsigned char*) b)[i]) return(1);
    if(((unsigned char*) a)[i] < ((unsigned char*) b)[i]) return(2);
   }
  } 	
  else if(s->keymode == WORD_ARRAY_MODE)  
  {
   for(i=0; s->keywlen>i; i++)
   {
    if(((unsigned short*) a)[i] > ((unsigned short*) b)[i]) return(1);
    if(((unsigned short*) a)[i] < ((unsigned short*) b)[i]) return(2);
   }
  }	  
  else if(s->keymode == WORD_ARRAY_INV_MODE)  
  {
   for(i=s->keywlen-1; i>=0; i--)
   {
    if(((unsigned short*) a)[i] > ((unsigned short*) b)[i]) return(1);
    if(((unsigned short*) a)[i] < ((unsigned short*) b)[i]) return(2);
   }
  } 	
  else if(s->keymode == INT_ARRAY_MODE)  
  {
   for(i=0; s->keywlen>i; i++)
   {
    if(((unsigned int*) a)[i] > ((unsigned int*) b)[i]) return(1);
    if(((unsigned int*) a)[i] < ((unsigned int*) b)[i]) return(2);
   }
  }	  
  else if(s->keymode == INT_ARRAY_INV_MODE)  
  {
   for(i=s->keywlen-1; i>=0; i--)
   {
    if(((unsigned int*) a)[i] > ((unsigned int*) b)[i]) return(1);
    if(((unsigned int*) a)[i] < ((unsigned int*) b)[i]) return(2);
   }
  } 	
  else if(s->keymode == INT8_ARRAY_MODE)  
  {
   for(i=s->keywlen-1; i>=0; i--)
   {
    if(((unsigned long int*) a)[i] > ((unsigned long int*) b)[i]) return(1);
    if(((unsigned long int*) a)[i] < ((unsigned long int*) b)[i]) return(2);
   }
  } 	
  else if(s->keymode == INT8_ARRAY_INV_MODE)  
  {
   for(i=s->keywlen-1; i>=0; i--)
   {
    if(((unsigned long int*) a)[i] > ((unsigned long int*) b)[i]) return(1);
    if(((unsigned long int*) a)[i] < ((unsigned long int*) b)[i]) return(2);
   }
  } 	  
  else // BYTE_ARRAY_ALPHA_MODE
  {
   for(i=0; s->keywlen>i; i++)
   {
    if((((unsigned char*) a)[i] | 0x20) > (((unsigned char*) b)[i] | 0x20)) return(1);
    if((((unsigned char*) a)[i] | 0x20) < (((unsigned char*) b)[i] | 0x20)) return(2);
   }
  }
  	
  return(0);	
}

/* ---[ VOID SORT_INV_COMPARE ]---------------------------------------------
   Summary:
        sort_compare()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
int sort_inv_compare(int* sort_h, void* a, void* b)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];
  int result = sort_compare(sort_h, a, b);
  
  if(s->ascending)
   return(result);

  if(result == 1)
   result = 2;
  else if(result == 2)
   result = 1;
  	
  return(result);	
}

/* ---[ VOID SORT_INV_COMPARE_IDX ]---------------------------------------------
   Summary:
        sort_inv_compare_idx()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
int sort_inv_compare_idx(int* sort_h, int a, int b)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];
  void* pointa; 
  void* pointb;
  
  sort_clc_offset(sort_h, &a);
  pointa = &s->current->data[s->curroffset + s->keystart];
   
  sort_clc_offset(sort_h, &b);
  pointb = &s->current->data[s->curroffset + s->keystart];
  
  return(sort_inv_compare(sort_h, pointa, pointb));
}


/* ---[ VOID SORT_CLC_INDEX ]---------------------------------------------
   Summary:
        sort_inv_compare_idx()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_clc_index(int* sort_h, int *index, int *sts)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];

 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 }   	
 
 if(s->prev_next==NULL)
 {
   *sts = SORT_NO_PREV_READ;	
   return;	
 }
 
 
 *index = s->prev_next->offset + (s->prev_offset / s->data_len);    

 *sts = SORT_OK;
}



/* ---[ VOID SORT_INV_DO_COMPARE ]---------------------------------------------
   Summary:
        sort_inv_do_compare()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
int sort_inv_do_compare(int* sort_h, int a)
{
  struct SORT_HANDLER *s = &sort_handler[*sort_h];
  void* pointa; 
  
  sort_clc_offset(sort_h, &a);
  pointa = &s->current->data[s->curroffset + s->keystart];
     
  return(sort_inv_compare(sort_h, pointa, s->compare));
}


/* ---[ VOID SORT_DOSWAP]---------------------------------------------
   Summary:
        sort_doswap()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_doswap(int* sort_h, int a, int b)
{
   struct SORT_HANDLER *s = &sort_handler[*sort_h]; 	
   void* pointa; 
   void* pointb;
   
   sort_clc_offset(sort_h, &a);
   pointa = &s->current->data[s->curroffset];
   
   sort_clc_offset(sort_h, &b);
   pointb = &s->current->data[s->curroffset];
      
   memcpy(s->tmpsort, pointa,     s->data_len);
   memcpy(pointa,     pointb,     s->data_len);
   memcpy(pointb,     s->tmpsort, s->data_len);      
}


/* ---[ INT SORT_QUICK_SRT]---------------------------------------------
   Summary:
        sort_quick_srt()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
int sort_quick_srt(int* sort_h, int iLo, int iHi)
{
  int Lo=iLo, Hi=iHi;
  int Mid = ((Lo + Hi) / 2);
  int result = 0;
   
  while(Hi >= Lo)
  {
   while (sort_inv_compare_idx(sort_h, Lo,Mid)==2) Lo++;
   while (sort_inv_compare_idx(sort_h, Hi,Mid)==1) Hi--;
   if( Lo <= Hi) 
   {
     if((Lo != Hi) && (sort_inv_compare_idx(sort_h, Lo, Hi) != 0))
     {
       result = 1;	
       sort_doswap(sort_h, Lo, Hi);
     } 
     Lo++;
     Hi--;
   }
  }
  
  if(Hi > iLo) sort_quick_srt(sort_h, iLo, Hi);
  if(Lo < iHi) sort_quick_srt(sort_h, Lo, iHi);	
  
  return(result);
}

/* ---[ INT SORT_CHECK_REPEATS]---------------------------------------------
   Summary:
        sort_check_srt()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
int sort_check_repeats(int* sort_h)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
 int i;
  
 for(i=0; (s->elements-1)>i ;i++)
 {	
  if (sort_inv_compare_idx(sort_h, i, i+1) == 0)
    return(1);
 }
	
 return(0);	
}


/* ---[ VOID SORT_SET_KEYLEN ]---------------------------------------------
   Summary:
        sort_set_keylen()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_set_keylen(int* sort_h, int* keylen, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
  	
 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 }   	
 
 if(*keylen > s->keyslen)
 {
   *sts = SORT_INV_KEYLEN;	
   return;	
 } 

 s->keylen    = *keylen;
 s->keywlen   = *keylen;
 

 if(s->keymode == NUMBER_MODE)  // Numeric
 {
   if((s->keylen != 1)	&&  (s->keylen != 2) && (s->keylen != 4) && (s->keylen != 8))
    s->keymode = BYTE_ARRAY_MODE;   // Byte Array
 }
 
 if((s->keymode == WORD_ARRAY_MODE) || (s->keymode == WORD_ARRAY_INV_MODE))
  s->keywlen   = (*keylen) / 2;
  
 if((s->keymode == INT_ARRAY_MODE) || (s->keymode == INT_ARRAY_INV_MODE))
  s->keywlen   = (*keylen) / 4;
  
 if((s->keymode == INT8_ARRAY_MODE) || (s->keymode == INT8_ARRAY_INV_MODE))
  s->keywlen   = (*keylen) / 8;   
  
 if(s->sort)
  s->keyrepeats = sort_check_repeats(sort_h);
    
 *sts = SORT_OK;  
}


/* ---[ VOID SORT_INDEX ]---------------------------------------------
   Summary:
        sort_index()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_index(int* sort_h, int* keystart, int* keylen, int* ascending, int* keymode, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
  	
 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 }   	
 
 s->keyslen    = *keylen;
 s->keystart   = *keystart;
 s->ascending  = *ascending;
 s->keymode    = *keymode;
 s->keyrepeats = 0;
 s->sort       = 0;

 sort_set_keylen(sort_h, keylen, sts);
 if(*sts != SORT_OK)
  return;
	  
 if(s->elements > 1)
  while(sort_quick_srt(sort_h, 0, s->elements-1) != 0);	
  
 s->sort = 1;
 s->keyrepeats = sort_check_repeats(sort_h);  
 *sts = SORT_OK;  
}


/* ---[ VOID SORT_GET_NEXT ]---------------------------------------------
   Summary:
        sort_get_next()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_get_next(int* sort_h, void* ptr, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
 int len;
 
 
 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 }  	


 if(s->current == NULL)
 {
   *sts = SORT_EOF;	
   return;
 }
 
 s->prev_next   = s->current;
 s->prev_offset = s->curroffset;       	  
 
 	
 memcpy( ptr, &s->current->data[s->curroffset], s->data_len);
 s->curroffset += s->data_len;

 if(s->curroffset >= s->current->len)
 {
  s->current = s->current->next;
  s->curroffset = 0;   	
 } 
    
 *sts = SORT_OK;	
}

/* ---[ VOID SORT_UPDATE_REC ]---------------------------------------------
   Summary:
        sort_update_rec()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_update_rec(int* sort_h, void* ptr, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
 int len;
 
 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 }  	

 if(s->prev_next == NULL)
 {
   *sts = SORT_EOF;
   return;
 }
 
 memcpy(&s->prev_next->data[s->prev_offset], ptr, s->data_len); 
 
 s->sort = 0; 

 *sts = SORT_OK;
}   	


/* ---[ VOID SORT_GO_FIRST ]---------------------------------------------
   Summary:
        sort_go_first()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_go_first(int* sort_h, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
 
 s->current = s->first;
 s->curroffset = 0;
 
}


/* ---[ VOID SORT_GET_INDEX ]---------------------------------------------
   Summary:
        sort_get_index()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_get_index(int* sort_h, int* index, void* ptr, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];	
 	
 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 }
 
 if(*index >= s->elements)
 {
   *sts = SORT_EOF;	
   return;	
 } 

 sort_clc_offset(sort_h, index);   
 sort_get_next(sort_h, ptr, sts);  	
}


/* ---[ VOID SORT_GET_REPEAT_KEY ]---------------------------------------------
   Summary:
        sort_get_repeat_key()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_get_repeat_key(int* sort_h, int* index, void* ptr, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];	
 int l,h,i,c,last_good;
 	
 if(!s->keyrepeats)
 {   	
  sort_get_index(sort_h, index, ptr, sts);		
  return;
 }  	
 
 last_good = *index;
 l = 0;
 h = *index;
 while (l <= h)
 { 
  i = (l + h) >> 1;
  c = sort_inv_do_compare(sort_h, i);
  if (c == 2)
   l = i + 1;
  else
  {
   h = i - 1;     
   if (c == 0) 
    last_good = i;
  } 
 }

 sort_get_index(sort_h, &last_good, ptr, sts);    
}

/* ---[ VOID SORT_GET_KEY ]---------------------------------------------
   Summary:
        sort_get_key()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_get_key(int* sort_h, void* key, void* ptr, int* sts)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];	
 int l,h,i,c,place; 	
 int result;
 	
 if(!s->opened)
 {
   *sts = SORT_NOT_OPEN;	
   return;	
 }
 
 if(!s->sort)
 {
   *sts = SORT_NOT_INDEXED;	
   return;	
 }
 
 s->compare = key;
 
 if(s->elements == 0)
 {
  *sts = SORT_EMPTY;
  return;	
 }

 if(s->elements == 1) 
 {
   c=sort_inv_do_compare(sort_h, 0);   
   sort_go_first(sort_h, sts);
   sort_get_next(sort_h, ptr, sts);
   
   if(c!=0)
    *sts = SORT_NO_MATCH;
    	   
   return; 
 }
   
 l = 0;
 h = s->elements - 1;
 place = h;
 while (l <= h)
 { 
   i = (l + h) >> 1;
   c = sort_inv_do_compare(sort_h, i);
   if (c == 2)
    l = i + 1;
   else
   {
    h = i - 1;     
    place = i;
    if (c == 0) 
    {
     sort_get_repeat_key(sort_h, &i, ptr, sts);	
     return;
    }
   }
 } 

 sort_get_index(sort_h, &place, ptr, sts);   	   
 return;     		
}

/* ---[ VOID SORT_CLEAR ]---------------------------------------------
   Summary:
        sort_clear()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_clear(int* sort_h)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
 struct SORT_DATA* j;
 void*  n; 

 sort_initialize();  
  
 if(!s->opened)
 {
   return;	
 }	
  
 if(s->last != NULL) 
 {
  j = s->last;
  while(j != s->first)
  {
   n = j;
   j = j->prev;
   sort_free(sort_h, n);	
  }
 }
 
 s->last            = s->first;
 s->current         = NULL;  
 s->curradd         = s->first;
 s->prev_next       = NULL;
 s->prev_offset     = 0;        	       
 s->index_len       = 0; 	
 s->elements        = 0;
 s->error           = 0;
 s->curroffset      = 0;
 s->sort            = 1; 
 s->first->next     = NULL;
 s->first->prev     = NULL;    
 s->first->elements = 0;
 s->first->len      = 0;    
 s->first->offset   = 0;
 
 return;   
}


/* ---[ VOID SORT_CLOSE ]---------------------------------------------
   Summary:
        sort_close()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_close(int* sort_h)
{
 struct SORT_HANDLER *s = &sort_handler[*sort_h];
 struct SORT_DATA* j;
 void*  n; 
 int    i;

 sort_initialize();  
  
 if(!s->opened)
 {
   return;	
 }	
 
 if(s->tmpsort != NULL)
  free(s->tmpsort);
  
 if(s->mem_data != NULL)
 {
  for(i=0; s->mem_data->cnt>i; i++)
   free(s->mem_data->mem_ptr[i]);	

  free(s->mem_data);
 } 
 
 if(s->last == NULL) 
 {
   s->opened = 0;	 
   return; 	
 }
  
 j = s->last;
 while(j != NULL)
 {
   n = j;
   j = j->prev;
   free(n);	
 }

 s->opened = 0;	  
}

/* ---[ VOID SORT_CLOSE_ALL ]---------------------------------------------
   Summary:
        sort_close_all()
   Description:
   Return Value:
   Caveats:
   Example:
   Revisions:
      REV     DATE      BY       DESCRIPTION
      ----  --------    ---      -----------------------------------------
   ----------------------------------------------------------------------- */      
void sort_close_all()
{
  int i;
  
  sort_initialize();  
  	
  for(i=0; MAX_HANDLER>i; i++)
  {
    if(sort_handler[i].opened)
      sort_close(&i);  
  }   	
}




