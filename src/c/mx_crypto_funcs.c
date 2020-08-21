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
/* Copyright 2005 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[MX_CRYPTO_FUNCS.C]=====================================================*/
/*                                                                            */
/* Purpose: Functions related to Hash calculation                             */
/*                                                                            */
/* Functions:                                                                 */
/* Generate_Salt_Data                                                         */
/* Generate_Msg_Digest                                                        */
/*                                                                            */
/* Load_Exchange_Enc_Key                                                      */
/* Decrypt_Client_Key                                                         */
/* Decrypt_Data                                                               */
/* Encrypt_Data                                                               */
/*                                                                            */
/*====[MX_CRYPTO_FUNCS.C]=====================================================*/
/*                                                                            */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stddef.h>
#include <errno.h>
#include <netinet/in.h>

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)
#   include <sys/time.h>
#   include "crypt_lib.h"
#   include "openssl/evp.h"
#   include "openssl/rsa.h"
#   include "openssl/bio.h"
#   include "openssl/x509.h"
#   include "openssl/pem.h"
#   include "openssl/err.h"
#   include "openssl/engine.h"
#endif

#include "includes_mbuf.h"

static char  ssl_error[1024];

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

#   define SSL_ERROR                                         \
    do                                                       \
    {                                                        \
        memset(ssl_error, 0x00, sizeof(ssl_error));          \
        ERR_load_crypto_strings();                           \
        snprintf(ssl_error, sizeof(ssl_error)-1,             \
                 "%s",                                       \
                 ERR_reason_error_string(ERR_get_error()));  \
        ERR_free_strings();                                  \
    }                                                        \
    while(0)

#endif

/* Start of DEBUG function */
/*
  static char                buffer[2048];
  char                hexvalue[] = "0123456789abcdef";

  char *
  strhex(const void *vhex, byte_4 len, char *sbuf)
  {
      char               *buf = sbuf;
      byte_4              i;
      byte_4              nibble;
      ubyte_1            *hex = (ubyte_1 *) vhex;

      for (i = 0; i < len; i++, hex++)
      {
          nibble = (*hex) >> 4;
          *buf++ = hexvalue[nibble];
          nibble = (*hex) & 0x0F;
          *buf++ = hexvalue[nibble];
      }
      *buf++ = '\0';
      return sbuf;
  }
*/
/* End of DEBUG function */

/* [Generate_Salt_Data]
 *
 * Summary:
 *
 * void Generate_Salt_Data(unsigned char *buf, unsigned int len
 *
 * Description:
 *
 * This function generates "len" byte random string and copies to "buf" 
 * The "buf" parameter is assumed to be atleast "len" size
 *
 * Returns Values:
 *
 * None
 *
 */


void
Generate_Salt_Data(unsigned char *buf, unsigned int len)
{
    struct timeval      tval;
    static unsigned int seed =0;
    int                 randnum;
    unsigned char      *rp;
    int                 i;
    int                 j;
    int                 k;
    int                 current_cdc;

    /* Generate seed if not done already */
    if(seed == 0)
    {
        seed = getpid();

#       if defined(PROSYS_ENV_ALL)

            current_cdc = gblcom_p->hdr.cdc;

#       elif defined(GOLS_ENV_ALL)

            GET_CDC (&current_cdc);

#       endif

        seed += current_cdc;
        gettimeofday(&tval, NULL);
        seed += tval.tv_usec;

        /* seed the RNG */
        srand(seed);
    }

    /* Generate "len" byte random string */
    i = 0;
    k = sizeof(randnum);
    rp = (unsigned char *) &randnum;

    while (i < len)
    {
        randnum = rand();
        for (j = 0; j < k && i < len; j++)
        {
            if (rp[j])
            {
                buf[i] = rp[j];
            }
            i++;
        }
    }


}


/* [Generate_Msg_Digest]
 *
 * Summary:
 *
 * int Generate_Msg_Digest(unsigned int hash_alg_code, unsigned char *msg, unsigned int len,
 *                         unsigned char *md_value, unsigned int *md_len)
 *
 * hash_alg_code - Hash Algorithm Code 
 * msg - Message for which digest has to be calculated
 * len - Message len
 * md_value - message digest output
 * md_len - length of digest
 *
 * Description:
 *
 * This function generates message digest based on hash_alg_code
 * Currently MD5 and SHA1 are supported.
 * md_value should be atleast MAX_HASH_VALUE_LENGTH size
 *
 * Returns Values:
 * 1 ==> Success
 * 0 ==> Invalid algorithm code
 *
 */

int
Generate_Msg_Digest(unsigned int hash_alg_code, unsigned char *msg, unsigned int len, unsigned char *md_value, unsigned int *md_len)
{
    int                 rstat = 0;
#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)
    EVP_MD_CTX          mdctx;
    const EVP_MD       *md;
    char                md_name[10];

    switch (hash_alg_code)
    {
    case MD5_HASH:
        snprintf(md_name, sizeof(md_name), "MD5");
        break;

    case SHA1_HASH:
        snprintf(md_name, sizeof(md_name), "SHA1");
        break;

    case SHA256_HASH:
        snprintf(md_name, sizeof(md_name), "SHA256");
        break;

    case SHA384_HASH:
        snprintf(md_name, sizeof(md_name), "SHA384");
        break;

    case SHA512_HASH:
        snprintf(md_name, sizeof(md_name), "SHA512");
        break;

    default:
        printf("Invalid hash_alg_code %d \n", hash_alg_code);
        return 0;
    }

    OpenSSL_add_all_digests();

    md = EVP_get_digestbyname(md_name);
    if (md != NULL)
    {
        EVP_MD_CTX_init(&mdctx);
        if (1 == EVP_DigestInit_ex(&mdctx, md, NULL))
        {
            if (1 == EVP_DigestUpdate(&mdctx, msg, len))
            {
                if (1 == EVP_DigestFinal_ex(&mdctx, md_value, md_len))
                {
                    EVP_MD_CTX_cleanup(&mdctx);
                    rstat = 1;
                }
                else
                {
                    printf("EVP_DigestFinal_ex failed\n");
                }
            }
            else
            {
                printf("EVP_DigestUpdate failed \n");
            }
        }
        else
        {
            printf("EVP_DigestInit_ex failed \n");
        }
    }
    else
    {
        printf("EVP_get_digestbyname failed \n");
    }

#endif

    return (rstat);
}

/* [Load_Exchange_Enc_Key]
 *
 * Summary:
 *
 * Load_Exchange_Enc_Key (void)
 * 
 * Input Arguments: None
 *
 * Description:
 *
 * This function retrieves the keystore passphrase from GTMS and then
 * opens the keystore.  The RSA public key read and stored in memory.
 * The RSA public key is read in binary and also stored in memory.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Load_Exchange_Enc_Key()
{
    byte_4              rstat = P_FAILURE;
    byte_4              public_key_len = 0;

    ubyte_1            *public_key_data = NULL;
    struct mbuf        *public_key_mbuf = NULL;

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

    byte_1              password[256];
    byte_1              username[256];
    byte_4              public_key_file_size = 0;
    FILE               *public_key_file_p;
    BIO                *bioIn;
    G_ERROR             error; 

    err_string = null_err_string;
    memset(password,0x00,sizeof(password));
    memset(username,0x00,sizeof(username));

    if (tnicon_p->exchange_enc_key_state == ENC_CONFIGURED)
    {
        OpenSSL_add_all_algorithms();
        tnicon_p->x_decrypt_key = (void *) RSA_new();

        if (tnicon_p->x_decrypt_key != NULL)
        {
            rstat = g_pswd_retrieve("MX_KEYSTORE", username, password, &error);

            if (rstat == P_SUCCESS)
            {
                bioIn = BIO_new_file(tnicon_p->keystore_file, "r");
                if (bioIn == NULL)
                {
                    rstat = P_FAILURE;
                    sprintf(err_string.par1,"unable to open keystore");
                    sprintf(err_string.par2,"(%s)", tnicon_p->keystore_file);
                }
                else
                {
                    tnicon_p->x_decrypt_key =
                        (void *) PEM_read_bio_RSAPrivateKey(bioIn,
                                                            (RSA **)&tnicon_p->x_decrypt_key,
                                                            0,
                                                            password);
                    if (tnicon_p->x_decrypt_key != NULL)
                    {
                       rstat = P_SUCCESS;
                    }
                    else
                    {
                        SSL_ERROR;
                        rstat = P_FAILURE;
                        sprintf(err_string.par1,"key retrieval failure");
                        sprintf(err_string.par2,"(%s)", ssl_error);
                    }
                }
                BIO_free(bioIn);
            }
            else
            {
                rstat = P_FAILURE;
                sprintf(err_string.par1,"unable to retrieve keystore password");
            }

            /* Read RSA public key as a binary file.  It is assumed the key format is DER */

            if (rstat == P_SUCCESS)
            {
                public_key_file_p = fopen(tnicon_p->publickey_file, "rb");
                fseek(public_key_file_p, 0, SEEK_END);
                public_key_file_size = ftell(public_key_file_p);
                rewind (public_key_file_p);

                if (public_key_file_size <= max_pdu_size_val)
                {
                    if ((public_key_mbuf = mb_alloc (public_key_file_size)) == NULL)
                    {
                        rstat = P_FAILURE;
                        sprintf(err_string.par1,"public key buffer allocation failure");
                        sprintf(err_string.par2,"size = %d", public_key_file_size);
                    }
                    else
                    {
                        public_key_data = public_key_mbuf->m_data;
                        public_key_len = fread (public_key_data, 1,
                                                public_key_file_size,
                                                public_key_file_p);

                        if (public_key_len == public_key_file_size)
                        {
                            public_key_mbuf->m_len = public_key_len;
                            tnicon_p->x_encrypt_key = (void *) public_key_mbuf;
                            tnicon_p->exchange_enc_key_state = ENC_AVAILABLE; 
                        }
                        else
                        {
                            rstat = P_FAILURE;
                            sprintf(err_string.par1,"failure reading public key file");
                        }
                    }
                }
                else
                {
                    rstat = P_FAILURE;
                    sprintf(err_string.par1,"public key too large for mbuf allocation");
                    sprintf(err_string.par2,"size = %d", public_key_file_size);
                }
                fclose(public_key_file_p);
            } 
        }
        else
        {
            SSL_ERROR;
            rstat = P_FAILURE;
            sprintf(err_string.par1,"key allocation failure");
            sprintf(err_string.par2,"(%s)", ssl_error);
        }

        if (rstat == P_FAILURE)
        {
            RSA_free((RSA *)tnicon_p->x_decrypt_key);
            tnicon_p->exchange_enc_key_state = ENC_NOT_AVAILABLE;
            output_err("Load_Exchange_Enc_Key",
                       MI_TNI_KEY_X_DISABLE,
                       MX_ERR_LVL_WARNING,
                       err_string);
        }
    }

#endif

    return (rstat);
}

/* [Decrypt_Client_Key]
 *
 * Summary:
 *
 * Decrypt_Client_Key (ubyte_2 conn,
 *                     byte_4 enc_key_length,
 *                     ubyte_1 *enc_key_ptr)
 *
 * Input Arguments:
 *
 * conn - Connection number
 * enc_key_length  - Length in bytes of the encrypted symmetric key
 * enc_key_ptr - Pointer to the encrypted symmetric key
 *
 * Description:
 * This function allocates an mbuf to decrypted symmetric key.  The 
 * encrypted symmetric key is decrypted using the RSA private key.
 * The symmetric key is then placed in another mbuf and assigned to
 * the connection. 
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Decrypt_Client_Key (ubyte_2 conn, byte_4 enc_key_length, ubyte_1 *enc_key_ptr)
{
    ubyte_1            *decrypted_data = NULL;

    byte_4              bytes_decrypted = 0;
    byte_4              encrypted_bytes_read = 0;
    byte_4              key_modulus = 0;
    byte_4              loop_count = 0;
    byte_4              num_loops = 0;
    byte_4              rstat = P_SUCCESS;

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

    static byte_4       seed = 0;
    struct mbuf        *decrypted_key_mbuf = NULL;
    struct mbuf        *key_mbuf = NULL;

    CRYPT_AES_EXPKEY   *aes_exp_key;

    int              idx1;
    int              idx2;

    if (seed == 0)
    {
        RAND_seed(&enc_key_ptr[0], enc_key_length);
        seed = 1;
    }

    if( (decrypted_key_mbuf = mb_alloc (max_pdu_size_val)) == NULL )
    {
        sprintf(err_string.par1,"decrypt key");
        sprintf(err_string.par2,"%d",max_pdu_size_val);
        sprintf(err_string.par3,"%s","Decrypt_Client_Key");

        output_err("Decrypt_Client_Key",
                   MI_TNI_MB_ALLOC,
                   MX_ERR_LVL_ERROR,
                   err_string);

        rstat = P_FAILURE;
    }
    else
    {
        decrypted_data = (ubyte_1 *) decrypted_key_mbuf->m_data;

        /* Start of DEBUG code */
        /*
          printf("bytes_encrypted = %d\n", enc_key_length);
          idx2 = 0;
          for (idx1=0; idx1 < enc_key_length; idx1++)
          {
              if (idx2 < 9) {
                  printf("(byte)0x%2.2x, ",enc_key_ptr[idx1]);
              }
              else
              {
                  printf("(byte)0x%2.2x, \n",enc_key_ptr[idx1]);
                  idx2 = -1;
              }
              idx2++;
          }
          printf("\n");
        */
        /* End of DEBUG code */

    }

    if ((rstat == P_SUCCESS) &&
        (tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_30) &&
        (tnicon_p->exchange_enc_key_state == ENC_AVAILABLE) &&
        (enc_key_length > 0) && (enc_key_ptr != NULL))
    {
        switch (tnicon_p->connection[conn].enc_key_x_method)
        {
        case METH_ENC_KEY_X_RSA:
            key_modulus = RSA_size((RSA *)tnicon_p->x_decrypt_key);

/* Encrypted data size must be evenly divisible by the RSA key modulus size */

            if (key_modulus != 0)
            {
                if ((enc_key_length%key_modulus) == 0)
                {
                    num_loops = enc_key_length/key_modulus;
                }
                else
                {
                    rstat = P_FAILURE;
                }
            }
            else
            {
                rstat = P_FAILURE;
            }

/* Decrypt the encryption key */

            if (rstat == P_SUCCESS)
            {
                for (loop_count = 0;
                     (loop_count < num_loops) && (rstat == P_SUCCESS); loop_count++)
                {
                    /* Start of DEBUG code */
                    /*
                      printf("key_modulus = %d\n",key_modulus);
                      printf("encrypted_bytes_read = %d\n", encrypted_bytes_read);
                      printf("bytes_decrypted = %d\n", bytes_decrypted);
                      printf("Byte %2.2x\n", enc_key_ptr[encrypted_bytes_read]);
                    */
                    /* End of DEBUG code */

                    bytes_decrypted = RSA_private_decrypt(key_modulus, 
	                                                  &enc_key_ptr[encrypted_bytes_read], 
                                                          &decrypted_data[bytes_decrypted], 
                                                          (RSA *)tnicon_p->x_decrypt_key, 
                                                          RSA_PKCS1_PADDING);

                    if (bytes_decrypted < 0)
                    {
                        SSL_ERROR;
                        rstat = P_FAILURE;
                    }
                    else
                    {
                        encrypted_bytes_read += key_modulus;
                        decrypted_key_mbuf->m_len += bytes_decrypted;
                    }
                }
            }

            if (rstat == P_SUCCESS)
            {
                switch (tnicon_p->connection[conn].enc_key_type)
                {
                case ENC_KEY_AES_128_CBC:
                case ENC_KEY_AES_256_CBC:
                    /* Allocate buffer for key */
                    if( (key_mbuf = mb_alloc (sizeof (CRYPT_AES_EXPKEY))) == NULL )
                    {
                        sprintf(err_string.par1,"key");
                        sprintf(err_string.par2,"%ld", sizeof (CRYPT_AES_EXPKEY));
                        sprintf(err_string.par3,"%s","Decrypt_Client_Key");

                        output_err("Decrypt_Client_Key",
                                   MI_TNI_MB_ALLOC,
                                   MX_ERR_LVL_ERROR,
                                   err_string);

                        rstat = P_FAILURE;
                    }
                    else
                    {
                        aes_exp_key = (CRYPT_AES_EXPKEY *) key_mbuf->m_data;

                        /* Start of DEBUG code */
                        /*
                          for (idx1=0; idx1 < bytes_decrypted; idx1++)
                          {
                              printf("%2.2x",decrypted_data[idx1]);
                          }
                          printf("\n");
                        */
                        /* End of DEBUG code */

                        rstat = crypt_aes_expand_key(&decrypted_data[0],
                                                     bytes_decrypted*8,
                                                     aes_exp_key);

                        /* Start of DEBUG code */
                        /*
                          printf("decrypt key = <%s>\n", strhex(&aes_exp_key->key[0],
                                  sizeof(aes_exp_key->key[0]), buffer));
                          printf("encrypt key = <%s>\n", strhex(&aes_exp_key->key[1],
                                 sizeof(aes_exp_key->key[1]), buffer));
                          printf("rstat = %d\n",rstat);
                        */
                        /* End of DEBUG code */
                    }

                    if (rstat == P_SUCCESS)
                    {
                        /* if key already present deallocate it */
                        if (tnicon_p->connection[conn].enc_state == ENC_AVAILABLE)
                        {
                            mb_free_p((struct mbuf *) tnicon_p->connection[conn].enc_key);
                        }
                        tnicon_p->connection[conn].enc_state = ENC_AVAILABLE;
                        tnicon_p->connection[conn].enc_key = (void *) key_mbuf;
                    }
                    break;

                default:
                    rstat = P_FAILURE;
                    break;
                }
            }
            break;

        default:
            rstat = P_FAILURE;
            break;
        }
    }

    if (rstat == P_FAILURE)
    {
        mb_free_p(key_mbuf);
    }
    mb_free_p(decrypted_key_mbuf);

#endif

    return (rstat);
}

/* [Decrypt_Data]
 *
 * Summary:
 *
 * Decrypt_Data(ubyte_2 conn,
 *              byte_4 del_mode,
 *              byte_4 enc_mode,
 *              byte_4 data_len,
 *              ubyte_1 *data,
 *              byte_4 *rstat)
 *
 * Input Arguments:
 *
 * conn - Connection number
 * del_mode - Delivery mode
 * enc_mode - Encryption mode
 * data_len - Length in bytes of data to be decrypted
 * data - Pointer to encrypted data
 *
 * Output Arguments:
 *
 * rstat - Result status code
 *
 * Description:
 *
 * This function determines if decryption is required based on the
 * delivery mode and the encryption mode of the message. If decryption
 * is required, then the encrypted data is decrypted using the 
 * connection's symmetric key.
 *
 * Returns Value:
 *
 * Upon success an mbuf containing the decrypted data.  Upon failure
 * null.
 *
 */

struct mbuf *Decrypt_Data(ubyte_2 conn, byte_4 del_mode, byte_4 enc_mode,
                          byte_4 data_len, ubyte_1 *data, byte_4 *rstat)
{
    byte_4              decrypt = P_FALSE;
    byte_4              rc = P_SUCCESS;

    struct mbuf        *decrypt_data_mbuf = NULL;
    struct mbuf        *key_mbuf;

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

    byte_1              aes_iv[AES_BLOCK_SIZE] = { 0x00 };

    CRYPT_AES_EXPKEY   *aes_exp_key;

#endif

    if (enc_mode == -1)
    {
        enc_mode = ENC_NONE;
    }

    /* Determine if decryption is requested */
    switch (del_mode)
    {
    case REQUEST:
        switch (enc_mode)
        {
        case ENC_RQST:
        case ENC_RQST_RESP:
            decrypt = P_TRUE;
            break;
        }
        break;

    case RESPONSE:
        switch (enc_mode)
        {
        case ENC_RESP:
        case ENC_RQST_RESP:
            decrypt = P_TRUE;
            break;
        }
        break;
    }

    /* Decrypt data */
    if (decrypt == P_TRUE)
    {
        if (tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_30 &&
            tnicon_p->connection[conn].enc_state == ENC_AVAILABLE)
        {
            switch (tnicon_p->connection[conn].enc_key_type)
            {
            case ENC_KEY_AES_128_CBC:
            case ENC_KEY_AES_256_CBC:

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

                memset(aes_iv, 0, sizeof(aes_iv));

                /* Allocate buffer for decrypted data */
                if( (decrypt_data_mbuf = mb_alloc (data_len + CRYPT_AES_BLOCK_SIZE)) == NULL )
                {
                    sprintf(err_string.par1,"key");
                    sprintf(err_string.par2,"%d",data_len + CRYPT_AES_BLOCK_SIZE);
                    sprintf(err_string.par3,"%s","Decrypt_Data");

                    output_err("Decrypt_Data",
                               MI_TNI_MB_ALLOC,
                               MX_ERR_LVL_ERROR,
                               err_string);

                    rc = MXSRV_RPC_ERR_NOBUILD;
                }
                else
                {
                    key_mbuf = (struct mbuf *) tnicon_p->connection[conn].enc_key;
                    aes_exp_key = (CRYPT_AES_EXPKEY *) key_mbuf->m_data;
                    rc = crypt_aes_cbc_decrypt(data,
                                               data_len,
                                               aes_exp_key,
                                               (CRYPT_AES_IV *) aes_iv,
                                               decrypt_data_mbuf->m_data,
                                               data_len + CRYPT_AES_BLOCK_SIZE);
                    if (rc <= 0)
                    {
                        mb_free_p(decrypt_data_mbuf);
                        decrypt_data_mbuf = NULL;
                        rc = MXSRV_RPC_ERR_DECRYPT;
                    }
                    else
                    {
                        decrypt_data_mbuf->m_len = rc;
                        rc = P_SUCCESS;
                    }
                }

#endif

                break;
            }
        }
        else
        {
            rc = MXSRV_RPC_ERR_NOENC;
        }
    }
    else /* No decryption required, copy unencrypted data into mbuf and return it */
    {
        if( (decrypt_data_mbuf = mb_alloc (data_len)) == NULL )
        {
            sprintf(err_string.par1,"unencrypted data");
            sprintf(err_string.par2,"%d",data_len);
            sprintf(err_string.par3,"%s","Decrypt_Data");

            output_err("Decrypt_Data",
                       MI_TNI_MB_ALLOC,
                       MX_ERR_LVL_ERROR,
                       err_string);

            rc = MXSRV_RPC_ERR_NOBUILD;
        }
        else
        {
            memcpy (decrypt_data_mbuf->m_data, data, data_len);
            decrypt_data_mbuf->m_len = data_len;
        }
    }

    *rstat = rc;
    return(decrypt_data_mbuf);
}

/* [Encrypt_Data]
 *
 * Summary:
 *
 * Encrypt_Data(ubyte_2 conn,
 *              byte_4 del_mode,
 *              byte_4 enc_mode,
 *              byte_4 data_len,
 *              ubyte_1 *data,
 *              byte_4 *rstat)
 *
 * Input Arguments:
 *
 * conn - Connection number
 * del_mode - Delivery mode
 * enc_mode - Encryption mode
 * data_len - Length in bytes of data to be decrypted
 * data - Pointer to encrypted data
 *
 * Output Arguments:
 *
 * rstat - Result status code
 *
 * Description:
 *
 * This function determines if encryption is required based on the
 * delivery mode and the encryption mode of the message. If encryption
 * is required, then the data is encrypted using the connection's
 * symmetric key.
 *
 * Returns Value:
 *
 * Upon success an mbuf containing the decrypted data.  Upon failure
 * null.
 *
 */

struct mbuf *Encrypt_Data(ubyte_2 conn, byte_4 del_mode, byte_4 enc_mode,
                          byte_4 *data_len, ubyte_1 *data, byte_4 *rstat)
{
    byte_4              encrypt = P_FALSE;
    byte_4              rc = P_SUCCESS;

    struct mbuf        *encrypt_data_mbuf = NULL;
    struct mbuf        *key_mbuf;

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

    byte_1              aes_iv[AES_BLOCK_SIZE] = { 0x00 };

    CRYPT_AES_EXPKEY   *aes_exp_key;

#endif

    if (enc_mode == -1)
    {
        enc_mode = ENC_NONE;
    }

    /* Determine if encryption is requested */
    switch (del_mode)
    {
    case REQUEST:
        switch (enc_mode)
        {
        case ENC_RQST:
        case ENC_RQST_RESP:
            encrypt = P_TRUE;
            break;
        }
        break;

    case RESPONSE:
        switch (enc_mode)
        {
        case ENC_RESP:
        case ENC_RQST_RESP:
            encrypt = P_TRUE;
            break;
        }
        break;
    }

    /* Encrypt data */
    if (encrypt == P_TRUE)
    {
        if (tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_30 &&
            tnicon_p->connection[conn].enc_state == ENC_AVAILABLE)
        {
            switch (tnicon_p->connection[conn].enc_key_type)
            {
            case ENC_KEY_AES_128_CBC:
            case ENC_KEY_AES_256_CBC:

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

                memset(aes_iv, 0, sizeof(aes_iv));

                /* Allocate buffer for encrypted data */
                if( (encrypt_data_mbuf = mb_alloc (*data_len + CRYPT_AES_BLOCK_SIZE)) == NULL )
                {
                    sprintf(err_string.par1,"encrypted data");
                    sprintf(err_string.par2,"%d",*data_len + CRYPT_AES_BLOCK_SIZE);
                    sprintf(err_string.par3,"%s","Encrypt_Data");

                    output_err("Encrypt_Data",
                               MI_TNI_MB_ALLOC,
                               MX_ERR_LVL_ERROR,
                               err_string);

                    rc = MXSRV_RPC_ERR_NOBUILD;
                }
                else
                {
                    key_mbuf = (struct mbuf *) tnicon_p->connection[conn].enc_key;
                    aes_exp_key = (CRYPT_AES_EXPKEY *) key_mbuf->m_data;

                    /* Start of DEBUG code */
                    /*
                      printf("decrypt key = <%s>\n", strhex(&aes_exp_key->key[0],
                             sizeof(aes_exp_key->key[0]), buffer));
                      printf("encrypt key = <%s>\n", strhex(&aes_exp_key->key[1],
                             sizeof(aes_exp_key->key[1]), buffer));
                    */
                    /* End of DEBUG code */

                    rc = crypt_aes_cbc_encrypt(data,
                                               *data_len,
                                               aes_exp_key,
                                               (CRYPT_AES_IV *) aes_iv,
                                               encrypt_data_mbuf->m_data,
                                               *data_len + CRYPT_AES_BLOCK_SIZE);

                    if (rc < 0)
                    {
                        mb_free_p(encrypt_data_mbuf);
                        encrypt_data_mbuf = NULL;
                        rc = MXSRV_RPC_ERR_ENCRYPT;
                    }
                    else
                    {
                        encrypt_data_mbuf->m_len = rc;
                        *data_len = rc;
                        rc = P_SUCCESS;
                    }
                }

#endif

                break;
            }
        }
        else
        {
            rc = MXSRV_RPC_ERR_NOENC;
        }
    }
    else /* No encryption required, copy unencrypted data into mbuf and return it */
    {
        if( (encrypt_data_mbuf = mb_alloc (*data_len)) == NULL )
        {
            sprintf(err_string.par1,"unencrypted data");
            sprintf(err_string.par2,"%d",*data_len);
            sprintf(err_string.par3,"%s","Encrypt_Data");

            output_err("Encrypt_Data",
                       MI_TNI_MB_ALLOC,
                       MX_ERR_LVL_ERROR,
                       err_string);

            rc = MXSRV_RPC_ERR_NOBUILD;
        }
        else
        {
            memcpy (encrypt_data_mbuf->m_data, data, *data_len);
            encrypt_data_mbuf->m_len = *data_len; 
        }
    }
    *rstat = rc;
    return(encrypt_data_mbuf);
}
