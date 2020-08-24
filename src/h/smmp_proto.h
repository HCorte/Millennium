/*
 * ===[ smmp_proto.h ]=============================================
 *
 * Description:   SMMP api prototypes
 *
 * -------------------------------------------------------------------
 * This item is the property of GTECH Corporation, West Greenwich,
 * Rhode Island, and contains confidential and trade secret
 * information.  It may not be transferred from the custody or control
 * of GTECH except as authorized in writing by an officer of GTECH.
 * Neither this item nor the information it contains may be used,
 * transferred, reproduced, published, or disclosed, in whole or in
 * part, and directly or indirectly, except as expressly authorized by
 * an officer of GTECH, pursuant to written agreement.
 *
 * Copyright (c) 2007 GTECH Corporation. All rights reserved.
 * -------------------------------------------------------------------
 *
 * ===================================================================
 */


#ifndef SMMP_PROTO_H
#define SMMP_PROTO_H 1

/* smmp_cfg_bufs.c */
ubyte_4             smmp_config_buf_pool(ubyte_4 pool_number, char *pool_name, ubyte_4 n_buffers, ubyte_4 buffer_size,
                                         ubyte_4 buffer_threshold);

ubyte_4             smmp_init_buf_pools(void *unused_param1);

void                smmp_buf_pool_stats(void);

/* smmp_error.c */
ubyte_4             smmp_post_err_message(ubyte_4 post_status);

/* smmp_error_pdu.c */
ubyte_4             smmp_err_start(void *host_strenc_method, void *strenc_method, void **msg_hdl);
ubyte_4             smmp_err_end(void **msg_hdl, void *msg_p, ubyte_4 * msg_length);

/* smmp_map_api.c */
ubyte_4             smmp_map_next(const void *msg_hdl, void **map_hdl);
ubyte_4             smmp_map_prev(const void *msg_hdl, void **map_hdl);
ubyte_4             smmp_map_get(const void *msg_hdl, const void *map_identifier,
                                 const ubyte_4 map_ident_len, const void *map_version,
                                 const ubyte_4 map_ver_len, const ubyte_4 map_format, void **map_hdl);

ubyte_4             smmp_map_get_attr_nbr(const void *msg_hdl, const void *map_hdl, const ubyte_4 attribute, byte_4 * attr_val);
ubyte_4             smmp_map_set_attr_nbr(const void *msg_hdl, const void *map_hdl, const ubyte_4 attribute, byte_4 attr_val);

ubyte_4             smmp_map_get_attr_str(const void *msg_hdl, const void *map_hdl, const ubyte_4 attribute,
                                          void *attr_val, const ubyte_4 value_buffer_max, ubyte_4 * attr_val_length);

ubyte_4             smmp_map_set_attr_str(const void *msg_hdl, const void *map_hdl,
                                          const ubyte_4 attribute, const char *attr_val, const ubyte_4 attr_val_length);

ubyte_4             smmp_map_start(const void *msg_hdl, const char *map_identifier,
                                   const ubyte_4 map_ident_len, const char *map_version,
                                   const ubyte_4 map_ver_len, const ubyte_4 map_format, void **map_hdl);
ubyte_4             smmp_map_end(const void *msg_hdl, void **map_hdl);

ubyte_4             smmp_map_get_next_element(const void *msg_hdl, const void *map_hdl,
                                              void *elem_name, ubyte_4 elem_name_len, ubyte_4 * datatype);

/* smmp_msg_api.c */
ubyte_4             smmp_msg_load(const void *msg_p, const ubyte_4 msg_length, const void *host_strenc_method, void **msg_hdl);
ubyte_4             smmp_msg_unload(void **msg_hdl);

ubyte_4             smmp_msg_start(void *host_strenc_method, void *strenc_method, void **msg_hdl);
ubyte_4             smmp_msg_end(void **msg_hdl, void *msg_p, ubyte_4 * msg_length);

/* smmp_std_create_funcs.c */
ubyte_4             smmp_val_set_boolean(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_BOOLEAN value);
ubyte_4             smmp_val_set_null(const void *msg_hdl, const void *map_hdl,
                                      const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_1 value);
ubyte_4             smmp_val_set_byte_1(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_1 value);
ubyte_4             smmp_val_set_ubyte_1(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_1 value);
ubyte_4             smmp_val_set_byte_2(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_2 value);
ubyte_4             smmp_val_set_ubyte_2(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_2 value);
ubyte_4             smmp_val_set_byte_4(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_4 value);
ubyte_4             smmp_val_set_ubyte_4(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_4 value);
ubyte_4             smmp_val_set_byte_8(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_8 value);
ubyte_4             smmp_val_set_ubyte_8(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_8 value);
ubyte_4             smmp_val_set_asc(const void *msg_hdl, const void *map_hdl,
                                     const void *element_name, const ubyte_4 element_name_len, SMMP_ASC * value);
ubyte_4             smmp_val_set_varasc(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_VARASC * value);
ubyte_4             smmp_val_set_str(const void *msg_hdl, const void *map_hdl, const void *element_name,
                                     const ubyte_4 element_name_len, SMMP_STR * value, ubyte_4 value_len);
ubyte_4             smmp_val_set_varstr(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name,
                                        const ubyte_4 element_name_len, SMMP_VARSTR * value, ubyte_4 value_len);
ubyte_4             smmp_val_set_binary(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name,
                                        const ubyte_4 element_name_len, SMMP_BINARY * value, ubyte_4 value_len);
ubyte_4             smmp_val_set_varbin(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name,
                                        const ubyte_4 element_name_len, SMMP_VARBIN * value, ubyte_4 value_len);
ubyte_4             smmp_val_set_real_4(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_REAL_4 value);
ubyte_4             smmp_val_set_real_8(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_REAL_8 value);
ubyte_4             smmp_val_set_datetime_8(const void *msg_hdl, const void *map_hdl,
                                            const void *element_name, const ubyte_4 element_name_len, SMMP_DATETIME_8 value);

ubyte_4             smmp_val_set_bigdecimal(const void *msg_hdl, const void *map_hdl,
                                            const void *element_name,
                                            const ubyte_4 element_name_len, SMMP_BIGDECIMAL * value, ubyte_4 value_len);

/* smmp_std_parse_funcs.c */
ubyte_4             smmp_val_get_boolean(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_BOOLEAN * value);
ubyte_4             smmp_val_get_attr_nbr(const void *msg_hdl, const void *map_hdl,
                                          const void *element_name,
                                          const ubyte_4 element_name_len, const ubyte_4 attribute, byte_4 * attr_val);
ubyte_4             smmp_val_get_attr_str(const void *msg_hdl, const void *map_hdl,
                                          const void *element_name,
                                          const ubyte_4 element_name_len, const ubyte_4 attribute,
                                          void *attr_val, const ubyte_4 value_buffer_max, ubyte_4 * attr_val_length);
ubyte_4             smmp_val_get_byte_1(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_1 * value);
ubyte_4             smmp_val_get_ubyte_1(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_1 * value);
ubyte_4             smmp_val_get_byte_2(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_2 * value);
ubyte_4             smmp_val_get_ubyte_2(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_2 * value);
ubyte_4             smmp_val_get_byte_4(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_4 * value);
ubyte_4             smmp_val_get_ubyte_4(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_4 * value);
ubyte_4             smmp_val_get_byte_8(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_BYTE_8 * value);
ubyte_4             smmp_val_get_ubyte_8(const void *msg_hdl, const void *map_hdl,
                                         const void *element_name, const ubyte_4 element_name_len, SMMP_UBYTE_8 * value);
ubyte_4             smmp_val_get_asc(const void *msg_hdl, const void *map_hdl, const void *element_name,
                                     const ubyte_4 element_name_len,
                                     SMMP_ASC * value, const ubyte_4 value_buffer_max);
ubyte_4             smmp_val_get_varasc(const void *msg_hdl, const void *map_hdl, const void *element_name,
                                        const ubyte_4 element_name_len,
                                        SMMP_VARASC * value, const ubyte_4 value_buffer_max);
ubyte_4             smmp_val_get_binary(const void *msg_hdl, const void *map_hdl, const void *element_name,
                                        const ubyte_4 element_name_len,
                                        SMMP_BINARY * value, const ubyte_4 value_buffer_max, ubyte_4 * value_len);
ubyte_4             smmp_val_get_varbin(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name,
                                        const ubyte_4 element_name_len,
                                        SMMP_VARBIN * value, const ubyte_4 value_buffer_max, ubyte_4 * value_len);
ubyte_4             smmp_val_get_real_4(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_REAL_4 * value);
ubyte_4             smmp_val_get_real_8(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name, const ubyte_4 element_name_len, SMMP_REAL_8 * value);
ubyte_4             smmp_val_get_datetime_8(const void *msg_hdl, const void *map_hdl,
                                            const void *element_name, const ubyte_4 element_name_len, SMMP_DATETIME_8 * value);
ubyte_4             smmp_val_get_str(const void *msg_hdl, const void *map_hdl,
                                     const void *element_name,
                                     const ubyte_4 element_name_len,
                                     SMMP_STR * value, const ubyte_4 value_buffer_max, ubyte_4 * value_len);
ubyte_4             smmp_val_get_varstr(const void *msg_hdl, const void *map_hdl,
                                        const void *element_name,
                                        const ubyte_4 element_name_len,
                                        SMMP_VARSTR * value, const ubyte_4 value_buffer_max, ubyte_4 * value_len);
ubyte_4             smmp_val_get_bigdecimal(const void *msg_hdl, const void *map_hdl,
                                            const void *element_name,
                                            const ubyte_4 element_name_len,
                                            SMMP_BIGDECIMAL * value, const ubyte_4 value_buffer_max, ubyte_4 * value_len);

/* smmp_str_util_funcs.c */
ubyte_4 smmp_validate_str_conv(const byte_1 *from_strenc_method, const byte_1 *to_strenc_method);
ubyte_4 smmp_str_iconv(byte_1 *src, ubyte_4 src_len, const byte_1 *from_codeset, const byte_1 *to_codeset,
                       byte_1 *des, byte_4 *des_len);

/* smmp_util_buf.c */
ubyte_4             smmp_free_all_loaded_msgs(void);
ubyte_4             smmp_free_loaded_msg(void **msg_hdl);

/* smmp_util_pdu.c */
ubyte_4             smmp_print_loaded_msgs(void *msg_hdl);
ubyte_4             smmp_print_loaded_msgs_new(void *msg_hdl);

#endif
