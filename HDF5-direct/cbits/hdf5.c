// Wrappers for calling HDF5 functions in thread-safe manner. HDF5
// could be build with and without threading support.
//
// If it's built in thread-unsafe manner we must protect all calls by
// mutex. In partivular recursive mutex since HDF API makes use of
// callbacks. It's possible to do locking on haskell side but it
// turned out rather unpleasant and error-prone.
//
// If it's build in thread-safe manner current error becomes
// thread-local instead of global. Cheapest way of obtaining error
// stack is to do error checking in the same call
//
// In both cases we need wrapper function. Thus we can abuse some CPP
// in order to get function which works for both variants.

#include "hdf5-hs.h"
#include <hdf5_hl.h>

#ifdef H5_HAVE_THREADSAFE
// ----------------------------------------------------------------
// Thread safe build
static __thread printing_disabled = 0;

#define INI                                       \
    do {                                          \
        printf("calling %s\n", __FUNCTION__);     \
        if( 0 == printing_disabled ) {            \
            H5Eset_auto(H5E_DEFAULT, NULL, NULL); \
            printing_disabled = 1;                \
        }                                         \
    } while(0)
#else
// ----------------------------------------------------------------
// Thread unsafe build

// Global flag which is used to ensure that we turned off printing
// (and do it only once.
static int printing_disabled = 0;

#define INI                                       \
    do {                                          \
        printf("calling %s\n", __FUNCTION__);     \
        if( 0 == printing_disabled ) {            \
            H5Eset_auto(H5E_DEFAULT, NULL, NULL); \
            printing_disabled = 1;                \
        }                                         \
    } while(0)
#endif
// ----------------------------------------------------------------

#define CHECK_CSTR(expr)                                \
    do {                                                \
        INI;                                            \
        char* res = expr;                               \
        if( !res ) { *error = H5Eget_current_stack(); printf("Call errored\n"); } \
        return res;                                     \
    } while(0)
#define CHECK(ty, expr)                                    \
    do {                                                   \
        INI;                                               \
        ty res = expr;                                     \
        if( res < 0 ) { *error = H5Eget_current_stack(); printf("Call errored\n"); } \
        return res;                                        \
    } while(0)

#define CHECK_ERR(expr)   CHECK(herr_t,  expr)
#define CHECK_TRI(expr)   CHECK(htri_t,  expr)
#define CHECK_HID(expr)   CHECK(hid_t,   expr)
#define CHECK_SSIZE(expr) CHECK(ssize_t, expr)


// ----------------------------------------------------------------
// H5A
// ----------------------------------------------------------------

hid_t hs_H5Aopen(hid_t obj_id, const char *attr_name, hid_t aapl_id, hid_t *error) {
    CHECK_HID(H5Aopen(obj_id, attr_name, aapl_id));
}

herr_t  hs_H5Aclose(hid_t attr_id, hid_t *error) {
    CHECK_ERR(H5Aclose(attr_id));
}

hid_t hs_H5Acreate2(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t *error) {
    CHECK_HID(H5Acreate2(loc_id, attr_name, type_id, space_id, acpl_id, aapl_id));
}

herr_t hs_H5Aread(hid_t attr_id, hid_t type_id, void *buf, hid_t *error) {
    CHECK_ERR(H5Aread (attr_id, type_id, buf));
}

herr_t hs_H5Awrite(hid_t attr_id, hid_t type_id, const void *buf, hid_t *error) {
    CHECK_ERR(H5Awrite(attr_id, type_id, buf));
}

htri_t hs_H5Aexists(hid_t obj_id, const char *attr_name, hid_t *error) {
    CHECK_TRI(H5Aexists(obj_id, attr_name));
}

hid_t hs_H5Aget_type(hid_t attr_id, hid_t *error) {
    CHECK_HID(H5Aget_type(attr_id));
}

hid_t hs_H5Aget_space (hid_t attr_id, hid_t *error) {
    CHECK_HID(H5Aget_space(attr_id));
}

/*
hid_t   H5Acreate_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t lapl_id)
herr_t  H5Adelete (hid_t loc_id, const char *attr_name)
herr_t  H5Adelete_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id)
herr_t  H5Adelete_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id)
htri_t  H5Aexists_by_name (hid_t obj_id, const char *obj_name, const char *attr_name, hid_t lapl_id)
hid_t   H5Aget_create_plist (hid_t attr_id)
herr_t  H5Aget_info (hid_t attr_id, H5A_info_t *ainfo)
herr_t  H5Aget_info_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo, hid_t lapl_id)
herr_t  H5Aget_info_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, H5A_info_t *ainfo, hid_t lapl_id)
ssize_t H5Aget_name (hid_t attr_id, size_t buf_size, char *buf)
ssize_t H5Aget_name_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, char *name, size_t size, hid_t lapl_id)
hsize_t H5Aget_storage_size (hid_t attr_id)
herr_t  H5Aiterate2 (hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data)
herr_t  H5Aiterate_by_name (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data, hid_t lapl_id)
hid_t   H5Aopen_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t aapl_id, hid_t lapl_id)
hid_t   H5Aopen_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id, hid_t lapl_id)
herr_t  H5Arename (hid_t loc_id, const char *old_name, const char *new_name)
herr_t  H5Arename_by_name (hid_t loc_id, const char *obj_name, const char *old_attr_name, const char *new_attr_name, hid_t lapl_id)
hid_t   H5Acreate1 (hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t acpl_id)
int     H5Aget_num_attrs (hid_t loc_id)
herr_t  H5Aiterate1 (hid_t loc_id, unsigned *idx, H5A_operator1_t op, void *op_data)
hid_t   H5Aopen_idx (hid_t loc_id, unsigned idx)
hid_t   H5Aopen_name (hid_t loc_id, const char *name)
*/

// ----------------------------------------------------------------
// H5D
// ----------------------------------------------------------------

hid_t hs_H5Dopen2(hid_t loc_id, const char *name, hid_t dapl_id, hid_t *error) {
    CHECK_HID(H5Dopen2(loc_id, name, dapl_id));
}

hid_t hs_H5Dcreate2(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id, hid_t *error) {
    CHECK_HID(H5Dcreate2(loc_id, name, type_id, space_id, lcpl_id, dcpl_id, dapl_id));
}

herr_t hs_H5Dclose(hid_t dset_id, hid_t *error) {
    CHECK_ERR(H5Dclose(dset_id));
}

hid_t hs_H5Dget_type(hid_t dset_id, hid_t *error) {
    CHECK_HID(H5Dget_type(dset_id));
}

hid_t hs_H5Dget_space(hid_t dset_id, hid_t *error) {
    CHECK_HID(H5Dget_space(dset_id));
}

herr_t hs_H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, void *buf, hid_t *error) {
    CHECK_ERR(H5Dread(dset_id, mem_type_id, mem_space_id, file_space_id, dxpl_id, buf));
}

herr_t hs_H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, const void *buf, hid_t *error) {
    CHECK_ERR(H5Dwrite(dset_id, mem_type_id, mem_space_id, file_space_id, dxpl_id, buf));
}

herr_t hs_H5Dset_extent(hid_t dset_id, const hsize_t size[], hid_t *error) {
    CHECK_ERR(H5Dset_extent(dset_id, size));
}

/*
hid_t H5Dcreate_anon(hid_t loc_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id)
herr_t H5Dget_space_status(hid_t dset_id, H5D_space_status_t *allocation)
hid_t H5Dget_create_plist(hid_t dset_id)
hid_t H5Dget_access_plist(hid_t dset_id)
hsize_t H5Dget_storage_size(hid_t dset_id)
herr_t H5Dget_chunk_storage_size(hid_t dset_id, const hsize_t *offset, hsize_t *chunk_bytes)
herr_t H5Dget_num_chunks(hid_t dset_id, hid_t fspace_id, hsize_t *nchunks)
herr_t H5Dget_chunk_info_by_coord(hid_t dset_id, const hsize_t *offset, unsigned *filter_mask, haddr_t *addr, hsize_t *size)
herr_t H5Dchunk_iter(hid_t dset_id, hid_t dxpl_id, H5D_chunk_iter_op_t cb, void *op_data)
herr_t H5Dget_chunk_info(hid_t dset_id, hid_t fspace_id, hsize_t chk_idx, hsize_t *offset, unsigned *filter_mask, haddr_t *addr, hsize_t *size)
haddr_t H5Dget_offset(hid_t dset_id)
herr_t H5Dread_multi(size_t count, hid_t dset_id[], hid_t mem_type_id[], hid_t mem_space_id[], hid_t file_space_id[], hid_t dxpl_id, void *buf[])
herr_t H5Dwrite_multi(size_t count, hid_t dset_id[], hid_t mem_type_id[], hid_t mem_space_id[], hid_t file_space_id[], hid_t dxpl_id, const void *buf[])
herr_t H5Dwrite_chunk(hid_t dset_id, hid_t dxpl_id, uint32_t filters, const hsize_t *offset, size_t data_size, const void *buf)
herr_t H5Dread_chunk(hid_t dset_id, hid_t dxpl_id, const hsize_t *offset, uint32_t *filters, void *buf)
herr_t H5Diterate(void *buf, hid_t type_id, hid_t space_id, H5D_operator_t op, void *operator_data)
herr_t H5Dvlen_get_buf_size(hid_t dset_id, hid_t type_id, hid_t space_id, hsize_t *size)
herr_t H5Dfill(const void *fill, hid_t fill_type_id, void *buf, hid_t buf_type_id, hid_t space_id)
herr_t H5Dflush(hid_t dset_id)
herr_t H5Drefresh(hid_t dset_id)
herr_t H5Dscatter(H5D_scatter_func_t op, void *op_data, hid_t type_id, hid_t dst_space_id, void *dst_buf)
herr_t H5Dgather(hid_t src_space_id, const void *src_buf, hid_t type_id, size_t dst_buf_size, void *dst_buf, H5D_gather_func_t op, void *op_data)
hid_t H5Dcreate1(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t dcpl_id)
hid_t H5Dopen1(hid_t loc_id, const char *name)
herr_t H5Dextend(hid_t dset_id, const hsize_t size[])
herr_t H5Dvlen_reclaim(hid_t type_id, hid_t space_id, hid_t dxpl_id, void *buf)
*/

// ----------------------------------------------------------------
// H5E
// ----------------------------------------------------------------

herr_t hs_H5Eclose_msg(hid_t err_id, hid_t *error) {
    CHECK_ERR(H5Eclose_msg(err_id));
}

herr_t hs_H5Eclose_stack(hid_t stack_id, hid_t *error) {
    CHECK_ERR(H5Eclose_stack(stack_id));
}

hid_t hs_H5Ecreate_stack(hid_t *error) {
    CHECK_HID(H5Ecreate_stack());
}

hid_t hs_H5Eget_current_stack(hid_t *error) {
    CHECK_HID(H5Eget_current_stack());
}

herr_t hs_H5Eget_auto2(hid_t estack_id, H5E_auto2_t *func, void **client_data, hid_t *error) {
    CHECK_ERR(H5Eget_auto2(estack_id, func, client_data));
}

herr_t hs_H5Eset_auto2(hid_t estack_id, H5E_auto2_t func, void *client_data, hid_t *error) {
    CHECK_ERR(H5Eset_auto2(estack_id, func, client_data));
}

herr_t hs_H5Ewalk2(hid_t err_stack, H5E_direction_t direction, H5E_walk2_t func, void *client_data, hid_t *error) {
    CHECK_ERR(H5Ewalk2(err_stack, direction, func, client_data));
}

ssize_t hs_H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg, size_t size, hid_t *error) {
    CHECK_SSIZE(H5Eget_msg(msg_id, type, msg, size));
}

/*
hid_t	H5Eregister_class(const char *cls_name, const char *lib_name, const char *version)
herr_t	H5Eunregister_class(hid_t class_id)
hid_t	H5Ecreate_msg(hid_t cls, H5E_type_t msg_type, const char *msg)
herr_t	H5Eappend_stack(hid_t dst_stack_id, hid_t src_stack_id, hbool_t close_source_stack)
ssize_t	H5Eget_class_name(hid_t class_id, char *name, size_t size)
herr_t	H5Eset_current_stack(hid_t err_stack_id)
herr_t	H5Epush2(hid_t err_stack, const char *file, const char *func, unsigned line, hid_t cls_id, hid_t maj_id, hid_t min_id, const char *msg,...)
herr_t	H5Epop(hid_t err_stack, size_t count)
herr_t	H5Eprint2(hid_t err_stack, FILE *stream)
herr_t	H5Eclear2(hid_t err_stack)
herr_t	H5Eauto_is_v2(hid_t err_stack, unsigned *is_stack)
ssize_t	H5Eget_num(hid_t error_stack_id)
herr_t	H5Eclear1(void)
herr_t	H5Eget_auto1(H5E_auto1_t *func, void **client_data)
herr_t	H5Epush1(const char *file, const char *func, unsigned line, H5E_major_t maj, H5E_minor_t min, const char *str)
herr_t	H5Eprint1(FILE *stream)
herr_t	H5Eset_auto1(H5E_auto1_t func, void *client_data)
*/


// ----------------------------------------------------------------
// H5F
// ----------------------------------------------------------------

// htri_t hs_H5Fis_accessible(const char *container_name, hid_t fapl_id, hid_t *error) {
//     CHECK_TRI(H5Fis_accessible(container_name, fapl_id));
// }

hid_t hs_H5Fcreate(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t *error) {
    CHECK_HID(H5Fcreate(filename, flags, fcpl_id, fapl_id));
}

hid_t hs_H5Fopen(const char *filename, unsigned flags, hid_t fapl_id, hid_t *error) {
    CHECK_HID(H5Fopen(filename, flags, fapl_id));
}

hid_t hs_H5Freopen(hid_t file_id, hid_t *error) {
    CHECK_HID(H5Freopen(file_id));
}

herr_t hs_H5Fclose(hid_t file_id, hid_t *error) {
    CHECK_ERR(H5Fclose(file_id));
}

// herr_t hs_H5Fdelete(const char *filename, hid_t fapl_id, hid_t *error) {
//     CHECK_ERR(H5Fdelete(filename, fapl_id));
// }

/*
herr_t	H5Fflush(hid_t object_id, H5F_scope_t scope)
hid_t	H5Fget_create_plist(hid_t file_id)
hid_t	H5Fget_access_plist(hid_t file_id)
herr_t	H5Fget_intent(hid_t file_id, unsigned *intent)
herr_t	H5Fget_fileno(hid_t file_id, unsigned long *fileno)
ssize_t	H5Fget_obj_count(hid_t file_id, unsigned types)
ssize_t	H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *obj_id_list)
herr_t	H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle)
herr_t	H5Fmount(hid_t loc, const char *name, hid_t child, hid_t plist)
herr_t	H5Funmount(hid_t loc, const char *name)
hssize_t	H5Fget_freespace(hid_t file_id)
herr_t	H5Fget_filesize(hid_t file_id, hsize_t *size)
herr_t	H5Fget_eoa(hid_t file_id, haddr_t *eoa)
herr_t	H5Fincrement_filesize(hid_t file_id, hsize_t increment)
ssize_t	H5Fget_file_image(hid_t file_id, void *buf_ptr, size_t buf_len)
ssize_t	H5Fget_name(hid_t obj_id, char *name, size_t size)
herr_t	H5Fget_info2(hid_t obj_id, H5F_info2_t *file_info)
ssize_t	H5Fget_free_sections(hid_t file_id, H5F_mem_t type, size_t nsects, H5F_sect_info_t *sect_info)
herr_t	H5Fclear_elink_file_cache(hid_t file_id)
herr_t	H5Fset_libver_bounds(hid_t file_id, H5F_libver_t low, H5F_libver_t high)
herr_t	H5Freset_page_buffering_stats(hid_t file_id)
herr_t	H5Fget_page_buffering_stats(hid_t file_id, unsigned accesses[2], unsigned hits[2], unsigned misses[2], unsigned evictions[2], unsigned bypasses[2])
herr_t	H5Fget_dset_no_attrs_hint(hid_t file_id, hbool_t *minimize)
herr_t	H5Fset_dset_no_attrs_hint(hid_t file_id, hbool_t minimize)
herr_t	H5Fget_info1(hid_t obj_id, H5F_info1_t *file_info)
herr_t	H5Fset_latest_format(hid_t file_id, hbool_t latest_format)
htri_t	H5Fis_hdf5(const char *file_name)
*/


// ----------------------------------------------------------------
// H5G
// ----------------------------------------------------------------

hid_t hs_H5Gcreate2(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id, hid_t *error) {
    CHECK_HID(H5Gcreate2(loc_id, name, lcpl_id, gcpl_id, gapl_id));
}

hid_t hs_H5Gopen2(hid_t loc_id, const char *name, hid_t gapl_id, hid_t *error) {
    CHECK_HID(H5Gopen2(loc_id, name, gapl_id));
}

herr_t hs_H5Gclose(hid_t group_id, hid_t *error) {
    CHECK_ERR(H5Gclose(group_id));
}

/*
hid_t	H5Gcreate_anon(hid_t loc_id, hid_t gcpl_id, hid_t gapl_id)
hid_t	H5Gget_create_plist(hid_t group_id)
herr_t	H5Gget_info(hid_t loc_id, H5G_info_t *ginfo)
herr_t	H5Gget_info_by_name(hid_t loc_id, const char *name, H5G_info_t *ginfo, hid_t lapl_id)
herr_t	H5Gget_info_by_idx(hid_t loc_id, const char *group_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5G_info_t *ginfo, hid_t lapl_id)
herr_t	H5Gflush(hid_t group_id)
herr_t	H5Grefresh(hid_t group_id)
hid_t	H5Gcreate1(hid_t loc_id, const char *name, size_t size_hint)
hid_t	H5Gopen1(hid_t loc_id, const char *name)
herr_t	H5Glink(hid_t cur_loc_id, H5L_type_t type, const char *cur_name, const char *new_name)
herr_t	H5Glink2(hid_t cur_loc_id, const char *cur_name, H5L_type_t type, hid_t new_loc_id, const char *new_name)
herr_t	H5Gmove(hid_t src_loc_id, const char *src_name, const char *dst_name)
herr_t	H5Gmove2(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dst_name)
herr_t	H5Gunlink(hid_t loc_id, const char *name)
herr_t	H5Gget_linkval(hid_t loc_id, const char *name, size_t size, char *buf)
herr_t	H5Gset_comment(hid_t loc_id, const char *name, const char *comment)
int	H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize, char *buf)
herr_t	H5Giterate(hid_t loc_id, const char *name, int *idx, H5G_iterate_t op, void *op_data)
herr_t	H5Gget_num_objs(hid_t loc_id, hsize_t *num_objs)
herr_t	H5Gget_objinfo(hid_t loc_id, const char *name, hbool_t follow_link, H5G_stat_t *statbuf)
ssize_t	H5Gget_objname_by_idx(hid_t loc_id, hsize_t idx, char *name, size_t size)
H5G_obj_t	H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx)
*/


// ----------------------------------------------------------------
// H5LT
// ----------------------------------------------------------------

herr_t hs_H5LTdtype_to_text(hid_t dtype, char *str, H5LT_lang_t lang_type, size_t *len, hid_t *error) {
    CHECK_ERR(H5LTdtype_to_text(dtype, str, lang_type, len));
}
htri_t hs_H5LTpath_valid(hid_t loc_id, const char *path, bool check_object_valid, hid_t *error) {
    CHECK_TRI(H5LTpath_valid(loc_id, path, check_object_valid));
}

/*
H5_HLDLL herr_t H5LTmake_dataset (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer)
H5_HLDLL herr_t H5LTmake_dataset_char (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const char *buffer)
H5_HLDLL herr_t H5LTmake_dataset_short (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const short *buffer)
H5_HLDLL herr_t H5LTmake_dataset_int (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const int *buffer)
H5_HLDLL herr_t H5LTmake_dataset_long (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const long *buffer)
H5_HLDLL herr_t H5LTmake_dataset_float (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const float *buffer)
H5_HLDLL herr_t H5LTmake_dataset_double (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const double *buffer)
H5_HLDLL herr_t H5LTmake_dataset_string (hid_t loc_id, const char *dset_name, const char *buf)
H5_HLDLL herr_t H5LTread_dataset (hid_t loc_id, const char *dset_name, hid_t type_id, void *buffer)
H5_HLDLL herr_t H5LTread_dataset_char (hid_t loc_id, const char *dset_name, char *buffer)
H5_HLDLL herr_t H5LTread_dataset_short (hid_t loc_id, const char *dset_name, short *buffer)
H5_HLDLL herr_t H5LTread_dataset_int (hid_t loc_id, const char *dset_name, int *buffer)
H5_HLDLL herr_t H5LTread_dataset_long (hid_t loc_id, const char *dset_name, long *buffer)
H5_HLDLL herr_t H5LTread_dataset_float (hid_t loc_id, const char *dset_name, float *buffer)
H5_HLDLL herr_t H5LTread_dataset_double (hid_t loc_id, const char *dset_name, double *buffer)
H5_HLDLL herr_t H5LTread_dataset_string (hid_t loc_id, const char *dset_name, char *buf)
H5_HLDLL herr_t H5LTget_dataset_ndims (hid_t loc_id, const char *dset_name, int *rank)
H5_HLDLL herr_t H5LTget_dataset_info (hid_t loc_id, const char *dset_name, hsize_t *dims, H5T_class_t *type_class, size_t *type_size)
H5_HLDLL herr_t H5LTfind_dataset (hid_t loc_id, const char *name)
H5_HLDLL herr_t H5LTset_attribute_string (hid_t loc_id, const char *obj_name, const char *attr_name, const char *attr_data)
H5_HLDLL herr_t H5LTset_attribute_char (hid_t loc_id, const char *obj_name, const char *attr_name, const char *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_uchar (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned char *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_short (hid_t loc_id, const char *obj_name, const char *attr_name, const short *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_ushort (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned short *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_int (hid_t loc_id, const char *obj_name, const char *attr_name, const int *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_uint (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned int *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_long (hid_t loc_id, const char *obj_name, const char *attr_name, const long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_long_long (hid_t loc_id, const char *obj_name, const char *attr_name, const long long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_ulong (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_ullong (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned long long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_float (hid_t loc_id, const char *obj_name, const char *attr_name, const float *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_double (hid_t loc_id, const char *obj_name, const char *attr_name, const double *buffer, size_t size)
H5_HLDLL herr_t H5LTget_attribute (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t mem_type_id, void *data)
H5_HLDLL herr_t H5LTget_attribute_string (hid_t loc_id, const char *obj_name, const char *attr_name, char *data)
H5_HLDLL herr_t H5LTget_attribute_char (hid_t loc_id, const char *obj_name, const char *attr_name, char *data)
H5_HLDLL herr_t H5LTget_attribute_uchar (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned char *data)
H5_HLDLL herr_t H5LTget_attribute_short (hid_t loc_id, const char *obj_name, const char *attr_name, short *data)
H5_HLDLL herr_t H5LTget_attribute_ushort (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned short *data)
H5_HLDLL herr_t H5LTget_attribute_int (hid_t loc_id, const char *obj_name, const char *attr_name, int *data)
H5_HLDLL herr_t H5LTget_attribute_uint (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned int *data)
H5_HLDLL herr_t H5LTget_attribute_long (hid_t loc_id, const char *obj_name, const char *attr_name, long *data)
H5_HLDLL herr_t H5LTget_attribute_long_long (hid_t loc_id, const char *obj_name, const char *attr_name, long long *data)
H5_HLDLL herr_t H5LTget_attribute_ulong (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned long *data)
H5_HLDLL herr_t H5LTget_attribute_ullong (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned long long *data)
H5_HLDLL herr_t H5LTget_attribute_float (hid_t loc_id, const char *obj_name, const char *attr_name, float *data)
H5_HLDLL herr_t H5LTget_attribute_double (hid_t loc_id, const char *obj_name, const char *attr_name, double *data)
H5_HLDLL herr_t H5LTget_attribute_ndims (hid_t loc_id, const char *obj_name, const char *attr_name, int *rank)
H5_HLDLL herr_t H5LTget_attribute_info (hid_t loc_id, const char *obj_name, const char *attr_name, hsize_t *dims, H5T_class_t *type_class, size_t *type_size)
H5_HLDLL hid_t 	H5LTtext_to_dtype (const char *text, H5LT_lang_t lang_type)
H5_HLDLL herr_t H5LTfind_attribute (hid_t loc_id, const char *name)
H5_HLDLL htri_t H5LTpath_valid (hid_t loc_id, const char *path, hbool_t check_object_valid)
H5_HLDLL hid_t 	H5LTopen_file_image (void *buf_ptr, size_t buf_size, unsigned flags)
*/


// ----------------------------------------------------------------
// H5S
// ----------------------------------------------------------------

herr_t hs_H5Sclose(hid_t space_id, hid_t *error) {
    CHECK_ERR(H5Sclose(space_id));
}

hid_t hs_H5Screate(H5S_class_t type, hid_t *error) {
    CHECK_HID(H5Screate(type));
}

hid_t hs_H5Screate_simple(int rank, const hsize_t dims[], const hsize_t maxdims[], hid_t *error) {
    CHECK_HID(H5Screate_simple(rank, dims, maxdims));
}

htri_t hs_H5Sis_simple(hid_t space_id, hid_t *error) {
    CHECK_TRI(H5Sis_simple(space_id));
}

int hs_H5Sget_simple_extent_dims(hid_t space_id, hsize_t dims[], hsize_t maxdims[], hid_t *error) {
    CHECK(int,H5Sget_simple_extent_dims(space_id, dims, maxdims));
}

int	hs_H5Sget_simple_extent_ndims(hid_t space_id, hid_t *error) {
    CHECK(int,H5Sget_simple_extent_ndims(space_id));
}

hssize_t hs_H5Sget_simple_extent_npoints(hid_t space_id, hid_t *error) {
    CHECK(hssize_t, H5Sget_simple_extent_npoints(space_id));
}

H5S_class_t	hs_H5Sget_simple_extent_type(hid_t space_id, hid_t *error) {
    CHECK(H5S_class_t, H5Sget_simple_extent_type(space_id));
}

herr_t hs_H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op,
                              const hsize_t start[], const hsize_t stride[],
                              const hsize_t count[], const hsize_t block[],
                              hid_t *error) {
    CHECK_ERR(H5Sselect_hyperslab(space_id, op, start, stride, count, block));
}

/*
hid_t	H5Scombine_hyperslab(hid_t space_id, H5S_seloper_t op, const hsize_t start[], const hsize_t stride[], const hsize_t count[], const hsize_t block[])
hid_t	H5Scombine_select(hid_t space1_id, H5S_seloper_t op, hid_t space2_id)
hid_t	H5Scopy(hid_t space_id)
hid_t	H5Sdecode(const void *buf)
herr_t	H5Sencode2(hid_t obj_id, void *buf, size_t *nalloc, hid_t fapl)
herr_t	H5Sextent_copy(hid_t dst_id, hid_t src_id)
htri_t	H5Sextent_equal(hid_t space1_id, hid_t space2_id)
htri_t	H5Sget_regular_hyperslab(hid_t spaceid, hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[])
herr_t	H5Sget_select_bounds(hid_t spaceid, hsize_t start[], hsize_t end[])
hssize_t	H5Sget_select_elem_npoints(hid_t spaceid)
herr_t	H5Sget_select_elem_pointlist(hid_t spaceid, hsize_t startpoint, hsize_t numpoints, hsize_t buf[])
herr_t	H5Sget_select_hyper_blocklist(hid_t spaceid, hsize_t startblock, hsize_t numblocks, hsize_t buf[])
hssize_t	H5Sget_select_hyper_nblocks(hid_t spaceid)
hssize_t	H5Sget_select_npoints(hid_t spaceid)
H5S_sel_type	H5Sget_select_type(hid_t spaceid)
htri_t	H5Sis_regular_hyperslab(hid_t spaceid)
herr_t	H5Smodify_select(hid_t space1_id, H5S_seloper_t op, hid_t space2_id)
herr_t	H5Soffset_simple(hid_t space_id, const hssize_t *offset)
herr_t	H5Ssel_iter_close(hid_t sel_iter_id)
hid_t	H5Ssel_iter_create(hid_t spaceid, size_t elmt_size, unsigned flags)
herr_t	H5Ssel_iter_get_seq_list(hid_t sel_iter_id, size_t maxseq, size_t maxbytes, size_t *nseq, size_t *nbytes, hsize_t *off, size_t *len)
herr_t	H5Ssel_iter_reset(hid_t sel_iter_id, hid_t space_id)
herr_t	H5Sselect_adjust(hid_t spaceid, const hssize_t *offset)
herr_t	H5Sselect_all(hid_t spaceid)
herr_t	H5Sselect_copy(hid_t dst_id, hid_t src_id)
herr_t	H5Sselect_elements(hid_t space_id, H5S_seloper_t op, size_t num_elem, const hsize_t *coord)
htri_t	H5Sselect_intersect_block(hid_t space_id, const hsize_t *start, const hsize_t *end)
herr_t	H5Sselect_none(hid_t spaceid)
hid_t	H5Sselect_project_intersection(hid_t src_space_id, hid_t dst_space_id, hid_t src_intersect_space_id)
htri_t	H5Sselect_shape_same(hid_t space1_id, hid_t space2_id)
htri_t	H5Sselect_valid(hid_t spaceid)
herr_t	H5Sset_extent_none(hid_t space_id)
herr_t	H5Sset_extent_simple(hid_t space_id, int rank, const hsize_t dims[], const hsize_t max[])
herr_t	H5Sencode1(hid_t obj_id, void *buf, size_t *nalloc)
*/


// ----------------------------------------------------------------
// H5T
// ----------------------------------------------------------------

herr_t hs_H5Tclose(hid_t type_id, hid_t *error) {
    CHECK_ERR(H5Tclose(type_id));
}

hid_t hs_H5Tcreate(H5T_class_t type, size_t size, hid_t *error) {
    CHECK_HID(H5Tcreate(type, size));
}

hid_t hs_H5Tget_super(hid_t type, hid_t *error) {
    CHECK_HID(H5Tget_super(type));
}

size_t hs_H5Tget_size(hid_t type_id, hid_t *error) {
    CHECK(size_t, H5Tget_size(type_id));
}

H5T_class_t	hs_H5Tget_class(hid_t type_id, hid_t *error) {
    CHECK(H5T_class_t, H5Tget_class(type_id));
}

H5T_order_t hs_H5Tget_order(hid_t type_id, hid_t *error) {
    CHECK(H5T_order_t, H5Tget_order(type_id));
}

size_t hs_H5Tget_precision(hid_t type_id, hid_t *error) {
    CHECK(size_t, H5Tget_precision(type_id));
}

H5T_sign_t hs_H5Tget_sign(hid_t type_id, hid_t *error) {
    CHECK(H5T_sign_t, H5Tget_sign(type_id));
}

hid_t hs_H5Tarray_create2(hid_t base_id, unsigned ndims, const hsize_t dim[], hid_t *error) {
    CHECK_HID(H5Tarray_create2(base_id, ndims, dim));
}

int hs_H5Tget_array_ndims(hid_t type_id, hid_t *error) {
    CHECK_HID(H5Tget_array_ndims(type_id));
}

int	hs_H5Tget_array_dims2(hid_t type_id, hsize_t dims[], hid_t *error) {
    CHECK_HID(H5Tget_array_dims2(type_id, dims));
}

int hs_H5Tget_nmembers(hid_t type_id, hid_t *error) {
    CHECK(int, H5Tget_nmembers(type_id));
}

char *hs_H5Tget_member_name(hid_t type_id, unsigned membno, hid_t *error) {
    CHECK_CSTR(H5Tget_member_name(type_id, membno));
}

int hs_H5Tget_member_index(hid_t type_id, const char *name, hid_t *error) {
    CHECK(int, H5Tget_member_index(type_id, name));
}

herr_t hs_H5Tinsert(hid_t parent_id, const char *name, size_t offset, hid_t member_id, hid_t *error) {
    CHECK_ERR(H5Tinsert(parent_id, name, offset, member_id));
}

hid_t hs_H5Tget_member_type(hid_t type_id, unsigned membno, hid_t *error) {
    CHECK_HID(H5Tget_member_type(type_id, membno));
}

hid_t hs_H5Tenum_create(hid_t base_id, hid_t *error) {
    CHECK_HID(H5Tenum_create(base_id));
}

herr_t hs_H5Tenum_insert(hid_t type, const char *name, const void *value, hid_t *error) {
    CHECK_ERR(H5Tenum_insert(type, name, value));
}

herr_t hs_H5Tenum_nameof(hid_t type, const void *value, char *name, size_t size, hid_t *error) {
    CHECK_ERR(H5Tenum_nameof(type, value, name, size));
}

herr_t hs_H5Tenum_valueof(hid_t type, const char *name, void *value, hid_t *error) {
    CHECK_ERR(H5Tenum_valueof(type, name, value));
}

herr_t hs_H5Tget_member_value(hid_t type_id, unsigned membno, void *value, hid_t *error) {
    CHECK_ERR(H5Tget_member_value(type_id, membno, value));
}

/*
hid_t	H5Tcopy(hid_t type_id)
herr_t	H5Tclose_async(hid_t type_id, hid_t es_id)
htri_t	H5Tequal(hid_t type1_id, hid_t type2_id)
herr_t	H5Tlock(hid_t type_id)
herr_t	H5Tcommit2(hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id)
herr_t	H5Tcommit_async(hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t es_id)
hid_t	H5Topen2(hid_t loc_id, const char *name, hid_t tapl_id)
hid_t	H5Topen_async(hid_t loc_id, const char *name, hid_t tapl_id, hid_t es_id)
herr_t	H5Tcommit_anon(hid_t loc_id, hid_t type_id, hid_t tcpl_id, hid_t tapl_id)
hid_t	H5Tget_create_plist(hid_t type_id)
htri_t	H5Tcommitted(hid_t type_id)
herr_t	H5Tencode(hid_t obj_id, void *buf, size_t *nalloc)
hid_t	H5Tdecode(const void *buf)
herr_t	H5Tflush(hid_t type_id)
herr_t	H5Trefresh(hid_t type_id)

htri_t	H5Tdetect_class(hid_t type_id, H5T_class_t cls)
hid_t	H5Tget_native_type(hid_t type_id, H5T_direction_t direction)
herr_t	H5Tset_size(hid_t type_id, size_t size)
herr_t	H5Tcommit1(hid_t loc_id, const char *name, hid_t type_id)
hid_t	H5Topen1(hid_t loc_id, const char *name)

int	H5Tget_offset(hid_t type_id)
herr_t	H5Tget_pad(hid_t type_id, H5T_pad_t *lsb, H5T_pad_t *msb)
herr_t	H5Tget_fields(hid_t type_id, size_t *spos, size_t *epos, size_t *esize, size_t *mpos, size_t *msize)
size_t	H5Tget_ebias(hid_t type_id)
H5T_norm_t	H5Tget_norm(hid_t type_id)
H5T_pad_t	H5Tget_inpad(hid_t type_id)
H5T_str_t	H5Tget_strpad(hid_t type_id)
H5T_cset_t	H5Tget_cset(hid_t type_id)
htri_t	H5Tis_variable_str(hid_t type_id)
herr_t	H5Tset_order(hid_t type_id, H5T_order_t order)
herr_t	H5Tset_precision(hid_t type_id, size_t prec)
herr_t	H5Tset_offset(hid_t type_id, size_t offset)
herr_t	H5Tset_pad(hid_t type_id, H5T_pad_t lsb, H5T_pad_t msb)
herr_t	H5Tset_sign(hid_t type_id, H5T_sign_t sign)
herr_t	H5Tset_fields(hid_t type_id, size_t spos, size_t epos, size_t esize, size_t mpos, size_t msize)
herr_t	H5Tset_ebias(hid_t type_id, size_t ebias)
herr_t	H5Tset_norm(hid_t type_id, H5T_norm_t norm)
herr_t	H5Tset_inpad(hid_t type_id, H5T_pad_t pad)
herr_t	H5Tset_cset(hid_t type_id, H5T_cset_t cset)
herr_t	H5Tset_strpad(hid_t type_id, H5T_str_t strpad)

herr_t  H5Tpack(hid_t type_id)
size_t  H5Tget_member_offset(hid_t type_id, unsigned membno)
H5T_class_t H5Tget_member_class(hid_t type_id, unsigned membno)
*/

// ----------------------------------------------------------------
// H5L
// ----------------------------------------------------------------

// NOTE: compatibility macro for HDF5-1.10
//
// 1.12 introduced H5_iterate2_t
herr_t hs_H5Literate(hid_t grp_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5L_iterate_t op, void *op_data, hid_t* error) {
    CHECK_ERR(H5Literate(grp_id, idx_type, order, idx, op, op_data));
}

herr_t hs_H5Ldelete(hid_t loc_id, const char *name, hid_t lapl_id, hid_t *error) {
    CHECK_ERR(H5Ldelete(loc_id, name, lapl_id));
}

/*
herr_t H5Literate_by_name2(hid_t loc_id, const char *group_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5L_iterate2_t op, void *op_data, hid_t lapl_id)
herr_t H5Lvisit2(hid_t grp_id, H5_index_t idx_type, H5_iter_order_t order, H5L_iterate2_t op, void *op_data)
herr_t H5Lvisit_by_name2(hid_t loc_id, const char *group_name, H5_index_t idx_type, H5_iter_order_t order, H5L_iterate2_t op, void *op_data, hid_t lapl_id)
*/

// ----------------------------------------------------------------
// H5P
// ----------------------------------------------------------------

herr_t hs_H5Pclose (hid_t plist_id, hid_t* error) {
    CHECK_ERR(H5Pclose(plist_id));
}

hid_t hs_H5Pcreate (hid_t cls_id, hid_t* error) {
    CHECK_HID(H5Pcreate(cls_id));
}

herr_t hs_H5Pset_chunk (hid_t plist_id, int ndims, const hsize_t dim[], hid_t* error) {
    CHECK_ERR(H5Pset_chunk(plist_id, ndims, dim));
}

herr_t hs_H5Pset_chunk_opts (hid_t plist_id, unsigned opts, hid_t* error) {
    CHECK_ERR(H5Pset_chunk_opts(plist_id, opts));
}

herr_t hs_H5Pset_deflate (hid_t plist_id, unsigned level, hid_t* error) {
    CHECK_ERR(H5Pset_deflate(plist_id, level));
}

herr_t hs_H5Pset_layout (hid_t plist_id, H5D_layout_t layout, hid_t* error) {
    CHECK_ERR(H5Pset_layout(plist_id, layout));
}


/*
hid_t   H5Pcopy (hid_t plist_id)
hid_t   H5Pdecode (const void *buf)
herr_t  H5Pencode2 (hid_t plist_id, void *buf, size_t *nalloc, hid_t fapl_id)
hid_t   H5Pget_class (hid_t plist_id)
*/

/*
Dataset Creation Properties

htri_t  H5Pall_filters_avail (hid_t plist_id)
herr_t  H5Pfill_value_defined (hid_t plist, H5D_fill_value_t *status)
herr_t  H5Pget_alloc_time (hid_t plist_id, H5D_alloc_time_t *alloc_time)
int H5Pget_chunk (hid_t plist_id, int max_ndims, hsize_t dim[])
herr_t  H5Pget_chunk_opts (hid_t plist_id, unsigned *opts)
herr_t  H5Pget_dset_no_attrs_hint (hid_t dcpl_id, hbool_t *minimize)
herr_t  H5Pget_external (hid_t plist_id, unsigned idx, size_t name_size, char *name, off_t *offset, hsize_t *size)
int H5Pget_external_count (hid_t plist_id)
herr_t  H5Pget_fill_time (hid_t plist_id, H5D_fill_time_t *fill_time)
herr_t  H5Pget_fill_value (hid_t plist_id, hid_t type_id, void *value)
H5D_layout_t    H5Pget_layout (hid_t plist_id)
herr_t  H5Pget_virtual_count (hid_t dcpl_id, size_t *count)
ssize_t H5Pget_virtual_dsetname (hid_t dcpl_id, size_t index, char *name, size_t size)
ssize_t H5Pget_virtual_filename (hid_t dcpl_id, size_t index, char *name, size_t size)
hid_t   H5Pget_virtual_srcspace (hid_t dcpl_id, size_t index)
hid_t   H5Pget_virtual_vspace (hid_t dcpl_id, size_t index)
herr_t  H5Pset_alloc_time (hid_t plist_id, H5D_alloc_time_t alloc_time)
herr_t  H5Pset_dset_no_attrs_hint (hid_t dcpl_id, hbool_t minimize)
herr_t  H5Pset_external (hid_t plist_id, const char *name, off_t offset, hsize_t size)
herr_t  H5Pset_fill_time (hid_t plist_id, H5D_fill_time_t fill_time)
herr_t  H5Pset_fill_value (hid_t plist_id, hid_t type_id, const void *value)
herr_t  H5Pset_shuffle (hid_t plist_id)

herr_t  H5Pset_nbit (hid_t plist_id)
herr_t  H5Pset_scaleoffset (hid_t plist_id, H5Z_SO_scale_type_t scale_type, int scale_factor)
herr_t  H5Pset_szip (hid_t plist_id, unsigned options_mask, unsigned pixels_per_block)
herr_t  H5Pset_virtual (hid_t dcpl_id, hid_t vspace_id, const char *src_file_name, const char *src_dset_name, hid_t src_space_id)
H5Z_filter_t    H5Pget_filter1 (hid_t plist_id, unsigned filter, unsigned int *flags, size_t *cd_nelmts, unsigned cd_values[], size_t namelen, char name[])
herr_t  H5Pget_filter_by_id1 (hid_t plist_id, H5Z_filter_t id, unsigned int *flags, size_t *cd_nelmts, unsigned cd_values[], size_t namelen, char name[])
*/
