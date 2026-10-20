#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>

#include "../readstat.h"
#include "../readstat_writer.h"
#include "readstat_sas.h"

/* Native 64-bit catalog records have six-byte type/length headers. The catalog
 * directory and XLSR records are required by SAS even though import only needs
 * the value/label pairs. All offsets below are relative to those native records. */
typedef struct sas7bcat_block_s {
    size_t len, records, content_len;
    uint32_t page;
    char *data;
} sas7bcat_block_t;

static void cat_w2(char *p, uint16_t n) { memcpy(p, &n, 2); }
static void cat_w4(char *p, uint32_t n) { memcpy(p, &n, 4); }
static void cat_w8(char *p, uint64_t n) { memcpy(p, &n, 8); }
static void cat_double(char *p, double n) { memcpy(p, &n, 8); }

static uint64_t sas7bcat_key(readstat_value_label_t *label) {
    double value = label->double_key;
    uint64_t key;
    if (label->tag || isnan(value)) {
        unsigned char tag = label->tag;
        unsigned char code = tag == '_' ? 0 :
            tag >= 'A' && tag <= 'Z' ? tag - 'A' + 2 : 1;
        key = UINT64_C(0x000000ffffffffff) | ((uint64_t)code << 40);
    } else if (value >= 0) {
        value = -value;
        memcpy(&key, &value, 8);
    } else {
        memcpy(&key, &value, 8);
        key = ~key;
    }
    return machine_is_little_endian() ? byteswap8(key) : key;
}

static int sas7bcat_compare_labels(const void *a, const void *b) {
    readstat_value_label_t *left = *(readstat_value_label_t **)a;
    readstat_value_label_t *right = *(readstat_value_label_t **)b;
    if (left->string_key || right->string_key) {
        if ((left->sas_rule_flags & 1) != (right->sas_rule_flags & 1))
            return left->sas_rule_flags & 1 ? -1 : 1;
        size_t n = left->string_key_len > right->string_key_len ? left->string_key_len : right->string_key_len;
        for (size_t i=0; i<n; i++) {
            unsigned char l = i < left->string_key_len ? left->string_key[i] : ' ';
            unsigned char r = i < right->string_key_len ? right->string_key[i] : ' ';
            if (l != r) return (l > r) - (l < r);
        }
        return 0;
    }
    uint64_t l = sas7bcat_key(left), r = sas7bcat_key(right);
    return memcmp(&l, &r, 8);
}

static int sas7bcat_make_block(sas7bcat_block_t *block, readstat_label_set_t *set,
        double timestamp, size_t *max_record) {
    size_t count = set->value_labels_count, key_count = 0, default_position = 0;
    int rich = 0;
    int string = set->type == READSTAT_TYPE_STRING;
    size_t name_len = strlen(set->name), max_key = 16, max_label = 0;
    size_t start = name_len > 8 ? 190 : 158;
    size_t values_len = 0, labels_len = 0, values_storage = 0, labels_storage = 0;
    readstat_value_label_t **sorted = calloc(count + 1, sizeof(*sorted));
    if (!sorted)
        return 0;
    for (size_t i=0; i<count; i++) {
        readstat_value_label_t *label = readstat_get_value_label(set, i);
        if (label->sas_rule_flags & 16) default_position = i+1;
        else sorted[key_count++] = label;
        if (label->sas_is_rule) rich = 1;
        if (label->string_key_len > max_key)
            max_key = label->string_key_len;
        if (label->string_end_len > max_key) max_key = label->string_end_len;
        if (label->label_len > max_label)
            max_label = label->label_len;
    }
    /* Keep short character keys in fixed 16-byte fields. Long keys use the
     * native length-prefixed, NUL-padded variant. */
    int long_string = string && max_key > 16;
    for (size_t i=0; i<count; i++) {
        readstat_value_label_t *label = readstat_get_value_label(set, i);
        size_t size = string ? (rich ? (long_string ? 29 + (label->string_key_len ? label->string_key_len : 1) + (label->string_end_len ? label->string_end_len : 1) : 54) : long_string ? 29 + (label->string_key_len ? label->string_key_len : 1) : 38) : 54;
        if (!(label->sas_rule_flags & 16)) {
            values_len += size;
            values_storage += (size-6+7) & ~(size_t)7;
            if (size > *max_record) *max_record = size;
        }
        labels_len += 11 + label->label_len;
        labels_storage += (5+label->label_len+7) & ~(size_t)7;
        if (11 + label->label_len > *max_record) *max_record = 11 + label->label_len;
    }
    block->len = start + values_len + labels_len;
    block->records = 1 + key_count + count;
    block->content_len = block->len - 6 * block->records;
    block->data = calloc(1, block->len);
    if (!block->data) {
        free(sorted);
        return 0;
    }
    char *p = block->data;
    cat_w2(p, 3);
    cat_w2(p+2, 0x98 | (name_len > 8 ? 0x20 : 0));
    memset(p+8, ' ', 8);
    memcpy(p+8, set->name, name_len > 8 ? 8 : name_len);
    cat_double(p+22, timestamp);
    cat_w2(p+30, string ? 11 : 1);
    cat_w2(p+32, 1);
    cat_w2(p+34, 40);
    cat_w2(p+36, set->sas_default_width ? set->sas_default_width : max_label);
    cat_w2(p+38, max_label);
    cat_w8(p+46, key_count);
    cat_w8(p+54, count);
    cat_double(p+62, rich ? set->sas_fuzz : string ? 0 : 1e-12);
    cat_w8(p+78, default_position);
    cat_w4(p+72, string ? 6 : 4);
    cat_w8(p+86, labels_storage);
    cat_w8(p+94, values_storage);
    cat_w2(p+118, max_key);
    cat_w2(p+120, max_key);
    cat_w2(p+122, max_label);
    cat_w2(p+124, 1);
    p[127] = string && !rich ? 1 : 2;
    cat_w4(p+128, long_string);
    cat_w4(p+134, long_string ? 4 : 0);
    if (name_len > 8) {
        memset(p+158, ' ', 32);
        memcpy(p+158, set->name, name_len);
    }
    qsort(sorted, key_count, sizeof(*sorted), sas7bcat_compare_labels);
    p += start;
    for (size_t i=0; i<key_count; i++) {
        readstat_value_label_t *label = sorted[i];
        size_t size = string ? (rich ? (long_string ? 29 + (label->string_key_len ? label->string_key_len : 1) + (label->string_end_len ? label->string_end_len : 1) : 54) : long_string ? 29 + (label->string_key_len ? label->string_key_len : 1) : 38) : 54;
        cat_w2(p, 4);
        cat_w2(p+2, size-6);
        cat_w2(p+6, string ? (rich ? (long_string ? 4 : 11) : long_string ? 3 : 10) : 1);
        p[7] = label->sas_rule_flags & 15;
        /* Labels remain in caller order; sorted keys point to their label slot. */
        size_t index = label - set->value_labels;
        cat_w4(p+14, index);
        if (string) {
            if (rich && long_string) {
                size_t sl = label->string_key_len ? label->string_key_len : 1;
                size_t el = label->string_end_len ? label->string_end_len : 1;
                cat_w2(p+8,sl); cat_w2(p+10,el);
                if (label->string_key_len) memcpy(p+22,label->string_key,sl);
                else p[22] = ' ';
                if (label->string_end_len) memcpy(p+22+sl,label->string_end,el);
                else p[22+sl] = ' ';
                if (label->sas_rule_flags & 1) p[22] = 0;
                if (label->sas_rule_flags & 2) p[22+sl] = (char)0xff;
            } else if (rich) {
                memset(p+22, ' ', 32);
                if (label->string_key_len) memcpy(p+22, label->string_key, label->string_key_len);
                if (label->string_end_len) memcpy(p+38, label->string_end, label->string_end_len);
                if (label->sas_rule_flags & 1) p[22] = 0;
                if (label->sas_rule_flags & 2) p[38] = (char)0xff;
            } else if (long_string) {
                cat_w2(p+8, label->string_key_len ? label->string_key_len : 1);
                if (label->string_key_len)
                    memcpy(p+23, label->string_key, label->string_key_len);
                else
                    p[23] = ' ';
            } else {
                memset(p+22, ' ', 16);
                if (label->string_key_len)
                    memcpy(p+22, label->string_key, label->string_key_len);
            }
        } else {
            uint64_t key = sas7bcat_key(label);
            /* Rules carry distinct endpoints and fuzz-expanded comparison bounds. */
            readstat_value_label_t bound = *label;
            if (label->sas_is_rule) { bound.double_key = label->end_key; bound.tag = label->end_tag; }
            uint64_t end = sas7bcat_key(&bound);
            memcpy(p+22, &key, 8); memcpy(p+30, &end, 8);
            uint64_t lower = key, upper = end;
            if (label->sas_is_rule && set->sas_fuzz > 0) {
                if (!label->tag) {
                    bound.double_key = fmax(-DBL_MAX, label->double_key-set->sas_fuzz);
                    bound.tag = 0; lower = sas7bcat_key(&bound);
                }
                if (!label->end_tag) {
                    bound.double_key = fmin(DBL_MAX, label->end_key+set->sas_fuzz);
                    bound.tag = 0; upper = sas7bcat_key(&bound);
                }
            }
            memcpy(p+38, &lower, 8); memcpy(p+46, &upper, 8);
        }
        p += size;
    }
    for (size_t i=0; i<count; i++) {
        readstat_value_label_t *label = readstat_get_value_label(set, i);
        cat_w2(p, 5);
        cat_w2(p+2, 5+label->label_len);
        cat_w2(p+6, 6);
        cat_w2(p+8, label->label_len);
        memcpy(p+10, label->label, label->label_len);
        p += 11+label->label_len;
    }
    free(sorted);
    return 1;
}

static void sas7bcat_page(char *page, size_t size, uint32_t mask, uint32_t number) {
    memset(page, 0, size);
    cat_w4(page, mask ^ number);
}

static void sas7bcat_xlsr(char *p, uint32_t page, size_t records, size_t content,
        unsigned flags, unsigned key_width, unsigned label_width, size_t page_size, double timestamp, int bitmap) {
    memcpy(p, "XLSR", 4);
    cat_w8(p+8, page);
    cat_w2(p+16, 32);
    cat_double(p+24, timestamp);
    cat_double(p+32, timestamp);
    cat_w8(p+40, bitmap ? 20 : 0);
    cat_w8(p+48, records);
    cat_w8(p+56, content);
    cat_w8(p+64, flags);
    cat_w2(p+72, page_size);
    memset(p+78, ' ', 18);
    if (!bitmap) {
        char descriptor[19];
        snprintf(descriptor, sizeof(descriptor), "O%04X%04X%04X0000 ",
            key_width, key_width, label_width);
        memcpy(p+78, descriptor, 18);
    }
    cat_w8(p+104, page);
    cat_w2(p+112, 32);
    memset(p+172, ' ', 8);
    if (!bitmap)
        p[171] = 'F';
    cat_w8(p+184, 1);
    cat_w8(p+192, 1);
    if (!bitmap)
        memset(p+244, ' ', 44);
}

typedef struct sas7bcat_directory_s {
    char **pages;
    size_t used, capacity, page_size;
    uint32_t first_page, mask, last_leaf;
} sas7bcat_directory_t;

/* Internal nodes have an ascending offset table of 40-byte upper-bound keys
 * and eight-byte child pointers. Leaves hold fixed 56-byte catalog entries. */
static uint32_t sas7bcat_directory_node(sas7bcat_directory_t *tree,
        const char *entries, size_t count, size_t level, int root) {
    if (tree->used == tree->capacity) {
        size_t capacity = tree->capacity ? tree->capacity*2 : 8;
        char **pages = realloc(tree->pages, capacity*sizeof(*pages));
        if (!pages)
            return 0;
        tree->pages = pages;
        tree->capacity = capacity;
    }
    size_t slot = tree->used++;
    uint32_t number = root ? 1 : tree->first_page + slot-1;
    char *page = calloc(1, tree->page_size);
    tree->pages[slot] = page;
    if (!page)
        return 0;
    cat_w4(page, tree->mask ^ number);
    cat_w8(page+24, 1);
    cat_w2(page+32, 0x1000);
    cat_w4(page+40, level);
    if (level == 0) {
        cat_w2(page+32, tree->last_leaf ? 0 : 0x1000);
        cat_w8(page+16, tree->last_leaf);
        tree->last_leaf = number;
        cat_w2(page+34, count+1);
        cat_w2(page+36, count);
        cat_w2(page+38, tree->page_size-56*count-48);
        memcpy(page+tree->page_size-56*count, entries, 56*count);
        return number;
    }
    size_t leaf_capacity = (tree->page_size-48)/56;
    size_t branch_capacity = (tree->page_size-48)/50;
    size_t child_capacity = leaf_capacity;
    for (size_t i=1; i<level; i++)
        child_capacity *= branch_capacity;
    size_t children = (count+child_capacity-1)/child_capacity;
    cat_w2(page+34, children+1);
    cat_w2(page+36, children);
    cat_w2(page+38, tree->page_size-48*children-48-2*children);
    for (size_t child=0; child<children; child++) {
        size_t offset = child*child_capacity;
        size_t size = count-offset > child_capacity ? child_capacity : count-offset;
        uint32_t target = sas7bcat_directory_node(tree, entries+56*offset, size, level-1, 0);
        if (!target)
            return 0;
        size_t record_offset = tree->page_size-48*(children-child);
        char *record = page+record_offset;
        if (child == 0)
            memset(record, 0xff, 40);
        else
            memcpy(record, entries+56*offset, 40);
        cat_w8(record+40, target);
        cat_w2(page+48+2*(children-child-1), record_offset);
    }
    return number;
}

static int sas7bcat_directory_compare(const void *a, const void *b) {
    return memcmp(b, a, 40);
}

static readstat_error_t sas7bcat_begin_data(void *writer_ctx) {
    readstat_writer_t *writer = writer_ctx;
    readstat_error_t retval = READSTAT_OK;
    size_t count = writer->label_sets_count;
    sas_header_info_t *hinfo = sas_header_info_init(writer, 1);
    sas7bcat_block_t *blocks = calloc(count+1, sizeof(*blocks));
    char *page = NULL, *entries = NULL;
    sas7bcat_directory_t tree = { 0 };
    if (!hinfo || !blocks) {
        retval = READSTAT_ERROR_MALLOC;
        goto cleanup;
    }
    double timestamp = (double)writer->timestamp + 315619200.0;
    size_t max_record = 190;
    for (size_t i=0; i<count; i++) {
        if (!sas7bcat_make_block(&blocks[i], writer->label_sets[i], timestamp, &max_record)) {
            retval = READSTAT_ERROR_MALLOC;
            goto cleanup;
        }
    }
    /* Each individual record must fit on a page. The directory and index
     * have independent overflow pages, so the number of formats is not capped. */
    hinfo->header_size = 8192;
    while (hinfo->page_size < 64 + max_record) {
        hinfo->page_size *= 2;
    }
    if (hinfo->page_size > 65536) {
        retval = READSTAT_ERROR_ROW_IS_TOO_WIDE_FOR_PAGE;
        goto cleanup;
    }
    hinfo->page_number_mask = UINT32_C(0x12345678);
    page = malloc(hinfo->page_size);
    if (!page) {
        retval = READSTAT_ERROR_MALLOC;
        goto cleanup;
    }
    size_t first_capacity = (hinfo->page_size-1296)/288;
    size_t extra_capacity = (hinfo->page_size-32)/288;
    size_t extra_pages = count > first_capacity ?
        (count-first_capacity+extra_capacity-1)/extra_capacity : 0;
    entries = calloc(count+1, 56);
    if (!entries) {
        retval = READSTAT_ERROR_MALLOC;
        goto cleanup;
    }
    memcpy(entries, "SYSRESR PGBITMAP", 16);
    cat_w8(entries+40, 2);
    cat_w2(entries+48, 1008);
    cat_w2(entries+50, 0x810);
    for (size_t i=0; i<count; i++) {
        readstat_label_set_t *set = writer->label_sets[i];
        char *entry = entries+56*(i+1);
        int string = set->type == READSTAT_TYPE_STRING;
        memcpy(entry, string ? "FORMATC " : "FORMAT  ", 8);
        memcpy(entry+8, set->name+string, strlen(set->name)-string);
        cat_w8(entry+40, i < first_capacity ? 2 : 4+(i-first_capacity)/extra_capacity);
        cat_w2(entry+48, i < first_capacity ? 1296+288*i : 32+288*((i-first_capacity)%extra_capacity));
        entry[51] = strlen(set->name)-string;
    }
    qsort(entries, count+1, 56, sas7bcat_directory_compare);
    tree.page_size = hinfo->page_size;
    tree.first_page = 4+extra_pages;
    tree.mask = hinfo->page_number_mask;
    size_t level = 0, capacity = (hinfo->page_size-48)/56;
    while (capacity < count+1) {
        level++;
        capacity *= (hinfo->page_size-48)/50;
    }
    if (!sas7bcat_directory_node(&tree, entries, count+1, level, 1)) {
        retval = READSTAT_ERROR_MALLOC;
        goto cleanup;
    }
    size_t chunk = hinfo->page_size-64;
    hinfo->page_count = 3+extra_pages+tree.used-1;
    for (size_t i=0; i<count; i++) {
        blocks[i].page = hinfo->page_count+1;
        hinfo->page_count += (blocks[i].len+chunk-1)/chunk;
    }
    sas_header_start_t header = {
        .a1 = SAS_ALIGNMENT_OFFSET_4, .a2 = SAS_ALIGNMENT_OFFSET_4,
        .endian = machine_is_little_endian() ? SAS_ENDIAN_LITTLE : SAS_ENDIAN_BIG,
        .file_format = SAS_FILE_FORMAT_UNIX, .encoding = 20,
        .file_type = "SAS FILE", .file_info = "CATALOG "
    };
    memcpy(header.magic, sas7bcat_magic_number, sizeof(header.magic));
    char table_name[sizeof(writer->table_name)];
    memcpy(table_name, writer->table_name, sizeof(table_name));
    memset(writer->table_name, ' ', 32);
    writer->table_name[32] = 0;
    retval = sas_write_header(writer, hinfo, header);
    memcpy(writer->table_name, table_name, sizeof(table_name));
    if (retval != READSTAT_OK)
        goto cleanup;

    if ((retval = readstat_write_bytes(writer, tree.pages[0], hinfo->page_size)) != READSTAT_OK)
        goto cleanup;

    sas7bcat_page(page, hinfo->page_size, hinfo->page_number_mask, 2);
    memcpy(page+32, "XLCH", 4);
    cat_w8(page+300, 32);
    cat_w8(page+312, hinfo->page_size);
    cat_w8(page+320, hinfo->page_count);
    cat_w8(page+336, 1);
    cat_w8(page+344, extra_pages ? 3+extra_pages : 2);
    cat_w8(page+352, extra_pages ? 32+288*((count-first_capacity-1)%extra_capacity+1) : 1296+288*count);
    cat_w8(page+376, 1);
    cat_w4(page+384, 0x280000);
    cat_w8(page+392, count);
    cat_w8(page+400, count);
    sas7bcat_xlsr(page+1008, 3, 1, 256, 256, 0, 0, hinfo->page_size, timestamp, 1);
    for (size_t i=0; i<count && i<first_capacity; i++)
        sas7bcat_xlsr(page+1296+288*i, blocks[i].page, blocks[i].records,
            blocks[i].content_len, (unsigned char)blocks[i].data[2],
            sas_read2(blocks[i].data+118, 0), sas_read2(blocks[i].data+122, 0), hinfo->page_size, timestamp, 0);
    if ((retval = readstat_write_bytes(writer, page, hinfo->page_size)) != READSTAT_OK)
        goto cleanup;

    sas7bcat_page(page, hinfo->page_size, hinfo->page_number_mask, 3);
    cat_w2(page+42, 262);
    cat_w8(page+48, 3);
    cat_w2(page+56, 32);
    cat_w2(page+58, hinfo->page_size-32);
    cat_w2(page+64, 106);
    cat_w2(page+66, 256);
    if ((retval = readstat_write_bytes(writer, page, hinfo->page_size)) != READSTAT_OK)
        goto cleanup;

    for (size_t index=0; index<extra_pages; index++) {
        sas7bcat_page(page, hinfo->page_size, hinfo->page_number_mask, 4+index);
        for (size_t item=0; item<extra_capacity; item++) {
            size_t i = first_capacity+index*extra_capacity+item;
            if (i == count)
                break;
            sas7bcat_xlsr(page+32+288*item, blocks[i].page, blocks[i].records,
                blocks[i].content_len, (unsigned char)blocks[i].data[2],
                sas_read2(blocks[i].data+118, 0), sas_read2(blocks[i].data+122, 0), hinfo->page_size, timestamp, 0);
        }
        if ((retval = readstat_write_bytes(writer, page, hinfo->page_size)) != READSTAT_OK)
            goto cleanup;
    }
    for (size_t i=1; i<tree.used; i++) {
        if ((retval = readstat_write_bytes(writer, tree.pages[i], hinfo->page_size)) != READSTAT_OK)
            goto cleanup;
    }
    for (size_t i=0; i<count; i++) {
        for (size_t offset=0; offset<blocks[i].len; offset+=chunk) {
            size_t remaining = blocks[i].len-offset;
            size_t length = remaining > chunk ? chunk : remaining;
            uint32_t number = blocks[i].page + offset/chunk;
            sas7bcat_page(page, hinfo->page_size, hinfo->page_number_mask, number);
            cat_w8(page+32, remaining > chunk ? number+1 : 0);
            cat_w2(page+40, remaining > chunk ? 32 : 0);
            cat_w2(page+42, length);
            cat_w8(page+48, number);
            cat_w2(page+56, 32);
            cat_w2(page+58, hinfo->page_size-32);
            memcpy(page+64, blocks[i].data+offset, length);
            if ((retval = readstat_write_bytes(writer, page, hinfo->page_size)) != READSTAT_OK)
                goto cleanup;
        }
    }
cleanup:
    if (blocks) {
        for (size_t i=0; i<count; i++)
            free(blocks[i].data);
    }
    free(blocks);
    for (size_t i=0; i<tree.used; i++)
        free(tree.pages[i]);
    free(tree.pages);
    free(entries);
    free(page);
    free(hinfo);
    return retval;
}

readstat_error_t readstat_begin_writing_sas7bcat(readstat_writer_t *writer, void *user_ctx) {
    if (writer->version == 0)
        writer->version = SAS_DEFAULT_FILE_VERSION;
    writer->is_64bit = 1;
    writer->callbacks.begin_data = sas7bcat_begin_data;
    return readstat_begin_writing_file(writer, user_ctx, 0);
}
