/*
 * Copyright (C) 2014-2018  Reto Buerki <reet@codelabs.ch>
 * Copyright (C) 2014-2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef MUSINFO_H_
#define MUSINFO_H_

#define MUEN_SUBJECT_INFO_MAGIC	0x02006f666e69756dULL

#define MAX_RESOURCE_COUNT	255
#define MAX_NAME_LENGTH		63
#define HASH_LENGTH		32
#define NO_PATTERN		256

#define MEM_WRITABLE_FLAG	(1 << 0)
#define MEM_EXECUTABLE_FLAG	(1 << 1)
#define MEM_CHANNEL_FLAG	(1 << 2)

#define DEV_MSI_FLAG		(1 << 0)

/* Resource name */
struct muen_name_type {
	uint8_t length;
	char data[MAX_NAME_LENGTH];
	uint8_t null_term;
} __attribute__((packed));

/* Known memory contents */
enum muen_content_kind {
	MUEN_CONTENT_UNINITIALIZED, MUEN_CONTENT_FILL, MUEN_CONTENT_FILE
};

/* Structure holding information about a memory region */
struct muen_memregion_type {
	enum muen_content_kind content;
	uint64_t address;
	uint64_t size;
	uint8_t hash[HASH_LENGTH];
	uint8_t flags;
	uint16_t pattern;
	char padding[1];
} __attribute__((packed, aligned(8)));

/* Required for explicit padding */
#define largest_variant_size sizeof(struct muen_memregion_type)
#define device_type_size 7

/* Structure holding information about a PCI device */
struct muen_device_type {
	uint16_t sid;
	uint16_t irte_start;
	uint8_t irq_start;
	uint8_t ir_count;
	uint8_t flags;
	char padding[largest_variant_size - device_type_size];
} __attribute__((packed, aligned(8)));

/* Currently known resource types */
enum muen_resource_kind {
	MUEN_RES_NONE, MUEN_RES_MEMORY, MUEN_RES_EVENT, MUEN_RES_VECTOR,
	MUEN_RES_DEVICE
};

/* Resource data depending on the kind of resource */
union muen_resource_data {
	struct muen_memregion_type mem;
	struct muen_device_type dev;
	uint8_t number;
};

/* Exported resource with associated name */
struct muen_resource_type {
	enum muen_resource_kind kind;
	struct muen_name_type name;
	char padding[3];
	union muen_resource_data data;
} __attribute__((packed, aligned(8)));

/* Muen subject information (sinfo) structure */
struct subject_info_type {
	uint64_t magic;
	uint32_t tsc_khz;
	struct muen_name_type name;
	uint16_t resource_count;
	char padding[1];
	struct muen_resource_type resources[MAX_RESOURCE_COUNT];
} __attribute__((packed, aligned (8)));

#endif /* MUSINFO_H_  */
