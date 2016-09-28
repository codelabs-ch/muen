/*
 * Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
 * Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

#define MUEN_SUBJECT_INFO_MAGIC	0x01006f666e69756dULL
#define MAX_NAME_LENGTH		63
#define HASH_LENGTH			32
#define MAX_RESOURCE_COUNT	255
#define NO_RESOURCE			0
#define NO_PATTERN			256

struct name_type {
	uint8_t length;
	char data[MAX_NAME_LENGTH];
} __attribute__((packed));

#define MEM_WRITABLE_FLAG   (1 << 0)
#define MEM_EXECUTABLE_FLAG (1 << 1)

enum content_type {content_uninitialized, content_fill, content_file};

struct memregion_type {
	enum content_type content;
	uint64_t address;
	uint64_t size;
	uint8_t hash[HASH_LENGTH];
	uint8_t flags;
	uint16_t pattern;
	char padding[1];
} __attribute__((packed, aligned (8)));

#define CHAN_EVENT_FLAG  (1 << 0)
#define CHAN_VECTOR_FLAG (1 << 1)

struct channel_info_type {
	uint8_t flags;
	uint8_t event;
	uint8_t vector;
	char padding[5];
} __attribute__((packed, aligned (8)));

struct resource_type {
	struct name_type name;
	uint8_t memregion_idx;
	uint8_t channel_info_idx;
	char padding[6];
} __attribute__((packed, aligned (8)));

struct dev_info_type {
	uint16_t sid;
	uint16_t irte_start;
	uint8_t irq_start;
	uint8_t ir_count;
	uint8_t flags;
	char padding[1];
} __attribute__((packed, aligned (8)));

#define DEV_MSI_FLAG  (1 << 0)

struct subject_info_type {
	uint64_t magic;
	uint8_t resource_count;
	uint8_t memregion_count;
	uint8_t channel_info_count;
	uint8_t dev_info_count;
	char padding[4];
	uint64_t tsc_khz;
	uint64_t tsc_schedule_start;
	uint64_t tsc_schedule_end;
	struct resource_type resources[MAX_RESOURCE_COUNT];
	struct memregion_type memregions[MAX_RESOURCE_COUNT];
	struct channel_info_type channels_info[MAX_RESOURCE_COUNT];
	struct dev_info_type dev_info[MAX_RESOURCE_COUNT];
} __attribute__((packed, aligned (8)));

#endif /* MUSINFO_H_  */
