/*
 * Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
 * Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 */

#ifndef MUSINFO_H_
#define MUSINFO_H_

#define MUEN_SUBJECT_INFO_MAGIC	0x01006f666e69756dULL
#define MAX_NAME_LENGTH		63
#define MAX_RESOURCE_COUNT	255
#define NO_RESOURCE			0

struct name_type {
	uint8_t length;
	char data[MAX_NAME_LENGTH];
} __attribute__((packed));

#define MEM_WRITABLE_FLAG   (1 << 0)
#define MEM_EXECUTABLE_FLAG (1 << 1)

struct memregion_type {
	uint64_t address;
	uint64_t size;
	uint8_t flags;
	char padding[7];
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

struct subject_info_type {
	uint64_t magic;
	uint8_t resource_count;
	uint8_t memregion_count;
	uint8_t channel_info_count;
	char padding[5];
	uint64_t tsc_khz;
	struct resource_type resources[MAX_RESOURCE_COUNT];
	struct memregion_type memregions[MAX_RESOURCE_COUNT];
	struct channel_info_type channels_info[MAX_RESOURCE_COUNT];
} __attribute__((packed, aligned (8)));

#endif /* MUSINFO_H_  */
