/*
 * Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
 * Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MUSINFO_H_
#define MUSINFO_H_

#include <stdint.h>

#define MUEN_SUBJECT_INFO_MAGIC	0x01006f666e69756d
#define MAX_NAME_LENGTH			63
#define MAX_CHANNEL_COUNT		255

struct name_type {
	uint8_t length;
	char data[MAX_NAME_LENGTH];
} __attribute__((packed));

#define WRITABLE_FLAG	1 << 0
#define HAS_EVENT_FLAG	1 << 1
#define HAS_VECTOR_FLAG	1 << 2

struct channel_type {
	struct name_type name;
	uint64_t address;
	uint64_t size;
	uint8_t flags;
	uint8_t event;
	uint8_t vector;
	char padding[5];
} __attribute__((packed));

struct subject_info_type {
	uint64_t magic;
	uint8_t channel_count;
	char padding[7];
	struct channel_type channels[MAX_CHANNEL_COUNT];
} __attribute__((packed));

#endif /* MUSINFO_H_  */
