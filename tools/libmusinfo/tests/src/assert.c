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

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

#include "musinfo.h"

int assert_name(const struct name_type * const name)
{
	if (name->length != MAX_NAME_LENGTH)
	{
		printf("Name: Invalid length %d\n", name->length);
		return 0;
	}

	int i;
	for (i = 0; i < name->length; i++)
	{
		if (name->data[i] != 'a')
		{
			printf("Name: Invalid character '%c' at position %d\n",
					name->data[i], i);
			return 0;
		}
	}
	return 1;
}

int assert_name_type(const int size, const int alignment,
		const int length_offset, const int data_offset)
{
	if (sizeof(struct name_type) != size)
	{
		printf("Name: Invalid size %d /= %d\n", size, sizeof(struct name_type));
		return 0;
	}
	if (__alignof__ (struct name_type) != alignment)
	{
		printf("Name: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct name_type));
		return 0;
	}

	if (offsetof(struct name_type, length) != length_offset)
	{
		printf("Name: Invalid 'length' offset %d /= %d\n", length_offset,
				offsetof(struct name_type, length));
		return 0;
	}

	if (offsetof(struct name_type, data) != data_offset)
	{
		printf("Name: Invalid 'data' offset %d /= %d\n", data_offset,
				offsetof(struct name_type, data));
		return 0;
	}

	return 1;
}

int assert_memregion(const struct memregion_type * const memregion)
{
	int i;

	if (memregion->kind != content_fill)
	{
		printf("Memregion: Invalid kind 0x%u\n", memregion->kind);
		return 0;
	}

	if (memregion->address != 0xdeadbeefcafefeed)
	{
		printf("Memregion: Invalid address 0x%lx\n", memregion->address);
		return 0;
	}

	if (memregion->size != 0x8080ababcdcd9090)
	{
		printf("Memregion: Invalid size field 0x%lx\n", memregion->size);
		return 0;
	}

	for (i = 0; i < HASH_LENGTH; i++)
	{
		if (memregion->hash[i] != 253)
		{
			printf("Memregion: Invalid hash value %u at position %d\n",
					memregion->hash[i], i);
			return 0;
		}
	}

	if (!(memregion->flags & MEM_WRITABLE_FLAG))
	{
		printf("Memregion: Writable flag not set\n");
		return 0;
	}

	if (!(memregion->flags & MEM_EXECUTABLE_FLAG))
	{
		printf("Memregion: Executable flag not set\n");
		return 0;
	}

	if (memregion->pattern != 45)
	{
		printf("Memregion: Invalid pattern %u\n", memregion->pattern);
		return 0;
	}

	return 1;
}

int assert_memregion_type(const int size, const int alignment,
		const int kind_offset, const int address_offset, const int size_offset,
		const int hash_offset, const int flags_offset, const int pattern_offset)
{
	if (sizeof(struct memregion_type) != size)
	{
		printf("Memregion: Invalid struct size %d /= %d\n", size,
				sizeof(struct memregion_type));
		return 0;
	}

	if (__alignof__ (struct memregion_type) != alignment)
	{
		printf("Memregion: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct memregion_type));
		return 0;
	}

	if (offsetof(struct memregion_type, kind) != kind_offset)
	{
		printf("Memregion: Invalid 'kind' offset %d /= %d\n", kind_offset,
				offsetof(struct memregion_type, kind));
		return 0;
	}

	if (offsetof(struct memregion_type, address) != address_offset)
	{
		printf("Memregion: Invalid 'address' offset %d /= %d\n", address_offset,
				offsetof(struct memregion_type, address));
		return 0;
	}

	if (offsetof(struct memregion_type, size) != size_offset)
	{
		printf("Memregion: Invalid 'size' offset %d /= %d\n", size_offset,
				offsetof(struct memregion_type, size));
		return 0;
	}

	if (offsetof(struct memregion_type, hash) != hash_offset)
	{
		printf("Memregion: Invalid 'hash' offset %d /= %d\n", hash_offset,
				offsetof(struct memregion_type, hash));
		return 0;
	}

	if (offsetof(struct memregion_type, flags) != flags_offset)
	{
		printf("Memregion: Invalid 'flags' offset %d /= %d\n", flags_offset,
				offsetof(struct memregion_type, flags));
		return 0;
	}

	if (offsetof(struct memregion_type, pattern) != pattern_offset)
	{
		printf("Memregion: Invalid 'pattern' offset %d /= %d\n", pattern_offset,
				offsetof(struct memregion_type, pattern));
		return 0;
	}

	return 1;
}

int assert_channel_info(const struct channel_info_type * const channel_info)
{
	if (!(channel_info->flags & CHAN_EVENT_FLAG))
	{
		printf("Channel: Has_Event flag not set\n");
		return 0;
	}
	if (!(channel_info->flags & CHAN_VECTOR_FLAG))
	{
		printf("Channel: Has_Vector flag not set\n");
		return 0;
	}

	if (channel_info->event != 128)
	{
		printf("Channel: Invalid event number %d\n", channel_info->event);
		return 0;
	}

	if (channel_info->vector != 255)
	{
		printf("Channel: Invalid vector number %d\n", channel_info->vector);
		return 0;
	}

	return 1;
}

int assert_channel_info_type(const int size, const int alignment,
		const int flags_offset, const int event_offset, const int vector_offset)
{
	if (sizeof(struct channel_info_type) != size)
	{
		printf("Channel: Invalid size %d /= %d\n", size,
				sizeof(struct channel_info_type));
		return 0;
	}
	if (__alignof__ (struct channel_info_type) != alignment)
	{
		printf("Channel: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct channel_info_type));
		return 0;
	}

	if (offsetof(struct channel_info_type, flags) != flags_offset)
	{
		printf("Channel: Invalid 'flags' offset %d /= %d\n", flags_offset,
				offsetof(struct channel_info_type, flags));
		return 0;
	}

	if (offsetof(struct channel_info_type, event) != event_offset)
	{
		printf("Channel: Invalid 'event' offset %d /= %d\n", event_offset,
				offsetof(struct channel_info_type, event));
		return 0;
	}

	if (offsetof(struct channel_info_type, vector) != vector_offset)
	{
		printf("Channel: Invalid 'vector' offset %d /= %d\n", vector_offset,
				offsetof(struct channel_info_type, vector));
		return 0;
	}

	return 1;
}

int assert_resource(const struct resource_type * const resource)
{
	if (!assert_name(&resource->name))
	{
		return 0;
	}

	if (resource->memregion_idx != 23)
	{
		printf("Resource: Invalid memregion index %d\n",
				resource->memregion_idx);
		return 0;
	}

	if (resource->channel_info_idx != 42)
	{
		printf("Resource: Invalid channel info index %d\n",
				resource->channel_info_idx);
		return 0;
	}

	return 1;
}

int assert_resource_type(const int size, const int alignment,
		const int name_offset, const int memregion_idx_offset,
		const int chaninfo_idx_offset)
{
	if (sizeof(struct resource_type) != size)
	{
		printf("Resource: Invalid size %d /= %d\n", size,
				sizeof(struct resource_type));
		return 0;
	}
	if (__alignof__ (struct resource_type) != alignment)
	{
		printf("Resource: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct resource_type));
		return 0;
	}

	if (offsetof(struct resource_type, name) != name_offset)
	{
		printf("Resource: Invalid 'name' offset %d /= %d\n", name_offset,
				offsetof(struct resource_type, name));
		return 0;
	}

	if (offsetof(struct resource_type, memregion_idx) != memregion_idx_offset)
	{
		printf("Resource: Invalid 'memregion_idx' offset %d /= %d\n",
				memregion_idx_offset,
				offsetof(struct resource_type, memregion_idx));
		return 0;
	}

	if (offsetof(struct resource_type, channel_info_idx) != chaninfo_idx_offset)
	{
		printf("Resource: Invalid 'channel_info_idx' offset %d /= %d\n",
				chaninfo_idx_offset,
				offsetof(struct resource_type, channel_info_idx));
		return 0;
	}

	return 1;
}

int assert_dev_info(const struct dev_info_type * const dev_info)
{
	if (dev_info->sid != 0xabcd)
	{
		printf("Dev: Invalid SID 0x%x\n", dev_info->sid);
		return 0;
	}

	if (dev_info->irte_start != 200)
	{
		printf("Dev: Invalid IRTE start %d\n", dev_info->irte_start);
		return 0;
	}

	if (dev_info->irq_start != 12)
	{
		printf("Dev: Invalid IRQ start %d\n", dev_info->irq_start);
		return 0;
	}

	if (dev_info->ir_count != 22)
	{
		printf("Dev: Invalid IR count %d\n", dev_info->ir_count);
		return 0;
	}

	if (!(dev_info->flags & DEV_MSI_FLAG))
	{
		printf("Dev: MSI flag not set\n");
		return 0;
	}

	return 1;
}

int assert_dev_info_type(const int size, const int alignment,
			 const int irte_start_offset, const int irq_start_offset,
			 const int ir_count_offset, const int flags_offset)
{
	if (sizeof(struct dev_info_type) != size)
	{
		printf("Dev: Invalid size %d /= %d\n", size,
				sizeof(struct dev_info_type));
		return 0;
	}
	if (__alignof__ (struct dev_info_type) != alignment)
	{
		printf("Dev: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct dev_info_type));
		return 0;
	}

	if (offsetof(struct dev_info_type, irte_start) != irte_start_offset)
	{
		printf("Dev: Invalid 'irte_start' offset %d /= %d\n", irte_start_offset,
				offsetof(struct dev_info_type, irte_start));
		return 0;
	}

	if (offsetof(struct dev_info_type, irq_start) != irq_start_offset)
	{
		printf("Dev: Invalid 'irq_start' offset %d /= %d\n",
				irq_start_offset,
				offsetof(struct dev_info_type, irq_start));
		return 0;
	}

	if (offsetof(struct dev_info_type, ir_count) != ir_count_offset)
	{
		printf("Dev: Invalid 'ir_count' offset %d /= %d\n",
				ir_count_offset,
				offsetof(struct dev_info_type, ir_count));
		return 0;
	}

	if (offsetof(struct dev_info_type, flags) != flags_offset)
	{
		printf("Dev: Invalid 'flags' offset %d /= %d\n",
				flags_offset,
				offsetof(struct dev_info_type, flags));
		return 0;
	}

	return 1;
}

int assert_subject_info(const struct subject_info_type * const info)
{
	if (info->magic != MUEN_SUBJECT_INFO_MAGIC)
	{
		printf("Sinfo: Invalid magic '%lx'\n", info->magic);
		return 0;
	}

	if (info->channel_info_count != MAX_RESOURCE_COUNT)
	{
		printf("Sinfo: Invalid channel info count %d\n",
				info->channel_info_count);
		return 0;
	}

	int i;
	for (i = 0; i < info->channel_info_count; i++)
	{
		if (!assert_channel_info(&info->channels_info[i]))
		{
			return 0;
		}
	}

	return 1;
}

int assert_subject_info_type(const int size, const int alignment,
		const int magic_offset, const int res_count_offset,
		const int memreg_count_offset, const int chan_count_offset,
		const int dev_count_offset,
		const int tsc_khz_offset, const int tsc_schd_start_offset,
		const int tsc_schd_end_offset, const int resources_offset,
		const int memregions_offset, const int chan_info_offset,
		const int dev_info_offset)
{
	if (sizeof(struct subject_info_type) != size)
	{
		printf("Sinfo: Invalid size %d /= %d\n", size,
				sizeof(struct subject_info_type));
		return 0;
	}
	if (__alignof__ (struct subject_info_type) != alignment)
	{
		printf("Sinfo: Invalid alignment %d /= %d\n", alignment,
				__alignof__ (struct subject_info_type));
		return 0;
	}

	if (offsetof(struct subject_info_type, magic) != magic_offset)
	{
		printf("Sinfo: Invalid 'magic' offset %d /= %d\n", magic_offset,
				offsetof(struct subject_info_type, magic));
		return 0;
	}

	if (offsetof(struct subject_info_type, resource_count) != res_count_offset)
	{
		printf("Sinfo: Invalid 'resource_count' offset %d /= %d\n",
				res_count_offset,
				offsetof(struct subject_info_type, resource_count));
		return 0;
	}

	if (offsetof(struct subject_info_type, memregion_count)
			!= memreg_count_offset)
	{
		printf("Sinfo: Invalid 'memregion__count' offset %d /= %d\n",
				memreg_count_offset,
				offsetof(struct subject_info_type, memregion_count));
		return 0;
	}

	if (offsetof(struct subject_info_type, channel_info_count)
			!= chan_count_offset)
	{
		printf("Sinfo: Invalid 'channel_info_count' offset %d /= %d\n",
				chan_count_offset,
				offsetof(struct subject_info_type, channel_info_count));
		return 0;
	}

	if (offsetof(struct subject_info_type, dev_info_count)
			!= dev_count_offset)
	{
		printf("Sinfo: Invalid 'dev_info_count' offset %d /= %d\n",
				dev_count_offset,
				offsetof(struct subject_info_type, dev_info_count));
		return 0;
	}

	if (offsetof(struct subject_info_type, tsc_khz) != tsc_khz_offset)
	{
		printf("Sinfo: Invalid 'tsc_khz' offset %d /= %d\n", tsc_khz_offset,
				offsetof(struct subject_info_type, tsc_khz));
		return 0;
	}

	if (offsetof(struct subject_info_type, tsc_schedule_start)
			!= tsc_schd_start_offset)
	{
		printf("Sinfo: Invalid 'tsc_schedule_start' offset %d /= %d\n",
				tsc_schd_start_offset,
				offsetof(struct subject_info_type, tsc_schedule_start));
		return 0;
	}

	if (offsetof(struct subject_info_type, tsc_schedule_end)
			!= tsc_schd_end_offset)
	{
		printf("Sinfo: Invalid 'tsc_schedule_end' offset %d /= %d\n",
				tsc_schd_end_offset,
				offsetof(struct subject_info_type, tsc_schedule_end));
		return 0;
	}

	if (offsetof(struct subject_info_type, resources) != resources_offset)
	{
		printf("Sinfo: Invalid 'resources' offset %d /= %d\n", resources_offset,
				offsetof(struct subject_info_type, resources));
		return 0;
	}

	if (offsetof(struct subject_info_type, memregions) != memregions_offset)
	{
		printf("Sinfo: Invalid 'memregions' offset %d /= %d\n",
				memregions_offset,
				offsetof(struct subject_info_type, memregions));
		return 0;
	}

	if (offsetof(struct subject_info_type, channels_info) != chan_info_offset)
	{
		printf("Sinfo: Invalid 'channels_info' offset %d /= %d\n",
				chan_info_offset,
				offsetof(struct subject_info_type, channels_info));
		return 0;
	}

	if (offsetof(struct subject_info_type, dev_info) != dev_info_offset)
	{
		printf("Sinfo: Invalid 'dev_info' offset %d /= %d\n",
				dev_info_offset,
				offsetof(struct subject_info_type, dev_info));
		return 0;
	}

	return 1;
}
