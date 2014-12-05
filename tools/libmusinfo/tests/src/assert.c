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

int assert_channel_info(const struct channel_info_type * const channel_info)
{
	if (!(channel_info->flags & HAS_EVENT_FLAG))
	{
		printf("Channel: Has_Event flag not set\n");
		return 0;
	}
	if (!(channel_info->flags & HAS_VECTOR_FLAG))
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
		const int magic_offset, const int chan_count_offset,
		const int tsc_khz_offset, const int chan_info_offset)
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

	if (offsetof(struct subject_info_type, channel_info_count)
			!= chan_count_offset)
	{
		printf("Sinfo: Invalid 'channel_info_count' offset %d /= %d\n",
				chan_count_offset,
				offsetof(struct subject_info_type, channel_info_count));
		return 0;
	}

	if (offsetof(struct subject_info_type, tsc_khz) != tsc_khz_offset)
	{
		printf("Sinfo: Invalid 'tsc_khz' offset %d /= %d\n", tsc_khz_offset,
				offsetof(struct subject_info_type, tsc_khz));
		return 0;
	}

	if (offsetof(struct subject_info_type, channels_info) != chan_info_offset)
	{
		printf("Sinfo: Invalid 'channels_info' offset %d /= %d\n",
				chan_info_offset,
				offsetof(struct subject_info_type, channels_info));
		return 0;
	}

	return 1;
}
