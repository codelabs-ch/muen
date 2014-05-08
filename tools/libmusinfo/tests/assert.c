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

#include "musinfo.h"

#define WRITABLE_FLAG	1 << 0
#define HAS_EVENT_FLAG	1 << 1
#define HAS_VECTOR_FLAG	1 << 2

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

int assert_channel(const struct channel_type * const channel)
{
	if (!assert_name(&channel->name))
	{
		return 0;
	}

	if (channel->address != 0xdeadbeefcafefeed)
	{
		printf("Channel: Invalid address 0x%lx\n", channel->address);
		return 0;
	}

	if (channel->size != 0x8080ababcdcd9090)
	{
		printf("Channel: Invalid size 0x%lx\n", channel->size);
		return 0;
	}

	if (!(channel->flags & WRITABLE_FLAG))
	{
		printf("Channel: Writable flag not set\n");
		return 0;
	}
	if (!(channel->flags & HAS_EVENT_FLAG))
	{
		printf("Channel: Has_Event flag not set\n");
		return 0;
	}
	if (!(channel->flags & HAS_VECTOR_FLAG))
	{
		printf("Channel: Has_Vector flag not set\n");
		return 0;
	}

	if (channel->event != 128)
	{
		printf("Channel: Invalid event number %d\n", channel->event);
		return 0;
	}

	if (channel->vector != 255)
	{
		printf("Channel: Invalid vector number %d\n", channel->vector);
		return 0;
	}

	return 1;
}
