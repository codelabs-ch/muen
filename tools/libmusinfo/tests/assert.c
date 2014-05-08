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
