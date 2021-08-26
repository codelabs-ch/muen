import math


def bool_to_str(x):
    """
    Return lowercase string representation for bool types.
    """
    return str(x).lower() if isinstance(x, bool) else x


def ada_hex_to_int(value):
    """
    Convert given Ada hex string to numeric value:
    16#0123_0000# -> 0x1230000
    """
    if value == "":
        return ""
    else:
        return int(value.replace('_', '')[3:-1], 16)


def int_to_ada_hex(value):
    """
    Convert given numeric value to Ada hex string:
    0x1230000 -> 16#0123_0000#
    """
    if value == "":
        return ""
    else:
        val = hex(value).rstrip("L").lstrip("0x")
        val = val.rjust(int(math.ceil(len(val) / 4.0) * 4), '0')
        hexvalue = "16#"
        for index in range(0, len(val)):
            if ((index % 4) == 0) and (index != 0):
                hexvalue += "_"

            hexvalue += val[index]

        return hexvalue + "#"
