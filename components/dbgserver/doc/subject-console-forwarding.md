# Subject Console Forwarding
The subject console forwarding mechanism is for attaching and detaching to
serial consoles via the Debug Server without the need for an elaborate setup,
e.g. network connection to a Linux subject and then forwarding the data from
there. The forwarded console is a bi-directional, character based device that
operates like a serial console. Communication is performed via two shared
memory stream channels (`in` and `out`) between a subject and the Debug
Server's interactive console. The input channel, which transports data from the
Debug Server to the subject, is event-driven so the subject gets notified when
new data is available.

The Debug Server provides the `lf` command to list the available forwarded
subject consoles and the `ac` command to attach to a specific console given by
ID. Detaching from a console can be achieved by pressing the `<ESC>` key twice
in a row.

The main use case is to provide access to hypervisor consoles (`hvc`) of Linux
subjects. The `hvc_muen` driver provides the necessary Linux driver which
creates the `/dev/hvc*` devices on which a getty can be run to provide a
terminal.

## Configuration
### Policy
On the policy level, a physical channel for each communication direction needs
to be declared. Additionally, the channel from Debug Server to the subject the
console is forwarded to, should have an event with mode `asap` to facilitate
timely notification on new input data.

```XML
<channel name="console_in_1"  size="16#1000#" hasEvent="asap"/>
<channel name="console_out_1" size="16#1000#"/>
```

In this example, `in` and `out` designate the direction of the console from the
point of view of the subject, e.g. `console_in` is used to transmit input data
from the Debug Server to the subject while `console_out` carries output data
from the subject to the Debug Server.

### Debug Server
To enable an unconstrained number of forwarded subject consoles, the channel
mappings specified on the subject-level must conform to the following naming
scheme: the logical channel names for input (going from Debug Server to
subject) must start with `subject_console_in` while those for output must have
the prefix `subject_console_out`:

```XML
<component ref="dbgserver">
 ...
 <map logical="subject_console_in_1" physical="console_in_1"/>
 <map logical="subject_console_out_1" physical="console_out_1"/>
 ...
</component>
```

This enables the generation of the necessary component XML specification with
the requested number of subject console channel mappings. An XSLT script
examines the subject resource mappings and determines the number of channels
associated with forwarded consoles.

Note: **make sure that corresponding input and output channels are mapped to
the same ID in the respective reader and writer channel arrays.** This can be
done by inspecting the generated Debug Server component spec.

### Linux hvc_muen driver
The configuration of the Linux subjects consists of two parts: the logical
channel mappings and the boot parameters for the `hvc_muen` kernel driver.

#### Console Channel mappings
Mappings for the console output and input channels must be created for the
Linux subjects in question:

```XML
<channels>
 ...
 <reader logical="hvc1_input" physical="console_in_1" vector="auto"/>
 <writer logical="hvc1_output" physical="console_out_1"/>
 ...
</channels>
```

The channel reader can optionally specify a vector to have an IRQ-driven
console instead of resorting to polling.

Logical names of the console channels must be passed to the `hvc_muen` driver
so it knows which channels to use for communication. This is done using the in
and out parameters:

```XML
<bootparams>hvc_muen.out=virtual_console,hvc1_output hvc_muen.in=,hvc1_input</bootparams>
```

As it supports up to 8 consoles simultaneously, the `in` and `out` parameters
are comma-separated lists where corresponding indices form the bi-directional
character device. To declare a unidirectional hvc device empty channel names
can be given by simply providing a comma. In the above example,
`virtual_console` is an output-only channel.

## Linux hvc_muen driver
The `hvc_muen` driver implements a *Linux hypervisor console (hvc) driver* as
part of the tty subsystem. When the driver is loaded, it looks for channels in
Sinfo and instantiates the corresponding hvc devices. The currently supported
maximum number of hvc devices is 8.
Note that the driver also supports unidirectional communication for the case
when only an outbound channel is present. The driver is configured via the
`out` and `in` module parameters. They specify a comma-separated list of output
and input channels. Corresponding indices in the arrays form a hvc device.

To compile the driver, enable the `HVC_MUEN` option in the Linux config, e.g.
using `make menuconfig`.

```
-> Device Drivers
  -> Character devices
    -> Enable TTY (TTY [=y])
      -> Muen virtual console support (HVC_MUEN)
```

The source code of the driver is located in the Linux kernel tree at
`drivers/tty/hvc/hvc_muen.c`.

## Implementation
The main concept of the implementation is to have different processing modes
per console. By default, the interactive console is in `Processing` mode, which
means the Debug Server will process any input coming from the interactive
console and interpret it as a command.

Attaching to a subject console will result in a transition to the `Forwarding`
mode.  Hitting the first key of the detach key combo will put it in the
`Buffering` since the pressed key will not be immediately forwarded. If the
next key matches again, the console is detached and a transition back to the
`Processing` mode is performed. If on the other hand, the key did not match,
the console goes back to `Processing` and the full key sequence is forwarded
to the subject.

```
                         +------------+
                         | Processing |
                         +------------+
                         /             ^
               Attach   /               \  Detach/Match
                       v                 \
             +------------+    Match    +-----------+
             | Forwarding |------------>| Buffering |
             +------------+             +-----------+
                   ^                          |
                   |          Mismatch        |
                   +--------------------------+
```

Issuing the `lf` command will print out a list of the available subject
consoles:

```
$ lf
|        ID | Console
|-----------+--------------------
|         1 | nic_linux
|         2 | storage_linux
```

The `ac` command followed by a number attaches to the requested subject
console. Any input following this command will be forwarded to the associated
subject:

```
$ ac1
Welcome to Linux on Muen SK
lnx1 login:
```

Pressing the detach key combination will bring the user back to the interactive
Debug Server console. The detach key combination is specified as the constant
`Dbg.Subject_Consoles.Detach_Keys`:

```Ada
--  Key combination to detach console.
Detach_Keys : constant Keycombo_Array (1 .. 2)
  := (1 => ASCII.ESC,
      2 => ASCII.ESC);
```

## Usage with QEMU
To use subject console forwarding in combination with QEMU, enable it in the
the demo system policy:

```XML
<config>
 ...
 <boolean name="dbgserver_console_forwarding" value="true"/>
 ...
</config>
```

By default, QEMU redirects the serial output to a file. To be able to interact
with the Debug Server, instruct QEMU to use the pty backend for the emulated
serial device by adjusting the `emulate/Makefile` as follows:

```
QEMU_OPTS += \
	-drive file=$(ISOFILE),index=0,media=disk,format=raw \
	-serial pty \
	...
```

With these changes in place, build the demo system using `make emulate`. A
warning that the system could not be started will appear on the console. This
is expected as the serial output is now being redirected to a pseudo-terminal.

To determine which pty to use in order to connect to the serial device, issue
the following command:

```
$ grep "char device redirected" emulate/emulate.out
char device redirected to /dev/pts/6 (label serial0)
```

In this case the serial device is being redirected to `/dev/pts/6`. Connecting
to it can be done e.g. using `screen` by passing the device as a parameter
`screen /dev/pts/6`.
If all works as expected, we are now connected with the interactive console of
the Debug Server where we can use the `lf` and `ac<num>` commands to use the
subject console forwarding feature.
