# Debug Server
The Debug Server component (`dbgserver`) implements a multi-client log server.
Clients send log data via shared memory channels. Incoming messages are read from
the log channels, buffered and then written to the configured log sinks, e.g.
serial console. Multiple log sinks can be enabled simultaneously. Messages will be
output to all active log sinks.

Native Ada/SPARK components can use the `libmudbglog` library to conveniently send
log messages to the debug server. Linux and other VM subjects can use the serial
console `ttyS0` emulated by an associated subject monitor (SM) to achieve the
same.

The Debug Server also implements an interactive console for issuing commands to
the component to control its behavior during runtime, e.g. querying the status of
the Debug Server or disabling log output from a specific client.

## Configuration
Various aspects of the Debug Server component can be configured via the component
XML specification as well as the global config in the system policy.

### Global Config Variables
The following table lists the global config variables and their meaning.

| Name                     | Meaning                                                                                |
| ------------------------ | -------------------------------------------------------------------------------------- |
| `logchannel_size`        | Size of log channels in bytes                                                          |
| `dbgserver_sink_pcspkr`  | Specifies whether the PC Speaker log sink is enabled                                   |
| `dbgserver_sink_serial`  | Specifies whether the serial/UART log sink is enabled                                  |
| `dbgserver_sink_shmem`   | Specifies whether the Shared Memory Stream log sink is enabled                         |
| `dbgserver_sink_xhcidbg` | Specifies whether the xHCI Debug Capability log sink is enabled                        |
| `hsuart_enabled`         | Specifies whether High Speed MMIO UART log sink is enabled. [^1]                       |
| `hsuart_supported`       | Specifies whether High Speed MMIO UART log sink is supported by the hardware platform. |
[^1] Depends on `dbgserver_sink_serial` and `hsuart_supported`.

### Component Config Variables
The following table lists the component local config variables and their
meaning.

| Name                            | Meaning                                                                                                                                               |
| ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| `default_channel_enabled_state` | Controls whether log channels are enabled by default. Setting this to `False` silences log messages from all log channels.                            |
| `enabled_channels_override`     | CSV list of channel names that are enabled irrespective of the default. It is used to override `default_channel_enabled_state` for specific channels. |

These configuration settings enable the restriction of what log messages are
output to the log sinks from the Debug Server's start of execution.  This can
be helpful during development of a Muen system, where only the log messages of
a particular set of clients is of interest.  Note that disabled log channels
can later be enabled during runtime using the *interactive console*.

## Component XML Specification
The log channels, which are written by clients and read by the Debug Server,
are mapped to a channel array in the component XML spec. It has the logical
name `log_channels`.

As the number of clients can vary between different systems policies, an XSLT
Script automatically generates the XML snippet for the log channels prior to
the generation of the source code spec files. The script enumerates the log
channel mappings of the Debug Server subject in the source policy, and creates
a corresponding channel array entry. Only subject mappings whose logical name
starts with `log_` are considered. It is recommended to use `log_channel`
followed by a suffix (e.g. number).

## Log channel data format
Clients use the SHMStreamv2 protocol over shared memory channels to send log
data to the Debug Server. The format of the messages is specified as follows
(see the `Debuglog.Types` package):

```Ada
   subtype Message_Index is Positive range 1 .. 56;
   subtype Message_Type is String (Message_Index);

   type Data_Type is record
      Timestamp : Interfaces.Unsigned_64;
      Message   : Message_Type;
   end record
     with Size => 8 * 64;

   for Data_Type use record
      Timestamp at 0 range 0 ..  8 * 8 - 1;
      Message   at 8 range 0 .. 56 * 8 - 1;
   end record;

   Null_Message : constant Message_Type
     := Message_Type'(Message_Index => ASCII.NUL);
   Null_Data    : constant Data_Type := Data_Type'
     (Timestamp => 0,
      Message   => Null_Message);
```

## Output Format
The log output is prefixed by the log channel number in hexadecimal format,
e.g. `16#0007#`. A special, single character comes after the numeric index
value. Currently, the following symbols are defined:

| Symbol | Meaning                       |
| ------ | ----------------------------- |
| %      | New Epoch in log channel      |
| #      | Overrun of log channel        |
| >      | Continuation of previous line |
| \|     | Regular log line              |

When the Debug Server has no pending log message to process, it appends an
*idle mark* line to the log output:

```
---
```

These lines are an indicator of the current log buffer state and are not part
of the output produced by any client. They can be ignored.

## Log Sinks
The Debug Server buffers the log messages sent by all client subjects and
writes them to a so called *log sinks*. These are hardware devices (except for
the Shared Memory sink) that facilitate the communication of the Debug Server
with an entity outside of the Muen system running on the target hardware, e.g.
developer host connected via serial cable.

Log sinks are independently handled by the Debug Server and processed
sequentially in a round-robin order. Multiple sinks can be enabled at the same
time. They are configured at integration time of the component via the
corresponding config values. Only enabled log sinks are compiled into the Debug
Server component.

### Serial
#### UART
This log sink uses a legacy UART (universal asynchronous receiver-transmitter)
for communication via a serial port using port I/O. The log data is transferred
to a host connected via a plain old serial cable. You can run any serial
console program such as [`screen`](https://www.gnu.org/software/screen/) or
[`minicom`](https://salsa.debian.org/minicom-team/minicom) on the host to
capture the Debug Server output:

```shell
$ screen /dev/ttyUSB0 115200
```

Alternatively, on hardware that supports Intel vPro with AMT, Serial-Over-LAN
(SoL) transmits the data via a network connection. To capture the logs and
communicate with the Debug Server on the Muen target system, you can use the
[`amtterm`](https://www.kraxel.org/blog/linux/amtterm/) tool:

```shell
$ amtterm $TARGET_IP
```

#### HSUART
A high-speed (HS) UART uses memory mapped I/O instead of port I/O for
programming the serial device registers. Register width is 32-bits. Same as
with the regular UART log sink which uses port I/O, a host must be connected to
the other end of the serial cable. Capturing the serial output in the host
works the same as in the case of the non-high-speed UART.

### xHCI Debug Capability
This log sink uses the [xHCI debug
capability](https://www-ssl.intel.com/content/www/us/en/io/universal-serial-bus/extensible-host-controler-interface-usb-xhci.html)
for transferring the log data to a host connected via an USB data cable. As it
performs data transfer in 4 K chunks, it operates at much higher speed than the
regular UART log sink. To implement this functionality it uses the `libxhcidbg`
library, which provides additional
[documentation](https://git.codelabs.ch/?p=libxhcidbg.git) on how to setup and
configure a Linux host to capture the output.

### Shared Memory
The Debug Server can also write the log output to a shared memory channel which
enables further processing by a separate component of the target system. An
example of such a use case is mapping of the shared memory log sink into the
Linux subject in the Muen Desktop system. This allows access to the logs in
Linux, where it can be stored in a file or sent to a remote syslog server. The
`mureadshmemlog` utility described in the Tools section can be used for this
purpose.

### PC Speaker
This log sink utilizes the PC speaker for output using frequency modulation. It
is low bandwidth and intended as a *last resort* when no other log sink can be
used on a given hardware platform.

Even- and odd-numbered bits are modulated using distinct frequencies to enable
synchronization; moreover, after each byte a byte separator frequency is
output. It is recommended to use an audio cable to eliminate noise
disturbances. The output of this sink is intended to be decoded using the
`mupcspkrdbg` utility described in the Tools section.

## Interactive Console
Some log sinks (i.e. UART as well as xHCI DbC) enable bi-directional
communication with the Debug Server component running on the target system.
This interactive console can be used to send commands to the Debug Server,
which processes them at runtime. All supported commands are listed below.

### Commands
| Command    | Description               |
| ---------- | ------------------------- |
| h          | Print help message        |
| `<ESC>`    | Clear line                |
| rb         | System Reboot             |
| sd         | System Shutdown           |
| st         | Print debug server status |
| lc         | List channels             |
| le         | List events               |
| ls         | List subjects             |
| sr         | Reset log streams         |
| la         | Enable all log channels   |
| ln         | Disable all log channels  |
| lt `<num>` | Toggle logging of channel |
| te `<num>` | Trigger event             |

## Tools
There are various tools and scripts which aide in processing the log messages
captured from the Debug Server. These can be run on the host system, which is
used to capture the output from (one of) the connected log sinks.

### Log Postprocessing
The `mulog.py`script has the following usage:

```shell
$ mulog.py <filename>
```

It can be used to process a file containing the Debug Server output captured
via one of the log sinks, e.g. serial UART or xHCI DbC. The script strips
delimiter and reassembles continuation lines while reordering and grouping the
messages by client/channel ID.

### Log Filtering
The `mulog-filter.py` script has the following usage:

```shell
$ mulog-filter.py <channel id> [filename]
```

It can be used to filter a given log for the messages with a specific log
client/channel ID. The file specified by filename is processed and only
messages from the specified ID are printed to standard output. The prefix is
stripped from the output and continuation lines a reassembled.
If no filename argument is given, then the script expects the log output from
standard input.

The following command connects to the host `TARGET` using AMT Serial-over-LAN.
Captured output is then piped to the `tee` command which writes the entire log
to the file `debug.log`while simultaneously printing the messages to standard
out. This output is then further piped to `mulog-filter.py` which was given the
parameter `0x2`. Thus, the script filters the log for messages from the log
client with ID `16#0002#`.

```shell
$ amtterm $TARGET_IP | tee debug.log | mulog-filter.py 0x2
```

### Shared Memory Log Reading
The `mureadshmemlog` tool can be used to read the log contents of a file
written by the shared memory log sink. In the case of the Muen Demo system, the
channel written by the Debug Server is mapped into one of the Linux subjects
and exposed to userspace via the
[`muenfs`](https://git.codelabs.ch/?p=muen/linux/muenfs.git) driver.
With the `muenfs` module loaded and mounted under `/muenfs`, the shared memory
log sink channel is accessible and the log data can be written to a file
using the following command:

```shell
$ mureadshmemlog /muenfs/debug_shm_sink_memory /var/log/muen.log
```

#### Systemd Service
In the case of the Muen Desktop system, the `mureadshmemlog` utility can be
used to create a [systemd](https://systemd.io/) service so the process of
capturing the log output to a file is automated and setup during boot. The tool
is expected to be installed under `/usr/local/bin`.

1. Load the `muenfs` module during boot by creating the file
   `/etc/modules-load.d/muenfs.conf`:

```
muenfs
```

2. Automatically mount /muenfs during boot by creating the file
   `/etc/systemd/system/muenfs.mount`:

```
[Unit]
Description=Muenfs
Requires=systemd-modules-load.service
After=systemd-modules-load.service
[Mount]
What=muenfs
Where=/muenfs
Type=muenfs
[Install]
WantedBy=local-fs.target
```

4. Automatically start capturing the log output to `/var/log/muen.log` by
   creating the file `/etc/systemd/system/muendbg.service`:

```
[Unit]
Description=Muen SHMmem debug capture
After=local-fs.target
RequiresMountsFor=/muenfs
[Service]
ExecStart=/usr/local/bin/mureadshmemlog /muenfs/debug_shm_sink_memory /var/log/muen.log
Restart=on-failure
[Install]
WantedBy=multi-user.target
```

With these files in place, systemd automatically starts the muendbg.service and
its dependecies during boot. You can check the status with the following
command:

```shell
$ sudo systemctl status muendbg.service
● muendbg.service - Muen SHMmem debug capture
     Loaded: loaded (/etc/systemd/system/muendbg.service; enabled; preset: enabled)
     Active: active (running) since Tue 2024-03-19 13:04:23 CET; 1min 46s ago
   Main PID: 891 (mureadshmemlog)
      Tasks: 1 (limit: 11593)
     Memory: 3.6M
        CPU: 6ms
     CGroup: /system.slice/muendbg.service
             └─891 /usr/local/bin/mureadshmemlog /muenfs/debug_shm_sink_memory /var/log/muen.log

Mar 19 13:04:23 host systemd[1]: Started muendbg.service - Muen SHMmem debug capture.
Mar 19 13:04:23 host mureadshmemlog[891]: Mar 19 2024 13:04:23 mureadshmemlog: Processing input file '/muenfs/debug_shm_sink_memory'
Mar 19 13:04:23 host mureadshmemlog[891]: Mar 19 2024 13:04:23 mureadshmemlog: Files successfuly opened, watch '/var/log/muen.log' for data
```

### PC Speaker Decoding
The PC Speaker log sink uses frequency modulation to transport log messages via
the audio channel. To extract the information, the sound must be recorded and
demodulated using the `mupcspkrdbg` tool. It accepts a stream of frequencies as
provided by the tool [`aubiopitch`](https://aubio.org), demodulates them, and
finally outputs the decoded debug log.

The following listing gives an usage example of how to combine `aubiopitch`,
[`arecord`](https://github.com/alsa-project/alsa-utils) and `mupcspkrdbg` to
recover the log messages:

```shell
$ aubiopitch -s -40 -H 128 <(arecord -f cd) | ./mupcspkrdbg
```

## Crash Audit
The Debug Server maps the Crash Audit memory region. During component startup,
the region is checked for the presence of an active crash audit record. If such
a crash dump is detected, all crash information is logged.

The below listing illustrates how such a Crash Audit log dump looks like:

```
DBG-LOG>
[Active CRASH AUDIT detected @ 16#0000000100000000#]
Records        : 16#01#
Boot Count     : 16#02#
Crash Count    : 16#01#
Kernel Version : v1.1.0

* Record 16#01#, APIC ID 16#04# @ TSC 16#00000070bf7db363# - Reason : 16#0000000000002000#
Subject 16#0010#
Exit reason: 16#00000012#, Exit qualification: 16#00000000#
RIP: 16#ffffffff8130d319# CS : 16#0000000000000010#
RSP: 16#ffffc900000e4fb8# SS : 16#0000000000000018#
RAX: 16#000000000000001e# RBX: 16#ffffc90000287e20# RCX: 16#0000000000000000#
RDX: 16#0000000000000000# RSI: 16#0000000000000000# RDI: 16#000000000000001e#
RBP: 16#ffffc90000287e20# R08: 16#0000000000000001# R09: 16#0000000000000001#
R10: 16#0000000000000000# R11: 16#ffffc900000e4ff8# R12: 16#0000000000000000#
R13: 16#0000000000000000# R14: 16#0000000000000000# R15: 16#0000000000000000#
CR0: 16#0000000080050033# CR2: 16#0000562013dad2c4# CR3: 16#0000000007120000#
CR4: 16#00000000001526e0# EFL: 16#0000000000000046#
CS  : 16#0000000000000010#:16#0000000000000000#:16#ffffffff#:16#0000a09b#
SS  : 16#0000000000000018#:16#0000000000000000#:16#ffffffff#:16#0000c093#
DS  : 16#0000000000000000#:16#0000000000000000#:16#ffffffff#:16#0001c000#
ES  : 16#0000000000000000#:16#0000000000000000#:16#ffffffff#:16#0001c000#
FS  : 16#0000000000000000#:16#0000000000000000#:16#ffffffff#:16#0001c000#
GS  : 16#0000000000000000#:16#ffff88801f280000#:16#ffffffff#:16#0001c000#
TR  : 16#0000000000000040#:16#fffffe000003e000#:16#00004087#:16#0000008b#
LDTR: 16#0000000000000000#:16#0000000000000000#:16#ffffffff#:16#0001c000#
MXCSR: 16#00001f80# MXCSR mask: 16#0000ffff#
FCW  : 16#037f# FSW: 16#0000# FTW: 16#00# FOP: 16#0000#
FIP  : 16#0000000000000000#   FDP: 16#0000000000000000#
```
